## Benchmarks for "OmicsExpressionTranscriptsTPMLogp1Profile.csv" import
## on AWS EC2 r6a instance:
## - base: > 5 minutes
## - data.table: munges the file
## - readr: 2 minutes, 40 seconds

## 23q2 release is processed against Ensembl 104. However, we're
## intentionally using a rolling release approach here to simply remove
## dead transcripts from analysis.



#' Import DepMap transcript expression data
#'
#' @export
#' @note Updated 2023-09-14.
#'
#' @return `DepMapTxExpression`.
#'
#' @examples
#' ## This is CPU intensive.
#' ## > object <- DepMapTxExpression()
#' ## > print(object)
DepMapTxExpression <- # nolint
    function() {
        dataset <- .currentBroadDataset
        files <- datasets[[dataset]][["files"]]
        assert(
            requireNamespaces("readr"),
            isString(dataset),
            is.list(files),
            hasRAM(n = 14L)
        )
        assayUrl <- files[["OmicsExpressionTranscriptsTPMLogp1Profile.csv"]]
        profilesUrl <- files[["OmicsDefaultModelProfiles.csv"]]
        assert(
            isAURL(assayUrl),
            isAURL(profilesUrl)
        )
        ## This step is CPU intensive.
        assay <- .importBroadDataFile(
            url = assayUrl,
            rownameCol = 1L,
            return = "matrix",
            engine = "readr"
        )
        assay <- t(assay)
        ## Rows (transcripts) --------------------------------------------------
        rn <- rownames(assay)
        assert(allAreMatchingFixed(x = rn, pattern = "ENST"))
        rn <- gsub(
            pattern = ".*(ENST[0-9]{11})$",
            replacement = "\\1",
            x = rn
        )
        assert(allAreMatchingRegex(x = rn, pattern = "^ENST[0-9]{11}$"))
        rownames(assay) <- rn
        rr <- makeGRangesFromEnsembl(
            organism = "Homo sapiens",
            level = "transcripts",
            genomeBuild = "GRCh38",
            ignoreVersion = TRUE
        )
        excludedTxs <- sort(setdiff(rownames(assay), names(rr)))
        txs <- sort(intersect(rownames(assay), names(rr)))
        assay <- assay[txs, , drop = FALSE]
        rr <- rr[txs]
        ## Columns (cells) -----------------------------------------------------
        ## Using "mi" for model info.
        mi <- .importBroadModelInfo(dataset = dataset)
        ## Using "op" for omics profiles.
        ## This contains multiple data types: rna, SNParray, wes, wgs.
        op <- .importBroadDataFile(url = profilesUrl)
        colnames(op) <- camelCase(colnames(op))
        assert(isSubset(
            x = c("modelId", "profileId", "profileType"),
            y = colnames(op)
        ))
        keep <- op[["profileType"]] == "rna"
        op <- op[keep, , drop = FALSE]
        op[["profileType"]] <- NULL
        assert(
            hasNoDuplicates(op[["profileId"]]),
            hasNoDuplicates(op[["modelId"]])
        )
        rownames(op) <- makeNames(op[["profileId"]])
        ## Subset to only contain relevant profiles that have transcript data.
        cells <- sort(intersect(
            x = op[["modelId"]],
            y = decode(mi[["cellosaurus"]][["depmapId"]])
        ))
        keep <- op[["modelId"]] %in% cells
        excludedCells <- sort(unique(op[["modelId"]][!keep]))
        op <- op[keep, , drop = FALSE]
        mi[["modelId"]] <- decode(mi[["cellosaurus"]][["depmapId"]])
        cd <- leftJoin(x = op, y = mi, by = "modelId")
        assert(isSubset(rownames(cd), colnames(assay)))
        assay <- assay[, rownames(cd)]
        rownames(cd) <- decode(cd[["cellosaurus"]][["accession"]])
        colnames(assay) <- rownames(cd)
        ## Prepare SummarizedExperiment ----------------------------------------
        assays <- list("log2Tpm" = assay)
        metadata <- list(
            "dataset" = dataset,
            "excludedCells" = excludedCells,
            "excludedTxs" = excludedTxs,
            "json" = datasets[[dataset]],
            "packageName" = .pkgName,
            "packageVersion" = .pkgVersion
        )
        se <- makeSummarizedExperiment(
            assays = assays,
            rowRanges = rr,
            colData = cd,
            metadata = metadata
        )
        new(se, Class = "DepMapTxExpression")
    }
