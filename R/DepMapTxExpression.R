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
        profilesUrl <- files[["OmicsProfiles.csv"]]
        assert(
            isAURL(assayUrl),
            isAURL(profilesUrl)
        )
        # Benchmarks on AWS EC2 r6a instance:
        # - base: > 5 minutes
        # - data.table: munges the file
        # - readr: 2 minutes, 40 seconds
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
        ## 23q2 release is processed against Ensembl 104. However, we're
        ## intentionally using a rolling release approach here to simply remove
        ## dead transcripts from analysis.
        rowRanges <- makeGRangesFromEnsembl(
            organism = "Homo sapiens",
            level = "transcripts",
            genomeBuild = "GRCh38",
            ignoreVersion = TRUE
        )
        excludedTxs <- sort(setdiff(rownames(assay), names(rowRanges)))
        txs <- sort(intersect(rownames(assay), names(rowRanges)))
        assay <- assay[txs, , drop = FALSE]
        rowRanges <- rowRanges[txs]
        ## Columns (cells) -----------------------------------------------------
        ## Using "mi" for model info.
        mi <- .importBroadModelInfo(dataset = dataset)
        ## Using "op" for omics profiles.
        ## This contains multiple data types: rna, SNParray, wes, wgs.
        op <- .importBroadDataFile(url = profilesUrl)
        colnames(op) <- camelCase(colnames(op))
        assert(isSubset(c("modelId", "profileId"), colnames(op)))
        rownames(op) <- makeNames(op[["profileId"]])
        assert(isSubset(colnames(assay), rownames(op)))
        op <- op[colnames(assay), , drop = FALSE]
        ## FIXME Consider removing duplicates here?
        ## > bad <- sort(op[["modelId"]][which(duplicated(op[["modelId"]]))])
        ## There are funky cells with duplicate profiles here...what's going
        ## on with that?
        ##  [1] "ACH-000029" "ACH-000095" "ACH-000143" "ACH-000206" "ACH-000328"
        ##  [6] "ACH-000337" "ACH-000455" "ACH-000468" "ACH-000517" "ACH-000532"
        ## [11] "ACH-000556" "ACH-000597" "ACH-000700" "ACH-000931" "ACH-000975"
        ## [16] "ACH-001192"
        assert(
            identical(unique(op[["datatype"]]), "rna"),
            hasNoDuplicates(op[["modelId"]])
        )

        ## Subset to only contain relevant profiles that have transcript data.
        cells <- sort(intersect(df[["modelId"]], modelInfo[["depmapId"]]))
        ok <- df[["modelId"]] %in% cells
        excludedCells <- sort(unique(df[["modelId"]][!ok]))
        df <- df[ok, , drop = FALSE]






        cd <- leftJoin(x = cd2, y = cd1, by = "depmapId")

        colnames(modelInfo)


        metadata(out)[["excludedCells"]] <- excludedCells
        metadata(out)[["excludedTxs"]] <- excludedTxs
        out
    }
