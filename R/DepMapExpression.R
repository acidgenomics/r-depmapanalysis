## FIXME Split these out into separate files.



#' Import DepMap gene or transcript expression data
#'
#' @name DepMapExpression
#' @note Updated 2023-09-14.
#'
#' @return `DepMapExpression`.
#'
#' @examples
#' ## Gene level.
#' object <- DepMapGeneExpression()
#' print(object)
#'
#' ## Transcript level (CPU intensive).
#' ## > object <- DepMapTxExpression()
#' ## > print(object)
NULL



#' @rdname DepMapExpression
#' @export
DepMapGeneExpression <- # nolint
    function() {
        .makeBroadSingleAssaySE(
            file = "OmicsExpressionProteinCodingGenesTPMLogp1.csv",
            assayName = "log2Tpm",
            class = "DepMapGeneExpression"
        )
    }



#' @rdname DepMapExpression
#' @export
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
        ## This contains multiple data types: rna, wes, wgs.
        modelInfo <- .importBroadModelInfo(dataset = dataset)
        df <- .importBroadDataFile(url = profilesUrl)
        colnames(df) <- camelCase(colnames(df))
        assert(isSubset(c("modelId", "profileId"), colnames(df)))
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
