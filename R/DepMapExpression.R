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
#' ## Transcript level.
#' object <- DepMapTxExpression()
#' print(object)
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
        colDataUrl <- files[["OmicsProfiles.csv"]]
        assert(
            isAURL(assayUrl),
            isAURL(colDataUrl)
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
        rn <- rownames(assay)
        assert(allAreMatchingFixed(x = rn, pattern = "ENST"))
        rn <- gsub(
            pattern = ".*(ENST[0-9]{11})$",
            replacement = "\\1",
            x = rn
        )
        assert(allAreMatchingRegex(x = rn, pattern = "^ENST[0-9]{11}$"))
        rownames(assay) <- rn
        ## FIXME We don't want to hard code the release version here.
        ## Alternatively, can stash the Ensembl release in the JSON metadata.
        rowRanges <- makeGRangesFromEnsembl(
            organism = "Homo sapiens",
            level = "transcripts",
            genomeBuild = "GRCh38",
            release = 104L,
            ignoreVersion = TRUE
        )
        assert(isSubset(rownames(assay), names(rowRanges)))
        ## Columns:
        ## [1] "PR_AdBjpG" "PR_I2AzwG" "PR_5ekAAC"
        colData <- .importBroadDataFile(url = colDataUrl)

        ## FIXME May want to match to a specific release version.
        rowRanges <- makeGRangesFromEnsembl(organism = "Homo sapiens")

        .makeBroadSingleAssaySE(
            file = "OmicsExpressionTranscriptsTPMLogp1Profile.csv",
            assayName = "log2Tpm",
            class = "DepMapTxExpression"
        )
    }
