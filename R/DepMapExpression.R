#' Import DepMap gene or transcript expression data
#'
#' @name DepMapExpression
#' @note Updated 2023-09-13.
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



## FIXME This step takes too long in RStudio...consider informing the user.
## FIXME Need to handle transcript-to-gene mapping here.
## FIXME This file is large and can crash RStudio on a MacBook.
## FIXME This uses that funky Profiles.csv file for identifiers, need to
## rethink the mapping.
## Need to transpose the assay to get cells into columns.
## FIXME Transcripts currently map to Ensembl/GENCODE, so use AnnotationHub
## for that.
## FIXME Need to import "OmicsProfiles.csv" here.
## FIXME Need to import "OmicsExpressionTranscriptsTPMLogp1Profile.csv" here.

#' @rdname DepMapExpression
#' @export
DepMapTxExpression <- # nolint
    function() {
        dataset <- .currentBroadDataset
        files <- datasets[[dataset]][["files"]]
        assert(
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
        assay <- .importBroadDataFile(
            url = assayUrl,
            rownameCol = 1L,
            return = "matrix"
        )
        assay <- t(assay)
        rn <- rownames(assay)
        assert(allAreMatchingFixed(x = rn, pattern = "ENST"))
        rn <- gsub(
            pattern = ".*(ENST[0-9]{11})$",
            replacement = "\\1",
            x = rn

        )
        rownames(assay) <- rn
        ncol <- length(x[[1L]])
        x <- unlist(x, recursive = FALSE, use.names = FALSE)
        x <- matrix(data = x, ncol = ncol, byrow = TRUE)

        matrix(as.numeric(unlist(strsplit(vv, " - "))), ncol = 2, byrow = TRUE)

        rn <- do.call(
            what = rbind,
            args =
        )

        ##

        ## Rows:
        ## [1] "TSPAN6_ENST00000373020" "TSPAN6_ENST00000494424" "TSPAN6_ENST00000496771"
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
