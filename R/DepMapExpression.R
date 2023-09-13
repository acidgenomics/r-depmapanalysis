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
        ## FIXME This step seems to be hanging in RStudio...need to debug.
        assay <- .importBroadDataFile(
            url = assayUrl,
            rownameCol = 1L,
            return = "matrix"
        )
        assay <- t(assay)
        colData <- .importBroadDataFile(url = colDataUrl)
        .makeBroadSingleAssaySE(
            file = "OmicsExpressionTranscriptsTPMLogp1Profile.csv",
            assayName = "log2Tpm",
            class = "DepMapTxExpression"
        )
    }
