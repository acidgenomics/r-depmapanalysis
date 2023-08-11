#' Import DepMap gene or transcript expression data
#'
#' @name DepMapExpression
#' @note Updated 2023-08-09.
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



## FIXME Need to handle transcript-to-gene mapping here.
## FIXME This file is large and can crash RStudio on a MacBook.
## FIXME Add a RAM check here, and inform user that this can take a long time.
## FIXME This uses that funky Profiles.csv file for identifiers, need to
## rethink the mapping.
## FIXME Consider adding CPU core and RAM checks into goalie.

#' @rdname DepMapExpression
#' @export
DepMapTxExpression <- # nolint
    function() {
        dataset <- .currentBroadDataset
        files <- datasets[[dataset]][["files"]]
        assert(
            isString(dataset),
            is.list(files)
        )
        assayUrl <- files[["OmicsExpressionTranscriptsTPMLogp1Profile.csv"]]
        colDataUrl <- files[["OmicsProfiles.csv"]]
        assert(
            isAURL(assayUrl),
            isAURL(colDataUrl)
        )
        ## This step is memory intensive and takes a while.
        assay <- .importBroadDataFile(
            url = assayUrl,
            rownameCol = 1L,
            return = "matrix"
        )
        assay <- t(assay)
        colData <- .importBroadDataFile(url = colDataUrl)
        ## Need to transpose the assay to get cells into columns.
        ## FIXME Transcripts currently map to Ensembl/GENCODE, so use AnnotationHub
        ## for that.
        ## FIXME Consider erroring unless user has > 16 GB of RAM.
        ## FIXME Need to import "OmicsProfiles.csv" here.
        ## FIXME Need to import "OmicsExpressionTranscriptsTPMLogp1Profile.csv" here.
        .makeBroadSingleAssaySE(
            file = "OmicsExpressionTranscriptsTPMLogp1Profile.csv",
            assayName = "log2Tpm",
            class = "DepMapTxExpression"
        )
    }
