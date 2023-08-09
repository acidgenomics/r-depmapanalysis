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
        ## FIXME Add this as new assert check to goalie. Super useful.
        assert(
            AcidBase::ram() >= 24L,
            msg = "This function requires more RAM."
        )
        dataset <- .currentBroadDataset
        ## FIXME Consider erroring unless user has > 16 GB of RAM.
        ## FIXME Need to import "OmicsProfiles.csv" here.
        ## FIXME Need to import "OmicsExpressionTranscriptsTPMLogp1Profile.csv" here.
        .makeBroadSingleAssaySE(
            file = "OmicsExpressionTranscriptsTPMLogp1Profile.csv",
            assayName = "log2Tpm",
            class = "DepMapTxExpression"
        )
    }
