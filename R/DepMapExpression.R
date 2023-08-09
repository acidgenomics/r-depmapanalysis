## FIXME Split this out into separate gene- and transcript-level classes.
## FIXME Canonical file is: OmicsExpressionProteinCodingGenesTPMLogp1.csv



#' Import DepMap expression data
#'
#' @export
#' @note Updated 2023-08-08.
#'
#' @return `DepMapExpression`.
#'
#' @examples
#' object <- DepMapExpression()
#' dim(object)
DepMapExpression <- # nolint
    function() {
        ## FIXME Rethink this approach.
        .makeCcleSE(
            dataset = match.arg(dataset),
            assayKey = "expression",
            assayName = "log2Tpm",
            class = "DepMapExpression"
        )
    }
