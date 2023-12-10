## FIXME Inform the user about which release we're using, and the corresponding
## file name.



#' Import DepMap gene expression data
#'
#' @export
#' @note Updated 2023-09-14.
#'
#' @return `DepMapGeneExpression`.
#'
#' @examples
#' ## Gene level.
#' object <- DepMapGeneExpression()
#' print(object)
DepMapGeneExpression <- # nolint
    function() {
        .makeBroadSingleAssaySE(
            file = "OmicsExpressionProteinCodingGenesTPMLogp1.csv",
            assayName = "log2Tpm",
            class = "DepMapGeneExpression"
        )
    }
