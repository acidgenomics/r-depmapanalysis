## FIXME Split this out into separate gene- and transcript-level classes.
## FIXME Canonical file is: OmicsExpressionProteinCodingGenesTPMLogp1.csv



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
        stop("FIXME")
        ## FIXME Rethink this approach.
    }



#' @rdname DepMapExpression
#' @export
DepMapTxExpression <- # nolint
    function() {
        stop("FIXME")
        ## FIXME Rethink this approach.
    }
