## FIXME Just class this on DepMapExperiment instead.



#' @name humanize
#' @inherit AcidGenerics::humanize
#' @note Updated 2023-03-20.
#'
#' @inheritParams AcidRoxygen::params
#' @param ... Additional arguments.
#'
#' @examples
#' data(crispr)
#'
#' ## DepMapGeneEffect ====
#' object <- crispr
#' print(head(rownames(object)))
#' print(head(colnames(object)))
#' object <- humanize(object)
#' print(head(rownames(object)))
#' print(head(colnames(object)))
NULL



## Updated 2023-03-20.
`humanize,DepMapGeneEffect` <- # nolint
    function(object) {
        stop("FIXME")
    }



#' @rdname humanize
#' @export
setMethod(
    f = "humanize",
    signature = signature(object = "DepMapGeneEffect"),
    definition = `humanize,DepMapGeneEffect`
)
