## FIXME Class this on DepMapExperiment



#' Filter problematic cell lines
#'
#' @name filterProblematic
#' @note Updated 2023-08-22.
#'
#' @inheritParams AcidRoxygen::params
#'
#' @return Modified object, with problematic cell lines removed.
#'
#' @examples
#' data(crispr)
#'
#' ## DepMapExperiment ====
#' object <- crispr
#' object <- filterProblematic(object)
#' print(object)
NULL



## Updated 2023-08-22.
`filterProblematic,DepMapGeneEffect` <- # nolint
    function(object) {
        assert(
            validObject(object),
            isSubset(
                x = "cellosaurus",
                y = colnames(colData(object))
            ),
            isSubset(
                x = "isProblematic",
                y = colnames(colData(object)[["cellosaurus"]]))

        )
        keep <- !colData(object)[["cellosaurus"]][["isProblematic"]]
        object <- object[keep, , drop = FALSE]
        object
    }



#' @rdname filterProblematic
#' @export
setMethod(
    f = "filterProblematic",
    signature = signature(object = "DepMapGeneEffect"),
    definition = `filterProblematic,DepMapGeneEffect`
)
