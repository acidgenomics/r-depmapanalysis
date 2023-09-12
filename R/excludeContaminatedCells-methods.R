#' @name excludeContaminatedCells
#' @inherit AcidGenerics::excludeContaminatedCells
#' @note Updated 2023-09-12.
#'
#' @inheritParams AcidRoxygen::params
#'
#' @examples
#' data(crispr)
#'
#' ## DepMapExperiment ====
#' object <- crispr
#' object <- excludeContaminatedCells(object)
#' print(object)
NULL



## Updated 2023-09-12.
`excludeContaminatedCells,DepMapExperiment` <- # nolint
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



#' @rdname excludeContaminatedCells
#' @export
setMethod(
    f = "excludeContaminatedCells",
    signature = signature(object = "DepMapExperiment"),
    definition = `excludeContaminatedCells,DepMapExperiment`
)
