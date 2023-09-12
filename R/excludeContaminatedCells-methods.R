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
        assert(validObject(object))
        cello <- colData(object)[["cellosaurus"]]
        cello <- excludeContaminatedCells(cello)
        i <- rownames(cello)
        assert(isSubset(i, colnames(object)))
        object <- object[, i, drop = FALSE]
        object
    }



#' @rdname excludeContaminatedCells
#' @export
setMethod(
    f = "excludeContaminatedCells",
    signature = signature(object = "DepMapExperiment"),
    definition = `excludeContaminatedCells,DepMapExperiment`
)
