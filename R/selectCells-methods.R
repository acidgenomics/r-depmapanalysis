#' @name selectCells
#' @inherit Cellosaurus::selectCells details
#' @inherit AcidGenerics::selectCells description return title
#' @note Updated 2023-09-12.
#'
#' @inheritParams AcidRoxygen::params
#'
#' @param ...
#' Key value pairs that map to cell line metadata.
#'
#' @examples
#' data(crispr)
#'
#' ## DepMapGeneEffect ====
#' object <- crispr
#' dim(object)
#' lineage <- colData(object)[["lineage"]][[1L]]
#' print(lineage)
#' all(colData(object)[["lineage"]] == lineage)
#' object <- selectCells(object, lineage = lineage)
#' all(colData(object)[["lineage"]] == lineage)
#' dim(object)
NULL



## Updated 2023-09-12.
`selectCells,DepMapExperiment` <-
    function(object, ...) {
        assert(validObject(object))
        cello <- colData(object)[["cellosaurus"]]
        assert(
            is(cello, "Cellosaurus"),
            validObject(cello),
            msg = "Cellosaurus metadata not correctly saved in object."
        )
        cello <- selectCells(cello, ...)
        assert(is(cello, "Cellosaurus"))
        j <- rownames(cello)
        assert(isSubset(colnames(object), j))
        out <- object[, j, drop = FALSE]
        out
    }



#' @rdname selectCells
#' @export
setMethod(
    f = "selectCells",
    signature = signature(object = "DepMapExperiment"),
    definition = `selectCells,DepMapExperiment`
)
