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
        ## FIXME This is getting unclassed, need to rework.
        cello <- colData(object)[["cellosaurus"]]
        assert(
            is(cello, "Cellosaurus"),
            validObject(cello)
        )
        cello <- selectCells(cello, ...)
        assert(is(cello, "Cellosaurus"))
        j <- rownames(cello)
        out <- object[, j, drop = FALSE]
    }



#' @rdname selectCells
#' @export
setMethod(
    f = "selectCells",
    signature = signature(object = "DepMapExperiment"),
    definition = `selectCells,DepMapExperiment`
)
