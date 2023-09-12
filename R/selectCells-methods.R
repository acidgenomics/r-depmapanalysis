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
#' print(dim(object))
#' oncotreeCode <-
#'     levels(colData(object)[["cellosaurus"]][["oncotreeCode"]])[[1L]]
#' print(oncotreeCode)
#' subset <- selectCells(object, oncotreeCode = oncotreeCode)
#' print(dim(object))
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
