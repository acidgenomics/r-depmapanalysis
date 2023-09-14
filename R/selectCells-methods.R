#' @name selectCells
#' @inherit Cellosaurus::selectCells details
#' @inherit AcidGenerics::selectCells description return title
#' @note Updated 2023-09-14.
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
#'     colData(object) |>
#'     _[["cellosaurus"]] |>
#'     _[["oncotreeCode"]] |>
#'     decode() |>
#'     _[[1L]]
#' print(oncotreeCode)
#' subset <- selectCells(object, oncotreeCode = oncotreeCode)
#' print(dim(object))
NULL



## Updated 2023-09-14.
`selectCells,SE` <- # nolint
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
        assert(isSubset(j, colnames(object)))
        out <- object[, j, drop = FALSE]
        out
    }



`selectCells,DepMapGeneEffect` <- # nolint
    `selectCells,SE`



#' @rdname selectCells
#' @export
setMethod(
    f = "selectCells",
    signature = signature(object = "DepMapGeneEffect"),
    definition = `selectCells,DepMapGeneEffect`
)
