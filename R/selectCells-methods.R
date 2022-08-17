#' Select cells
#'
#' @name selectCells
#' @note Updated 2022-08-17.
#'
#' @seealso
#' - `AcidExperiment::selectSamples`.
#'
#' @examples
#' data(crispr)
#'
#' ## GeneEffect ====
#' object <- crispr
#' print(object)
#' lineage <- colData(object)[["lineage"]][[1L]]
#' x <- selectCells(object, lineage = lineage)
#' print(x)
NULL



## FIXME Use `.mapCellsToColnames` here internally.
## Allow the user to select based on colData metadata.



## Updated 2022-08-17.
`selectCells,SE` <-
    function(
        object,
        ...
    ) {
        args <- list(...)
        assert(
            validObject(object),
            all(vapply(
                X = args,
                FUN = is.atomic,
                FUN.VALUE = logical(1L)
            )),
            msg = "Arguments must be atomic."
        )
        colData <- colData(object)
        assert(isSubset(names(args), colnames(colData)))
        ## Obtain the cell identifiers.
        list <- Map(
            col = names(args),
            arg = args,
            MoreArgs = list("data" = colData),
            f = function(col, arg, data) {
                rownames(data[data[[col]] %in% arg, , drop = FALSE])
            }
        )
        cells <- sort(as.character(Reduce(f = intersect, x = list)))
        assert(hasLength(cells))
        out <- object[, cells, drop = FALSE]
        out <- droplevels2(out)
        out

    }



## Updated 2022-08-17.
`selectCells,CCLECopyNumberData` <- `selectCells,SE`

## Updated 2022-08-17.
`selectCells,CCLEExpressionData` <- `selectCells,SE`

## Updated 2022-08-17.
`selectCells,CCLEMutationData` <- `selectCells,SE`



#' @rdname selectCells
#' @export
setMethod(
    f = "selectCells",
    signature = signature(object = "CCLECopyNumberData"),
    definition = `selectCells,CCLECopyNumberData`
)

#' @rdname selectCells
#' @export
setMethod(
    f = "selectCells",
    signature = signature(object = "CCLEExpressionData"),
    definition = `selectCells,CCLEExpressionData`
)

#' @rdname selectCells
#' @export
setMethod(
    f = "selectCells",
    signature = signature(object = "CCLEMutationData"),
    definition = `selectCells,CCLEMutationData`
)
