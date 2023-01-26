#' Select cells
#'
#' @name selectCells
#' @note Updated 2023-01-26.
#'
#' @inheritParams AcidRoxygen::params
#'
#' @param ...
#' Key value pairs that map to cell line metadata defined in `colData`.
#'
#' @return Modified object.
#'
#' @seealso
#' - `AcidExperiment::selectSamples`.
#'
#' @examples
#' data(crispr)
#'
#' ## GeneEffect ====
#' object <- crispr
#' dim(object)
#' lineage <- colData(object)[["lineage"]][[1L]]
#' print(lineage)
#' all(colData(object)[["lineage"]] == lineage)
#' object <- selectCells(object, lineage = lineage)
#' all(colData(object)[["lineage"]] == lineage)
#' dim(object)
NULL



## Updated 2022-08-17.
`selectCells,SE` <-
    function(object, ...) {
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
        ## FIXME Only allow the user to select factor columns.
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
`selectCells,CopyNumber` <- `selectCells,SE`

## Updated 2022-08-17.
`selectCells,CCLEExpressionData` <- `selectCells,SE`

## Updated 2022-08-17.
`selectCells,GeneEffect` <- `selectCells,SE`



#' @rdname selectCells
#' @export
setMethod(
    f = "selectCells",
    signature = signature(object = "CopyNumber"),
    definition = `selectCells,CopyNumber`
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
    signature = signature(object = "GeneEffect"),
    definition = `selectCells,GeneEffect`
)
