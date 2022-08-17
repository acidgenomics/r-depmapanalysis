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
        assert(validObject(object))



        assert(
            validObject(object),
            all(vapply(
                X = args,
                FUN = is.atomic,
                FUN.VALUE = logical(1L)
            )),
            msg = "Arguments must be atomic."
        )
        ## Match the arguments against the sample metadata.
        sampleData <- sampleData(object)
        assert(isSubset(names(args), colnames(sampleData)))
        ## Obtain the sample identifiers.
        list <- Map(
            col = names(args),
            arg = args,
            MoreArgs = list("data" = sampleData),
            f = function(col, arg, data) {
                rownames(data[data[[col]] %in% arg, , drop = FALSE])
            }
        )
        samples <- sort(as.character(Reduce(f = intersect, x = list)))
        assert(hasLength(samples))
        ## Return.
        out <- object[, samples, drop = FALSE]
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
    signature = signature(object = "GeneEffect"),
    definition = `selectCells,CCLEExpressionData`
)

#' @rdname selectCells
#' @export
setMethod(
    f = "selectCells",
    signature = signature(object = "GeneEffect"),
    definition = `selectCells,CCLEMutationData`
)
