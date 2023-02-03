## NOTE Consider reworking using pool approach from Cellosaurus package.

## FIXME We need to improve this by splitting the "alias" column into CharacterList.
## FIXME Doing it here, but we should pre-process the colData instead.
## FIXME Add support for ccleName metadata input.
## FIXME We may want this to simply return the position rather than the colname.

#' Map cell name input to column names
#'
#' @note Updated 2023-01-26.
#' @noRd
#'
#' @seealso
#' - `AcidExperiment::mapGenesToRownames`.
.mapCellsToColnames <- function(object, cells) {
    assert(
        is(object, "SummarizedExperiment"),
        hasRownames(object),
        isCharacter(cells)
    )
    colData <- colData(object)
    if (is.character(colData[["alias"]])) {
        x <- colData[["alias"]]
        x <- strsplit(x = x, split = ", ", fixed = TRUE)
        x <- CharacterList(x)
        colData[["alias"]] <- x
    }
    idx <- vapply(
        X = cells,
        object = colData,
        FUN = function(x, object) {
            idx <- match(x = x, table = rownames(object))
            if (isInt(idx)) {
                return(idx)
            }
            idx <- match(x = x, table = object[["cellLineName"]])
            if (isInt(idx)) {
                return(idx)
            }
            idx <- match(x = x, table = object[["strippedCellLineName"]])
            if (isInt(idx)) {
                return(idx)
            }
            idx <- match(x = x, table = object[["cellosaurusId"]])
            if (isInt(idx)) {
                return(idx)
            }
            idx <- match(x = x, table = object[["depmapId"]])
            if (isInt(idx)) {
                return(idx)
            }
            idx <- match(x = x, table = object[["ccleId"]])
            if (isInt(idx)) {
                return(idx)
            }
            idx <- match(x = x, table = object[["sangerModelId"]])
            if (isInt(idx)) {
                return(idx)
            }
            if (isSubset("alias", colnames(object))) {
                idx <- which(bapply(
                    X = object[["alias"]],
                    FUN = function(table) {
                        x %in% table
                    }
                ))
                if (isInt(idx)) {
                    return(idx)
                }
            }
            abort(sprintf("Failed to map cell: {.val %s}.", x))
        },
        FUN.VALUE = integer(1L),
        USE.NAMES = TRUE
    )
    out <- colnames(object)[idx]
    names(out) <- names(idx)
    out
}
