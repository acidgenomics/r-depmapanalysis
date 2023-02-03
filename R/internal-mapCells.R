## NOTE Consider reworking using pool approach from Cellosaurus package.

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
    ## FIXME Return NA and then error at the end in this case...
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
            NA_integer_
        },
        FUN.VALUE = integer(1L),
        USE.NAMES = TRUE
    )
    if (anyNA(idx)) {
        fail <- names(idx)[is.na(idx)]
        abort(sprintf(
            "Failed to map %d %s: %s.",
            length(fail),
            ngettext(
                n = length(fail),
                msg1 = "cell",
                msg2 = "cells"
            ),
            toInlineString(fail)
        ))
    }
    out <- colnames(object)[idx]
    names(out) <- names(idx)
    out
}
