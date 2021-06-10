#' Map cell name input to column names
#'
#' @note Updated 2021-06-10.
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
    libraryType <- metadata(object)[["libraryType"]]
    colData <- colData(object)
    if (is.character(colData[["alias"]])) {
        x <- colData[["alias"]]
        x <- strsplit(x = x, split = ", ", fixed = TRUE)
        x <- CharacterList(x)
        colData[["alias"]] <- x
    }
    if (
        identical(libraryType, "rnai") &&
        !isSubset("cellLineName", colnames(colData)) &&
        isSubset("ccleId", colnames(colData))
    ) {
        x <- colData[["ccleId"]]
        x <- vapply(
            X = strsplit(x = x, split = "_", fixed = TRUE),
            FUN = `[`,
            i = 1L,
            FUN.VALUE = character(1L)
        )
        colData[["cellLineName"]] <- x
    }
    idx <- vapply(
        X = cells,
        object = colData,
        libraryType = libraryType,
        FUN = function(x, object, libraryType) {
            idx <- match(x = x, table = rownames(object))
            if (isInt(idx)) return(idx)
            idx <- match(x = x, table = object[["cellLineName"]])
            if (isInt(idx)) return(idx)
            idx <- match(x = x, table = object[["strippedCellLineName"]])
            if (isInt(idx)) return(idx)
            idx <- match(x = x, table = object[["rrid"]])
            if (isInt(idx)) return(idx)
            idx <- match(x = x, table = object[["depMapId"]])
            if (isInt(idx)) return(idx)
            idx <- match(x = x, table = object[["ccleId"]])
            if (isInt(idx)) return(idx)
            idx <- match(x = x, table = object[["sangerModelId"]])
            if (isInt(idx)) return(idx)
            if (isSubset("alias", colnames(object))) {
                idx <- which(bapply(
                    X = object[["alias"]],
                    FUN = function(table) {
                        x %in% table
                    }
                ))
                if (isInt(idx)) return(idx)
            }
            stop(sprintf("Failed to map cell: %s.", x))
        },
        FUN.VALUE = integer(1L),
        USE.NAMES = TRUE
    )
    out <- colnames(object)[idx]
    names(out) <- names(idx)
    out
}
