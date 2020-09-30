#' Import DepMap data file
#'
#' @note Updated 2020-09-30.
#' @noRd
.importDataFile <- function(
    fileName,
    release,
    type = c("cellular_models", "genetic_dependency"),
    rownamesCol = NULL
) {
    if (is.null(release)) {
        release <- .currentRelease
    }
    assert(
        isString(fileName),
        isString(fileID),
        isString(release),
        isString(rownamesCol, nullOK = TRUE)
    )
    type <- match.arg(type)
    cli_alert(sprintf(
        "Downloading {.file %s} from DepMap {.var %s} release.",
        fileName, release
    ))
    fileID <- .depmap[[tolower(release)]][["cellular_models"]][[fileName]]
    file <- .cacheDataFile(fileName = fileName, fileID = fileID)
    suppressMessages({
        df <- import(file = file, format = "csv")
    })
    df <- snakeCase(df)
    if (isString(rownamesCol)) {
        assert(isSubset(rownamesCol, colnames(df)))
        df <- column_to_rownames(df, rownamesCol)
        df <- makeDimnames(df)
    }
    df <- as(df, "DataFrame")
    df
}



#' Import CCLE expression data
#'
#' @export
#' @note Updated 2020-09-30.
#'
#' @inheritParams params
#'
#' @return `DataFrame`.
#'
#' @examples
#' df <- importCCLEExpressionData()
importCCLEExpressionData <- function(release = NULL) {
    df <- .importDataFile(
        fileName = "ccle_expression.csv",
        type = "cellular_models",
        release = release,
        rownamesCol = "x"
    )
}



#' Import cell line sample info
#'
#' @export
#' @note Updated 2020-09-30.
#'
#' @inheritParams params
#'
#' @return `DataFrame`.
#'
#' @examples
#' df <- importCellLineSampleInfo()
importCellLineSampleInfo <- function(release = NULL) {
    .importDataFile(
        fileName = "sample_info.csv",
        type = "cellular_models",
        release = release,
        rownamesCol = "dep_map_id"
    )
}
