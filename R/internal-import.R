#' Download and cache a data file from DepMap into BiocFileCache
#'
#' @note Updated 2021-02-10.
#' @noRd
#'
#' @param fileId `character(1)`.
#'   DepMap file identifier on figshare.com.
#' @param fileName `character(1)`.
#'   File name.
#'
#' @return `character(1)`.
#'   Cached file path on disk.
#'
#' @examples
#' fileName <- "sample_info.csv"
#' fileId <- .depmap[["20q3"]][["cellular_models"]][[fileName]]
#' .cacheDataFile(fileName = fileName, fileId = fileId)
.cacheDataFile <- function(fileName, fileId, verbose = TRUE) {
    urlStem <- .urlStem
    assert(
        isAURL(urlStem),
        isFlag(verbose)
    )
    url <- pasteURL(urlStem, fileId)
    file <- cacheURL(url = url, pkg = .pkgName, verbose = verbose)
    assert(isAFile(file))
    file
}



#' Import cell line sample metadata
#'
#' @note Updated 2020-10-01.
#' @noRd
.importCellLineSampleData <-  # nolint
    function(release) {
        df <- .importDataFile(
            fileName = "sample_info.csv",
            release = release,
            rownamesCol = 1L
        )
        assert(is(df, "DataFrame"))
        df <- snakeCase(df)
        df
    }



#' Import common essential genes
#'
#' @note Updated 2020-10-02.
#' @noRd
.importCommonEssentials <-
    function(release) {
        .importGeneDataFile(
            fileName = "achilles_common_essentials.csv",
            release = release
        )
    }



#' Import control essential genes
#'
#' @note Updated 2020-10-02.
#' @noRd
.importControlCommonEssentials <-
    function(release) {
        .importGeneDataFile(
            fileName = "common_essentials.csv",
            release = release
        )
    }



#' Import control non-essential genes
#'
#' @note Updated 2020-10-02.
#' @noRd
.importControlNonessentials <-
    function(release) {
        .importGeneDataFile(
            fileName = "nonessentials.csv",
            release = release
        )
    }



#' Import DepMap data file
#'
#' @note Updated 2020-09-30.
#' @noRd
.importDataFile <- function(
    fileName,
    release,
    format = c("csv", "tsv"),
    rownamesCol = NULL,
    engine = "data.table",
    return = c("DataFrame", "matrix")
) {
    assert(
        isString(fileName),
        isString(release),
        isScalar(rownamesCol) || is.null(rownamesCol),
        isString(engine)
    )
    format <- match.arg(format)
    return <- match.arg(return)
    alert(sprintf(
        "Importing {.file %s} from DepMap {.var %s} release.",
        fileName, release
    ))
    fileId <- .depmap[[tolower(release)]][[fileName]]
    file <- .cacheDataFile(fileName = fileName, fileId = fileId)
    df <- import(file = file, format = format, engine = engine)
    if (isScalar(rownamesCol)) {
        if (!isString(rownamesCol)) {
            rownamesCol <- colnames(df)[[rownamesCol]]
        }
        assert(isSubset(rownamesCol, colnames(df)))
        rownames(df) <- df[[rownamesCol]]
    }
    df <- switch(
        EXPR = return,
        "DataFrame" = as(df, "DataFrame"),
        "matrix" = {
            if (hasRownames(df)) df[[rownamesCol]] <- NULL
            as.matrix(df)
        }
    )
    df <- makeDimnames(df)
    df
}



#' Import a DepMap file containing gene identifiers
#'
#' @note Updated 2021-06-08.
#' @noRd
.importGeneDataFile <-
    function(fileName, release) {
        df <- .importDataFile(
            fileName = fileName,
            release = release,
            ## Don't use 'data.table' here.
            engine = "base",
            return = "DataFrame"
        )
        assert(isCharacter(df[["gene"]]))
        vec <- sort(df[["gene"]])
        vec
    }
