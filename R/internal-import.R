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
#' fileId <- .datasets[["depmap_public_21q2"]][["metadata"]][[fileName]]
#' .cacheDataFile(fileName = fileName, fileId = fileId)
.cacheDataFile <- function(fileName, fileId, verbose = TRUE) {
    urlStem <- .urlStem
    assert(
        isString(fileName),
        isInt(fileId),
        isAURL(urlStem),
        isFlag(verbose)
    )
    url <- pasteURL(urlStem, as.character(fileId))
    file <- cacheURL(url = url, pkg = .pkgName, verbose = verbose)
    assert(isAFile(file))
    file
}



#' Import cell line sample metadata
#'
#' @note Updated 2020-10-01.
#' @noRd
.importCellLineSampleData <-  # nolint
    function(dataset) {
        df <- .importDataFile(
            fileName = "sample_info.csv",
            keys = "metadata",
            dataset = dataset,
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
    function(dataset) {
        .importGeneDataFile(
            fileName = "achilles_common_essentials.csv",
            dataset = dataset
        )
    }



#' Import control essential genes
#'
#' @note Updated 2020-10-02.
#' @noRd
.importControlCommonEssentials <-
    function(dataset) {
        .importGeneDataFile(
            fileName = "common_essentials.csv",
            dataset = dataset
        )
    }



#' Import control non-essential genes
#'
#' @note Updated 2020-10-02.
#' @noRd
.importControlNonessentials <-
    function(dataset) {
        .importGeneDataFile(
            fileName = "nonessentials.csv",
            dataset = dataset
        )
    }



#' Import a DepMap data file
#'
#' @note Updated 2021-06-09.
#' @noRd
.importDataFile <- function(
    dataset,
    keys,
    fileName,
    ## FIXME Consider adding lines support here?
    ## FIXME We can do this for genes file and then remove the first element...
    format = c("csv", "tsv"),
    rownamesCol = NULL,
    engine = "data.table",
    ## FIXME Allow character return if format is lines?
    return = c("DataFrame", "matrix")
) {
    assert(
        isString(dataset),
        isCharacter(keys),
        isString(fileName),
        isScalar(rownamesCol) || is.null(rownamesCol),
        isString(engine),
        is.list(.datasets)
    )
    format <- match.arg(format)
    return <- match.arg(return)
    dataset <- snakeCase(dataset)
    keys <- c(dataset, keys, fileName)
    alert(sprintf(
        "Importing {.file %s} from {.var %s} dataset.",
        fileName, dataset
    ))

    ## FIXME For key, allow multiple levels.
    ## This currently won't traverse down multiple levels...how to do this
    ## with a while/break approach?
    ## e.g. harmonia, chromos/ceres

    assert(
        isSubset(dataset, names(.datasets)),
        msg = "Unsupported dataset."
    )
    fileId <- `[[`(.datasets, keys)
    assert(isInt(fileId))
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
    function(fileName, dataset) {
        df <- .importDataFile(
            fileName = fileName,
            dataset = dataset,
            ## Don't use 'data.table' here.
            engine = "base",
            return = "DataFrame"
        )
        assert(isCharacter(df[["gene"]]))
        vec <- sort(df[["gene"]])
        vec
    }
