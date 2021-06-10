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



## FIXME This is failing for DEMETER2 dataset...



#' Import cell line sample metadata
#'
#' @note Updated 2021-06-10.
#' @noRd
.importCellLineSampleData <-  # nolint
    function(dataset) {
        df <- .importDataFile(
            dataset = dataset,
            keys = "metadata",
            fileName = "sample_info.csv",
            rownamesCol = 1L
        )
        assert(is(df, "DataFrame"))
        colnames(df) <- camelCase(colnames(df))
        if (
            !isSubset("cellLineName", colnames(df)) &&
            isSubset("ccleId", colnames(df))
        ) {
            x <- df[["ccleId"]]
            x <- vapply(
                X = strsplit(x = x, split = "_", fixed = TRUE),
                FUN = `[`,
                i = 1L,
                FUN.VALUE = character(1L)
            )
            df[["cellLineName"]] <- x
        }
        assert(isSubset("cellLineName", colnames(df)))
        df
    }



#' Import control essential genes
#'
#' @note Updated 2020-10-02.
#' @noRd
.importControlCommonEssentials <-
    function(dataset) {
        .importGeneDataFile(
            dataset = dataset,
            keys = "controls",
            fileName = "common_essentials.csv"
        )
    }



#' Import control non-essential genes
#'
#' @note Updated 2020-10-02.
#' @noRd
.importControlNonessentials <-
    function(dataset) {
        .importGeneDataFile(
            dataset = dataset,
            keys = "controls",
            fileName = "nonessentials.csv"
        )
    }



#' Import a DepMap data file
#'
#' @note Updated 2021-06-10.
#' @noRd
.importDataFile <- function(
    dataset,
    keys,
    fileName,
    format = c("csv", "tsv"),
    rownamesCol = NULL,
    engine = "data.table",
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
    keys <- c(dataset, keys, fileName)
    alert(sprintf(
        "Importing {.file %s} from {.var %s} dataset.",
        fileName, dataset
    ))
    assert(
        isSubset(dataset, names(.datasets)),
        msg = sprintf("Unsupported dataset: '%s'.", dataset)
    )
    tryCatch(
        expr = {
            fileId <- `[[`(.datasets, keys)
        },
        error = function(e) {
            stop(sprintf("Unsupported dataset keys: %s", toString(keys)))
        }
    )
    assert(isInt(fileId))
    file <- .cacheDataFile(fileName = fileName, fileId = fileId)
    df <- import(file = file, format = format, engine = engine, quiet = TRUE)
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
    function(dataset, keys, fileName) {
        df <- .importDataFile(
            dataset = dataset,
            keys = keys,
            fileName = fileName,
            ## Don't use 'data.table' here.
            engine = "base",
            return = "DataFrame"
        )
        assert(isCharacter(df[["gene"]]))
        vec <- sort(df[["gene"]])
        vec
    }
