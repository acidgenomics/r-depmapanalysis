#' Cache URL into package
#'
#' @note Updated 2021-07-07.
#' @noRd
.cacheURL <- function(...) {
    cacheURL(..., pkg = .pkgName)
}



## FIXME Need to rethink this.

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



## FIXME Need to rethink this.

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
#' @note Updated 2021-07-07.
#' @noRd
.importDataFile <- function(
    url,
    format = c("csv", "tsv"),
    rownamesCol = NULL,
    engine = "data.table",
    return = c("DataFrame", "matrix")
) {
    assert(
        isAURL(url),
        isScalar(rownamesCol) || is.null(rownamesCol),
        isString(engine)
    )
    format <- match.arg(format)
    return <- match.arg(return)
    tmpfile <- .cacheURL(url = url)
    df <- import(file = tmpfile, format = format, engine = engine)
    if (isScalar(rownamesCol)) {
        if (!isString(rownamesCol)) {
            rownamesCol <- colnames(df)[[rownamesCol]]
        }
        assert(isSubset(rownamesCol, colnames(df)))
        rownames(df) <- df[[rownamesCol]]
    }
    out <- switch(
        EXPR = return,
        "DataFrame" = as(df, "DataFrame"),
        "matrix" = {
            if (hasRownames(df)) df[[rownamesCol]] <- NULL
            as.matrix(df)
        }
    )
    out <- makeDimnames(out)
    out
}



#' Import a DepMap file containing gene identifiers
#'
#' @note Updated 2021-07-07.
#' @noRd
.importGeneDataFile <-
    function(url) {
        df <- .importDataFile(
            url = url,
            ## Don't use 'data.table' here.
            engine = "base",
            return = "DataFrame"
        )
        assert(isCharacter(df[["gene"]]))
        vec <- sort(df[["gene"]])
        vec
    }
