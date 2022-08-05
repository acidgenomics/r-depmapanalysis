#' Cache URL into package
#'
#' @note Updated 2021-07-08.
#' @noRd
.cacheURL <-
    function(url, ...) {
        alert(sprintf("Downloading DepMap file {.url %s}.", url))
        cacheURL(url, pkg = .pkgName, ...)
    }



#' Import cell line sample metadata
#'
#' @note Updated 2022-08-05.
#' @noRd
.importCellLineSampleData <- # nolint
    function(dataset) {
        url <- datasets[[dataset]][["metadata"]][["sample_info"]][["url"]]
        df <- .importDataFile(
            url = url,
            rownamesCol = 1L,
            return = "DataFrame"
        )
        assert(is(df, "DataFrame"))
        colnames(df) <- camelCase(colnames(df))
        if (
            !isSubset("cellLineName", colnames(df)) &&
                isSubset("strippedCellLineName", colnames(df))
        ) {
            ## e.g. "depmap_public_20q3".
            df[["cellLineName"]] <- df[["strippedCellLineName"]]
        } else if (
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



# NOTE There are currently some malformed DepMap flat files:
#
# - https://ndownloader.figshare.com/files/35020903
#   Using base engine:
#     line 1306 did not have 29 elements
# - https://ndownloader.figshare.com/files/13515395
#   Using readr:
#     New names `` -> `...1 issue`



#' Import a DepMap data file
#'
#' @note Updated 2022-08-05.
#' @noRd
.importDataFile <-
    function(url,
             format = c("csv", "tsv"),
             rownamesCol = NULL,
             engine = unname(ifelse(
                 test = isInstalled("data.table"),
                 yes = "data.table",
                 no = "base"
             )),
             return = c("DataFrame", "matrix")) {
        assert(
            isAURL(url),
            isScalar(rownamesCol) || is.null(rownamesCol),
            isString(engine)
        )
        format <- match.arg(format)
        return <- match.arg(return)
        ## Engine overrides for malformed flat files.
        if (as.integer(basename(url)) %in% c(35020903L)) {
            requireNamespaces("data.table")
            engine <- "data.table"
        }
        tmpfile <- .cacheURL(url = url)
        df <- import(
            file = tmpfile,
            format = format,
            engine = engine
        )
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
#' @note Updated 2022-08-05.
#' @noRd
.importGeneDataFile <-
    function(url) {
        df <- .importDataFile(
            url = url,
            engine = "base",
            return = "DataFrame"
        )
        colnames(df) <- camelCase(colnames(df))
        if (identical(colnames(df), c("geneSymbol", "geneId"))) {
            ## e.g. DEMETER2 control files.
            vec <- paste0(df[["geneSymbol"]], " (", df[["geneId"]], ")")
        } else {
            ## e.g. DepMap current public release files.
            assert(
                identical(colnames(df), "gene"),
                isCharacter(df[["gene"]])
            )
            vec <- df[["gene"]]
        }
        vec <- sort(vec)
        vec
    }
