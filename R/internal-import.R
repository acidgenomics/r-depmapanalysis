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
#' Sample metadata now indicates that there are merged cells we should drop
#' from analysis (e.g. ACH-002260).
#'
#' @note Updated 2022-08-19.
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
        if (isSubset("alias", colnames(df))) {
            x <- df[["alias"]]
            x <- strsplit(x = x, split = ", ", fixed = TRUE)
            x <- CharacterList(x)
            df[["alias"]] <- x
        }
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
        if (anyNA(df[["cellLineName"]])) {
            assert(isSubset("strippedCellLineName", colnames(df)))
            idx <- which(is.na(df[["cellLineName"]]))
            df[["cellLineName"]][idx] <- df[["strippedCellLineName"]][idx]
        }
        ## There can be some problematic cell lines that still persist here
        ## (e.g. ACH-002260).
        keep <- !is.na(df[["cellLineName"]])
        df <- df[keep, ]
        df <- factorize(df)
        df <- encode(df)
        df
    }



#' Import a DepMap data file
#'
#' @note Updated 2022-08-10.
#' @noRd
.importDataFile <-
    function(url,
             format = c("csv", "tsv"),
             rownamesCol = NULL,
             engine = getOption(
                 x = "acid.import.engine",
                 default = ifelse(
                     test = unname(isInstalled("data.table")),
                     yes = "data.table",
                     no = "base"
                 )
             ),
             return = c("DataFrame", "matrix")) {
        assert(
            isAURL(url),
            isScalar(rownamesCol) || is.null(rownamesCol),
            isString(engine)
        )
        format <- match.arg(format)
        return <- match.arg(return)
        ## Engine overrides for malformed DepMap flat file downloads.
        malformedIds <- c(
            31316011L,
            35020903L
        )
        if (as.integer(basename(url)) %in% malformedIds) {
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
