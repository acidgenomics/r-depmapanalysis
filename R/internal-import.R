#' Cache URL into package
#'
#' @note Updated 2023-03-08.
#' @noRd
.cacheURL <-
    function(url) {
        cacheURL(url = url, pkg = .pkgName)
    }



#' Import a DepMap data file
#'
#' @note Updated 2023-03-08.
#' @noRd
.importDataFile <-
    function(url,
             format = c("csv", "tsv"),
             colnames = TRUE,
             rownameCol = NULL,
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
            isFlag(colnames),
            isScalar(rownameCol) || is.null(rownameCol),
            isString(engine)
        )
        format <- match.arg(format)
        return <- match.arg(return)
        ## Engine overrides for malformed DepMap flat file downloads.
        malformedIds <- c(31316011L, 35020903L)
        if (isSubset(x = as.integer(basename(url)), y = malformedIds)) {
            requireNamespaces("data.table")
            engine <- "data.table"
        }
        tmpfile <- .cacheURL(url = url)
        df <- import(
            con = tmpfile,
            format = format,
            rownameCol = rownameCol,
            colnames = colnames,
            engine = engine
        )
        out <- switch(
            EXPR = return,
            "DataFrame" = as(df, "DataFrame"),
            "matrix" = as.matrix(df)
        )
        out <- makeDimnames(out)
        out
    }
