#' Import DepMap fusion call data
#'
#' @export
#' @note Updated 2023-08-08.
#'
#' @param filtered `logical(1)`.
#' Load filtered fusion calls.
#'
#' @return `DepMapFusions`.
#'
#' @examples
#' object <- DepMapFusions()
#' dim(object)
DepMapFusions <- # nolint
    function(filtered = TRUE) {
        dataset <- .currentDataset
        assert(isFlag(filtered))
        key <- ifelse(
            test = filtered,
            yes = "fusions",
            no = "fusions_unfiltered"
        )
        url <- datasets[[dataset]][["files"]][["ccle"]][[key]][["url"]]
        assert(isAURL(url))
        df <- .importDataFile(url = url, format = "csv", rownamesCol = NULL)
        assert(is(df, "DataFrame"))
        colnames(df)[colnames(df) == "DepMap_ID"] <- "depmapId"
        colnames(df) <- camelCase(colnames(df), strict = TRUE)
        df <- encode(df)
        metadata(df) <- list(
            "date" = Sys.Date(),
            "dataset" = dataset,
            "filtered" = filtered,
            "packageName" = .pkgName,
            "packageVersion" = .pkgVersion
        )
        new(Class = "DepMapFusions", df)
    }
