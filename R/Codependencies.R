#' Import Co-dependency scores
#'
#' @export
#' @note Updated 2022-08-05.
#'
#' @inheritParams params
#'
#' @return `Codependencies`.
#'
#' @seealso
#' - https://forum.depmap.org/t/down-load-of-co-dependencies/175/
#' - Python script for processing of all co-dependency scores:
#' https://gist.github.com/pgm/ac2ac4c664ef81200ce49133cc4cee02
#'
#' @examples
#' x <- Codependencies(geneName = "SOX10")
Codependencies <- # nolint
    function(geneName,
             dataset = c(
                 "Chronos_Combined",
                 "CERES_Combined",
                 "RNAi_merged"
             )) {
        assert(isString(geneName))
        dataset <- match.arg(dataset)
        url <- pasteURL(
            "depmap.org",
            "portal",
            "gene",
            geneName,
            paste0(
                "top_correlations?",
                "dataset_name=", dataset
            ),
            protocol = "https"
        )
        tmpfile <- .cacheURL(url)
        df <- import(file = tmpfile, format = "csv")
        df <- as(df, "DataFrame")
        colnames(df) <- camelCase(colnames(df), strict = TRUE)
        dataset <- df[["dataset"]][[1L]]
        df[["dataset"]] <- NULL
        colnames(df)[colnames(df) == "gene"] <- "geneName"
        metadata(df) <- list(
            "date" = Sys.Date(),
            "dataset" = dataset,
            "geneName" = geneName,
            "packageName" = .pkgName,
            "packageVersion" = .pkgVersion
        )
        new(Class = "Codependencies", df)
    }
