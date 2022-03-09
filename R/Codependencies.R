#' Import Co-dependency scores
#'
#' @export
#' @note Updated 2022-03-09.
#'
#' @inheritParams params
#'
#' @return `Codependencies`.
#'
#' @seealso
#' - https://forum.depmap.org/t/down-load-of-co-dependencies/175/
#' - Python script for processing of all co-dependency scores:
#'   https://gist.github.com/pgm/ac2ac4c664ef81200ce49133cc4cee02
#'
#' @examples
#' x <- Codependencies(gene = "SOX10")
Codependencies <-  # nolint
    function(
        gene,
        dataset = c(
            "Chronos_Combined",
            "CERES_Combined",
            "RNAi_merged"
        )
    ) {
        assert(isString(gene))
        dataset <- match.arg(dataset)
        url <- pasteURL(
            "depmap.org",
            "portal",
            "gene",
            gene,
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
        metadata(df) <- list(
            "date" = Sys.Date(),
            "dataset" = dataset,
            "gene" = gene,
            "packageName" = .pkgName,
            "packageVersion" = .pkgVersion
        )
        new(Class = "Codependencies", df)
    }
