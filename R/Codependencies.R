#' Import Co-dependency scores
#'
#' @export
#' @note Updated 2021-07-19.
#'
#' @inheritParams params
#'
#' @return `Codependencies`.
#'
#' @examples
#' x <- Codependencies(gene = "SOX10")
Codependencies <- function(
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
        "dataset" = dataset,
        "gene" = gene,
        "packageName" = .pkgName,
        "packageVersion" = .pkgVersion
    )
    new(Class = "Codependencies", df)
}
