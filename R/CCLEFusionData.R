#' Import CCLE fusion data
#'
#' @export
#' @note Updated 2021-07-07.
#'
#' @inheritParams params
#'
#' @return `CCLEFusionData`.
#'
#' @examples
#' object <- CCLEFusionData()
#' dim(object)
CCLEFusionData <-  # nolint
    function(dataset, filtered = TRUE) {
        dataset <- match.arg(dataset)
        assert(isFlag(filtered))
        key <- ifelse(
            test = filtered,
            yes = "fusions",
            no = "fusions_unfiltered"
        )
        url <- .datasets[[dataset]][["ccle"]][[key]][["url"]]
        assert(isAURL(url))
        df <- .importDataFile(url = url, format = "csv", rownamesCol = NULL)
        assert(is(df, "DataFrame"))
        colnames(df) <- camelCase(colnames(df), strict = TRUE)
        df <- encode(df)
        metadata(df) <- list(
            "dataset" = dataset,
            "filtered" = filtered,
            "packageName" = .pkgName,
            "packageVersion" = .pkgVersion
        )
        new("CCLEFusionData", df)
    }

formals(CCLEFusionData)[["dataset"]] <- .formalsList[["depmapDataset"]]
