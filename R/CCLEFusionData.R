#' Import CCLE fusion data
#'
#' @export
#' @note Updated 2022-09-21.
#'
#' @inheritParams params
#' @param filtered `logical(1)`.
#' Load filtered fusion calls.
#'
#' @return `CCLEFusionData`.
#'
#' @examples
#' object <- CCLEFusionData()
#' dim(object)
CCLEFusionData <- # nolint
    function(dataset, filtered = TRUE) {
        dataset <- match.arg(dataset)
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
        new("CCLEFusionData", df)
    }

formals(CCLEFusionData)[["dataset"]] <- # nolint
    .formalsList[["depmapDataset"]]
