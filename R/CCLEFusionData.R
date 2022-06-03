#' Import CCLE fusion data
#'
#' @export
#' @note Updated 2022-03-09.
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
        url <- datasets[[dataset]][["ccle"]][[key]][["url"]]
        assert(isAURL(url))
        df <- .importDataFile(url = url, format = "csv", rownamesCol = NULL)
        assert(is(df, "DataFrame"))
        colnames(df) <- camelCase(colnames(df), strict = TRUE)
        ## e.g. "depmap_public_20q3".
        colnames(df)[colnames(df) == "xFusionName"] <- "fusionName"
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

formals(CCLEFusionData)[["dataset"]] <- .formalsList[["depmapDataset"]]
