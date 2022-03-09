#' Import CCLE mutation data
#'
#' @export
#' @note Updated 2022-03-09.
#'
#' @inheritParams params
#'
#' @return `CCLEMutationData`.
#'
#' @examples
#' object <- CCLEMutationData()
#' dim(object)
CCLEMutationData <-  # nolint
    function(dataset) {
        dataset <- match.arg(dataset)
        url <- datasets[[dataset]][["ccle"]][["mutations"]][["url"]]
        assert(isAURL(url))
        df <- .importDataFile(url = url, format = "csv", rownamesCol = NULL)
        assert(is(df, "DataFrame"))
        colnames(df) <- camelCase(colnames(df), strict = TRUE)
        df <- encode(df)
        metadata(df) <- list(
            "date" = Sys.Date(),
            "dataset" = dataset,
            "packageName" = .pkgName,
            "packageVersion" = .pkgVersion
        )
        new("CCLEMutationData", df)
    }

formals(CCLEMutationData)[["dataset"]] <- .formalsList[["depmapDataset"]]
