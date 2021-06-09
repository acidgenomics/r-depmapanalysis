#' Import CCLE mutation data
#'
#' @export
#' @note Updated 2021-06-09.
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
        df <- .importDataFile(
            dataset = dataset,
            keys = "ccle",
            fileName = "ccle_mutations.csv",
            format = "csv",
            rownamesCol = NULL
        )
        assert(is(df, "DataFrame"))
        colnames(df) <- camelCase(colnames(df), strict = TRUE)
        df <- encode(df)
        metadata(df) <- list(
            "packageName" = .pkgName,
            "packageVersion" = .pkgVersion,
            "dataset" = dataset
        )
        new("CCLEMutationData", df)
    }

formals(CCLEMutationData)[["dataset"]] <- .formalsList[["depmapDataset"]]
