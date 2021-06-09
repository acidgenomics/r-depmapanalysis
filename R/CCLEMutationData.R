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
        df <- .importDataFile(
            dataset = match.arg(dataset),
            keys = "ccle",
            fileName = "ccle_mutations.csv",
            format = "csv",
            rownamesCol = NULL
        )
        assert(is(df, "DataFrame"))
        colnames(df) <- camelCase(colnames(df), strict = TRUE)
        metadata(df) <- list(
            "packageVersion" = .pkgVersion,
            "dataset" = dataset
        )
        df <- encode(df)
        new("CCLEMutationData", df)
    }

formals(CCLEMutationData)[["dataset"]] <- .formalsList[["depmapDataset"]]
