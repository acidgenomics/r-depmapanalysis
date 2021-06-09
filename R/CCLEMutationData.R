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
    function(dataset = NULL) {
        df <- .importDataFile(
            fileName = "ccle_mutations.csv",
            format = "csv",
            dataset = dataset,
            rownamesCol = NULL
        )
        assert(is(df, "DataFrame"))
        colnames(df) <- camelCase(colnames(df), strict = TRUE)
        metadata(df) <- list(
            "packageVersion" = .pkgVersion,
            "dataset" = dataset
        )
        new("CCLEMutationData", df)
    }

formals(CCLECopyNumberData)[["dataset"]] <- .formalsList[["depmapDataset"]]
