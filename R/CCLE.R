#' Import CCLE copy number data
#'
#' @export
#' @note Updated 2021-02-24.
#'
#' @inheritParams params
#'
#' @return `CCLECopyNumberData`.
#'
#' @examples
#' object <- CCLECopyNumberData()
#' dim(object)
CCLECopyNumberData <-  # nolint
    function(release = NULL) {
        df <- .importDataFile(
            fileName = "ccle_gene_cn.csv",
            release = .matchDepMapRelease(release),
            rownamesCol = 1L
        )
        assert(is(df, "DataFrame"))
        new("CCLECopyNumberData", df)
    }

formals(CCLECopyNumberData)[["release"]] <- .currentDepMapRelease



#' Import CCLE expression data
#'
#' @export
#' @note Updated 2021-02-24.
#'
#' @inheritParams params
#'
#' @return `CCLEExpressionData`.
#'
#' @examples
#' object <- CCLEExpressionData()
#' dim(object)
CCLEExpressionData <-  # nolint
    function(release = NULL) {
        df <- .importDataFile(
            fileName = "ccle_expression.csv",
            release = .matchDepMapRelease(release),
            rownamesCol = 1L
        )
        assert(is(df, "DataFrame"))
        new("CCLEExpressionData", df)
    }

formals(CCLEExpressionData)[["release"]] <- .currentDepMapRelease



#' Import CCLE mutation data
#'
#' @export
#' @note Updated 2021-02-24.
#'
#' @inheritParams params
#'
#' @return `CCLEMutationData`.
#'
#' @examples
#' object <- CCLEMutationData()
#' dim(object)
CCLEMutationData <-  # nolint
    function(release = NULL) {
        df <- .importDataFile(
            fileName = "ccle_mutations.csv",
            format = "tsv",
            release = .matchDepMapRelease(release),
            rownamesCol = NULL
        )
        assert(is(df, "DataFrame"))
        new("CCLEMutationData", df)
    }

formals(CCLEMutationData)[["release"]] <- .currentDepMapRelease
