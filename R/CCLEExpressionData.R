#' Import CCLE expression data
#'
#' @export
#' @note Updated 2021-06-10.
#'
#' @inheritParams params
#'
#' @return `CCLEExpressionData`.
#'
#' @examples
#' object <- CCLEExpressionData()
#' dim(object)
CCLEExpressionData <-  # nolint
    function(dataset) {
        .makeCcleSE(
            class = "CCLEExpressionData",
            assayName = "log2Tpm",
            fileName = "ccle_expression.csv",
            dataset = match.arg(dataset)
        )
    }

formals(CCLEExpressionData)[["dataset"]] <- .formalsList[["depmapDataset"]]
