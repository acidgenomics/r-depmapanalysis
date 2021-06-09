#' Import CCLE expression data
#'
#' @export
#' @note Updated 2021-06-09.
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
        .makeCcle(
            class = "CCLEExpressionData",
            assayName = "log2Tpm",
            fileName = "ccle_expression.csv",
            dataset = match.arg(dataset),
            rowData = TRUE,
            colData = TRUE
        )
    }

formals(CCLEExpressionData)[["dataset"]] <- .formalsList[["depmapDataset"]]
