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
    function(
        dataset = NULL,
        rowData = TRUE,
        colData = TRUE
    ) {
        .makeCcle(
            class = "CCLEExpressionData",
            assayName = "expression",
            fileName = "ccle_expression.csv",
            dataset = dataset,
            rowData = rowData,
            colData = colData
        )
    }

formals(CCLECopyNumberData)[["dataset"]] <- .formalsList[["depmapDataset"]]
