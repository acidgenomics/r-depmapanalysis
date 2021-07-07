#' Import CCLE expression data
#'
#' @export
#' @note Updated 2021-07-07.
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
            dataset = match.arg(dataset),
            assayKey = "expression",
            assayName = "log2Tpm",
            class = "CCLEExpressionData"
        )
    }

formals(CCLEExpressionData)[["dataset"]] <- .formalsList[["depmapDataset"]]
