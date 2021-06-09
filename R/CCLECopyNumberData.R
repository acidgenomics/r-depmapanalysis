#' Import CCLE copy number data
#'
#' @export
#' @note Updated 2021-06-09.
#'
#' @inheritParams params
#'
#' @return `CCLECopyNumberData`.
#'
#' @examples
#' object <- CCLECopyNumberData()
#' print(object)
CCLECopyNumberData <-  # nolint
    function(
        dataset = NULL,
        rowData = TRUE,
        colData = TRUE
    ) {
        .makeCcle(
            class = "CCLECopyNumberData",
            assayName = "copyNumber",
            fileName = "ccle_gene_cn.csv",
            dataset = dataset,
            rowData = rowData,
            colData = colData
        )
    }

formals(CCLECopyNumberData)[["dataset"]] <- .formalsList[["dataset"]]
