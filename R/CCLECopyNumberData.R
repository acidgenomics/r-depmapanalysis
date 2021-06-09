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
        dataset,
        rowData = TRUE,
        colData = TRUE
    ) {
        .makeCcle(
            class = "CCLECopyNumberData",
            assayName = "log2CopyNumber",
            fileName = "ccle_gene_cn.csv",
            dataset = match.arg(dataset),
            rowData = rowData,
            colData = colData
        )
    }

formals(CCLECopyNumberData)[["dataset"]] <- .formalsList[["depmapDataset"]]