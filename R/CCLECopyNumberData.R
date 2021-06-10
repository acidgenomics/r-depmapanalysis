#' Import CCLE copy number data
#'
#' @export
#' @note Updated 2021-06-10.
#'
#' @inheritParams params
#'
#' @return `CCLECopyNumberData`.
#'
#' @examples
#' object <- CCLECopyNumberData()
#' print(object)
CCLECopyNumberData <-  # nolint
    function(dataset) {
        .makeCcleSE(
            class = "CCLECopyNumberData",
            assayName = "log2CopyNumber",
            fileName = "ccle_gene_cn.csv",
            dataset = match.arg(dataset)
        )
    }

formals(CCLECopyNumberData)[["dataset"]] <- .formalsList[["depmapDataset"]]
