#' Import CCLE copy number data
#'
#' @export
#' @note Updated 2021-07-07.
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
            dataset = match.arg(dataset),
            assayKey = "copy_number",
            assayName = "log2CopyNumber",
            class = "CCLECopyNumberData"
        )
    }

formals(CCLECopyNumberData)[["dataset"]] <- .formalsList[["depmapDataset"]]
