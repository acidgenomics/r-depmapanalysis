#' Import DepMap (CCLE) copy number data
#'
#' @export
#' @note Updated 2023-01-26.
#'
#' @inheritParams params
#'
#' @return `CopyNumber`.
#'
#' @examples
#' object <- CopyNumber()
#' print(object)
CopyNumber <- # nolint
    function(dataset) {
        .makeCcleSE(
            dataset = match.arg(dataset),
            assayKey = "copy_number",
            assayName = "log2CopyNumber",
            class = "CopyNumber"
        )
    }

formals(CopyNumber)[["dataset"]] <- # nolint
    .formalsList[["depmapDataset"]]
