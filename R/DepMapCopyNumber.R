#' Import DepMap (CCLE) copy number data
#'
#' @export
#' @note Updated 2023-01-26.
#'
#' @inheritParams params
#'
#' @return `DepMapCopyNumber`.
#'
#' @examples
#' object <- DepMapCopyNumber()
#' print(object)
DepMapCopyNumber <- # nolint
    function(dataset) {
        .makeCcleSE(
            dataset = match.arg(dataset),
            assayKey = "copy_number",
            assayName = "log2CopyNumber",
            class = "DepMapCopyNumber"
        )
    }

formals(DepMapCopyNumber)[["dataset"]] <- # nolint
    .formalsList[["depmapDataset"]]
