#' Import DepMap copy number data
#'
#' @export
#' @note Updated 2023-08-03.
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
        ## FIXME Rework using `.makeBroadSE` internal function.
        .makeCcleSE(
            dataset = match.arg(dataset),
            assayKey = "copy_number",
            assayName = "log2CopyNumber",
            class = "DepMapCopyNumber"
        )
    }

formals(DepMapCopyNumber)[["dataset"]] <- # nolint
    .formalsList[["depmapDataset"]]
