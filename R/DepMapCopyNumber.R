#' Import DepMap copy number data
#'
#' @export
#' @note Updated 2023-08-08.
#'
#' @return `DepMapCopyNumber`.
#'
#' @examples
#' object <- DepMapCopyNumber()
#' print(object)
DepMapCopyNumber <- # nolint
    function() {
        .makeBroadSingleAssaySE(
            file = "OmicsCNGene.csv",
            assayName = "log2CopyNumber",
            class = "DepMapCopyNumber"
        )
    }
