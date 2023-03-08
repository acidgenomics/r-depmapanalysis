#' Import DepMap expression data
#'
#' @export
#' @note Updated 2022-01-26.
#'
#' @inheritParams params
#'
#' @return `DepMapExpression`.
#'
#' @examples
#' object <- DepMapExpression()
#' dim(object)
DepMapExpression <- # nolint
    function(dataset) {
        ## FIXME Rethink this approach.
        .makeCcleSE(
            dataset = match.arg(dataset),
            assayKey = "expression",
            assayName = "log2Tpm",
            class = "DepMapExpression"
        )
    }

formals(DepMapExpression)[["dataset"]] <- # nolint
    .formalsList[["depmapDataset"]]
