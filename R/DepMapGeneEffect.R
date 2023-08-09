## FIXME Seems like the RNAi generator is not returning any genes now?



#' DepMap gene effect in cancer cell lines
#'
#' @name DepMapGeneEffect
#' @note Updated 2023-08-09.
#'
#' @section Assays:
#'
#' - `effect`: **Chronos or CERES data** with principle components strongly
#' related to known batch effects removed, then shifted and scaled per cell
#' line so the median nonessential KO effect is 0 and the median essential KO
#' effect is -1.
#' - `probability`: **Probability** that knocking out the gene has a real
#' depletion effect using `gene_effect`.
#'
#'
#' @return `DepMapGeneEffect`.
#'
#' @examples
#' ## CRISPR.
#' object <- DepMapCrisprGeneEffect()
#' print(object)
#'
#' ## RNAi (DEMETER2).
#' object <- DepMapRnaiGeneEffect()
#' print(object)
NULL



#' @rdname DepMapGeneEffect
#' @export
DepMapCrisprGeneEffect <- function() {
    .importBroadGeneEffect(
        dataset = .currentDataset,
        class = "DepMapCrisprGeneEffect"
    )
}



#' @rdname DepMapGeneEffect
#' @export
DepMapRnaiGeneEffect <- function() {
    .importBroadGeneEffect(
        dataset = "demeter2_data_v6",
        class = "DepMapRnaiGeneEffect"
    )
}
