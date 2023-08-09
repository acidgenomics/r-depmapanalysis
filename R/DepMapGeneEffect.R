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
#' object <- DepMapCRISPRGeneEffect()
#' print(object)
#'
#' ## RNAi (DEMETER2).
#' object <- DepMapRNAiGeneEffect()
#' print(object)
NULL



#' @rdname DepMapGeneEffect
#' @export
DepMapCRISPRGeneEffect <- function() {
    .importBroadGeneEffect(
        dataset = .currentDataset,
        class = "DepMapCRISPRGeneEffect"
    )
}



#' @rdname DepMapGeneEffect
#' @export
DepMapRNAiGeneEffect <- function() {
    .importBroadGeneEffect(
        dataset = "demeter2_data_v6",
        class = "DepMapRNAiGeneEffect"
    )
}
