#' Import DEMETER2 RNAi screen gene effect data
#'
#' @export
#' @note Updated 2020-09-30.
#'
#' @return `matrix`.
#'
#' @examples
#' mat <- importDEMETER2GeneEffectData()
#' dim(mat)
importDEMETER2GeneEffectData <- function() {
    .importDataFile(
        fileName = "d2_combined_gene_dep_scores.csv",
        type = "genetic_dependency",
        release = "demeter2_data_v6",
        rownamesCol = 1L,
        return = "matrix"
    )
}
