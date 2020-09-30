#' Import Achilles gene effect data
#'
#' @export
#' @note Updated 2020-09-30.
#'
#' @return `matrix`.
#'
#' @examples
#' mat <- importAchillesGeneEffectData()
#' dim(mat)
importAchillesGeneEffectData <- function(release = NULL) {
    .importDataFile(
        fileName = "achilles_gene_effect.csv",
        type = "genetic_dependency",
        release = release,
        rownamesCol = 1L,
        return = "matrix"
    )
}
