#' Import Achilles gene dependency data
#'
#' @export
#' @note Updated 2020-09-30.
#'
#' @return `matrix`.
#'
#' @examples
#' mat <- importAchillesGeneDependencyData()
#' dim(mat)
importAchillesGeneDependencyData <- function(release = NULL) {
    .importDataFile(
        fileName = "achilles_gene_dependency.csv",
        type = "genetic_dependency",
        release = release,
        rownamesCol = 1L,
        return = "matrix"
    )
}
