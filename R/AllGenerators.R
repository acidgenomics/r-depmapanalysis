#' Import Achilles gene dependency data
#'
#' @export
#' @note Updated 2020-09-30.
#'
#' @return `AchillesGeneDependencyData`.
#'
#' @examples
#' object <- AchillesGeneDependencyData()
#' dim(object)
AchillesGeneDependencyData <-  # nolint
    function(release = NULL) {
        mat <- .importDataFile(
            fileName = "achilles_gene_dependency.csv",
            type = "genetic_dependency",
            release = release,
            rownamesCol = 1L,
            return = "matrix"
        )
        assert(is.matrix(mat))
        new("AchillesGeneDependencyData", mat)
    }



#' Import Achilles gene effect data
#'
#' @export
#' @note Updated 2020-09-30.
#'
#' @return `AchillesGeneEffectData`.
#'
#' @examples
#' object <- AchillesGeneEffectData()
#' dim(object)
AchillesGeneEffectData <- function(release = NULL) {
    mat <- .importDataFile(
        fileName = "achilles_gene_effect.csv",
        type = "genetic_dependency",
        release = release,
        rownamesCol = 1L,
        return = "matrix"
    )
    assert(is.matrix(mat))
    new("AchillesGeneEffectData", mat)
}



#' Import cell line sample metadata
#'
#' @export
#' @note Updated 2020-09-30.
#'
#' @inheritParams params
#'
#' @return `DataFrame`.
#'
#' @examples
#' object <- CellLineSampleData()
#' dim(object)
CellLineSampleData <- function(release = NULL) {
    .importDataFile(
        fileName = "sample_info.csv",
        type = "cellular_models",
        release = release,
        rownamesCol = 1L
    )
}
