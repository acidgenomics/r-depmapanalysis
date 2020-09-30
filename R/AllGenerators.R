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



#' Import CCLE copy number data
#'
#' @export
#' @note Updated 2020-09-30.
#'
#' @return `CCLECopyNumberData`.
#'
#' @examples
#' object <- CCLECopyNumberData()
#' dim(object)
CCLECopyNumberData <- function(release = NULL) {
    df <- .importDataFile(
        fileName = "ccle_gene_cn.csv",
        type = "cellular_models",
        release = release,
        rownamesCol = 1L
    )
    assert(is(df, "DataFrame"))
    new("CCLECopyNumberData", df)
}



#' Import CCLE expression data
#'
#' @export
#' @note Updated 2020-09-30.
#'
#' @inheritParams params
#'
#' @return `CCLEExpressionData`.
#'
#' @examples
#' object <- CCLEExpressionData()
#' dim(object)
CCLEExpressionData <- function(release = NULL) {
    df <- .importDataFile(
        fileName = "ccle_expression.csv",
        type = "cellular_models",
        release = release,
        rownamesCol = 1L
    )
    assert(is(df, "DataFrame"))
    new("CCLEExpressionData", df)
}



#' Import CCLE mutation data
#'
#' @export
#' @note Updated 2020-09-30.
#'
#' @inheritParams params
#'
#' @return `CCLEMutationData`.
#'
#' @examples
#' object <- CCLEMutationData()
#' dim(object)
CCLEMutationData <- function(release = NULL) {
    df <- .importDataFile(
        fileName = "ccle_mutations.csv",
        type = "cellular_models",
        format = "tsv",
        release = release,
        rownamesCol = NULL
    )
    assert(is(df, "DataFrame"))
    new("CCLEMutationData", df)
}



#' Import cell line sample metadata
#'
#' @export
#' @note Updated 2020-09-30.
#'
#' @inheritParams params
#'
#' @return `CellLineSampleData`.
#'
#' @examples
#' object <- CellLineSampleData()
#' dim(object)
CellLineSampleData <- function(release = NULL) {
    df <- .importDataFile(
        fileName = "sample_info.csv",
        type = "cellular_models",
        release = release,
        rownamesCol = 1L
    )
    assert(is(df, "DataFrame"))
    new("CellLineSampleData", df)
}



#' Import DEMETER2 RNAi screen gene effect data
#'
#' @export
#' @note Updated 2020-09-30.
#'
#' @return `DEMETER2GeneEffectData`.
#'
#' @examples
#' object <- DEMETER2GeneEffectData()
#' dim(object)
DEMETER2GeneEffectData <- function() {
    mat <- .importDataFile(
        fileName = "d2_combined_gene_dep_scores.csv",
        type = "genetic_dependency",
        release = "demeter2_data_v6",
        rownamesCol = 1L,
        return = "matrix"
    )
    assert(is.matrix(mat))
    new("DEMETER2GeneEffectData", mat)
}
