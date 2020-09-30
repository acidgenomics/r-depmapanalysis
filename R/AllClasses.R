#' Achilles gene dependency data
#'
#' @note Updated 2020-09-30.
#' @export
#'
#' @return `AchillesGeneDependencyData`.
setClass(
    Class = "AchillesGeneDependencyData",
    contains = "matrix"
)



#' Achilles gene effect data
#'
#' @note Updated 2020-09-30.
#' @export
#'
#' @return `AchillesGeneEffectData`.
setClass(
    Class = "AchillesGeneEffectData",
    contains = "matrix"
)



#' CCLE copy number data
#'
#' @note Updated 2020-09-30.
#' @export
#'
#' @return `CCLECopyNumberData`.
setClass(
    Class = "CCLECopyNumberData",
    contains = "DataFrame"
)



#' CCLE expression data
#'
#' @note Updated 2020-09-30.
#' @export
#'
#' @return `CCLEExpressionData`.
setClass(
    Class = "CCLEExpressionData",
    contains = "DataFrame"
)



#' CCLE mutation data
#'
#' @note Updated 2020-09-30.
#' @export
#'
#' @return `CCLEMutationData`.
setClass(
    Class = "CCLEMutationData",
    contains = "DataFrame"
)



#' Cell line sample metadata
#'
#' @note Updated 2020-09-30.
#' @export
#'
#' @return `CellLineSampleData`.
setClass(
    Class = "CellLineSampleData",
    contains = "DataFrame"
)



#' DEMETER2 RNAi gene effect data
#'
#' @note Updated 2020-09-30.
#' @export
#'
#' @return `DEMETER2GeneEffectData`.
setClass(
    Class = "DEMETER2GeneEffectData",
    contains = "matrix"
)
