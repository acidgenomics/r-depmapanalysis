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




## CCLECopyNumberData
## CCLEExpressionData
## CCLEMutationData



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
