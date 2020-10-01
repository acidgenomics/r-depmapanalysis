#' Achilles CRISPR screening data
#'
#' Scored using CERES algorithm.
#'
#' @note Updated 2020-10-01.
#' @export
#'
#' @return `Achilles`.
#'
#' @seealso
#' - https://depmap.org/portal/achilles/
#' - https://depmap.org/ceres/
setClass(
    Class = "Achilles",
    contains = "SummarizedExperiment"
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
#' @note Updated 2020-10-01.
#' @export
#'
#' @return `DEMETER2`.
setClass(
    Class = "DEMETER2",
    contains = "SummarizedExperiment"
)
