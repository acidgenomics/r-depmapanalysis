#' DepMapAnalysis
#'
#' Cancer Dependency Map (DepMap) analysis toolkit.
#'
#' @keywords internal

#' @importClassesFrom basejump DataFrame RangedSummarizedExperiment
#'   SummarizedExperiment
#'
#' @importFrom basejump Ensembl2Entrez alert alertWarning as_tibble assay
#'   cacheURL camelCase complete.cases decode do.call geneSynonyms import lapply
#'   makeDimnames makeGRangesFromEnsembl makeSummarizedExperiment
#'   mapGenesToRownames mcols melt metadata packageName packageVersion reorder
#'   snakeCase t
#' @importFrom ggplot2 aes coord_flip facet_wrap geom_boxplot geom_density
#'   geom_hline geom_vline ggplot labs scale_x_discrete
#' @importFrom goalie assert hasDimnames hasLength hasRownames isAFile isAURL
#'   isCharacter isFlag isScalar isString isSubset validate validateClasses
#' @importFrom methods as is new setClass setValidity validObject
#' @importFrom rlang !! sym
#' @importFrom stringr str_extract
"_PACKAGE"
