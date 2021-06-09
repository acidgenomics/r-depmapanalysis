#' DepMapAnalysis
#'
#' Cancer Dependency Map (DepMap) analysis toolkit.
#'
#' @keywords internal

#' @importClassesFrom basejump DataFrame SummarizedExperiment
#'
#' @importFrom basejump EntrezGeneInfo alert alertWarning assay assayNames
#'   as_tibble cacheURL camelCase colData complete.cases decode do.call
#'   geneSynonyms import lapply makeDimnames makeSummarizedExperiment
#'   mapGenesToRownames mcols melt metadata metadata<- packageName
#'   packageVersion pasteURL reorder rbind rowData snakeCase str_match t
#' @importFrom ggplot2 aes coord_flip facet_wrap geom_boxplot geom_density
#'   geom_hline geom_vline ggplot labs scale_x_discrete
#' @importFrom goalie areIntersectingSets assert hasDimnames hasLength
#'   hasRownames isAFile isAURL isCharacter isFlag isScalar isString isSubset
#'   validate validateClasses
#' @importFrom methods as is new setClass setValidity validObject
#' @importFrom rlang !! sym
"_PACKAGE"
