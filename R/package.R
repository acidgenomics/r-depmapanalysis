#' DepMapAnalysis
#'
#' Cancer Dependency Map (DepMap) analysis toolkit.
#'
#' @aliases NULL
#' @keywords internal

#' @importClassesFrom basejump DataFrame SummarizedExperiment
#'
#' @importFrom basejump EntrezGeneInfo alert alertInfo alertWarning assay
#'   assayNames as_tibble cacheURL camelCase capture.output colData
#'   complete.cases decode do.call encode geneSynonyms import lapply
#'   makeDimnames makeSummarizedExperiment mapGenesToRownames mcols melt
#'   metadata metadata<- packageName packageVersion pasteURL reorder rbind
#'   rowData showHeader showSlotInfo snakeCase str_match t
#' @importFrom ggplot2 aes coord_flip facet_wrap geom_boxplot geom_density
#'   geom_hline geom_vline ggplot labs scale_x_discrete
#' @importFrom goalie areIntersectingSets assert hasDimnames hasLength
#'   hasRownames isAFile isAURL isCharacter isFlag isInt isScalar isString
#'   isSubset validate validateClasses
#' @importFrom methods as is new setClass setValidity show validObject
#' @importFrom rlang !! sym
"_PACKAGE"
