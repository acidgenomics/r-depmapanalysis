#' DepMapAnalysis
#'
#' Cancer Dependency Map (DepMap) analysis toolkit.
#'
#' @aliases NULL
#' @keywords internal

#' @importClassesFrom basejump DataFrame SummarizedExperiment
#'
#' @importFrom basejump CharacterList EntrezGeneInfo alert alertInfo
#'   alertWarning assay assayNames as_tibble cacheURL camelCase capture.output
#'   colData complete.cases decode do.call encode geneSynonyms head import
#'   lapply makeDimnames makeSummarizedExperiment mapGenesToRownames mcols mean
#'   melt metadata metadata<- order packageName packageVersion pasteURL reorder
#'   rbind rowData showHeader showSlotInfo split str_match t
#' @importFrom ggplot2 aes coord_flip facet_wrap geom_boxplot geom_density
#'   geom_hline geom_jitter geom_point geom_vline ggplot labs scale_x_discrete
#'   scale_y_discrete
#' @importFrom goalie areIntersectingSets assert bapply hasDimnames hasLength
#'   hasRownames isAFile isAURL isCharacter isFlag isInt isPositive isScalar
#'   isString isSubset validate validateClasses
#' @importFrom methods as is new setClass setValidity show validObject
#' @importFrom rlang !! sym
#' @importFrom tidytext reorder_within scale_y_reordered
"_PACKAGE"



## FIXME basejump needs to reexport median generic, so it works with IRanges.
#' @importFrom IRanges median
NULL
