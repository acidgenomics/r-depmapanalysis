#' DepMapAnalysis
#'
#' Cancer Dependency Map (DepMap) analysis toolkit.
#'
#' @aliases NULL
#' @keywords internal

#' @importClassesFrom basejump DataFrame SummarizedExperiment
#'
#' @importFrom basejump CharacterList DataFrame EntrezGeneInfo alert alertInfo
#'   alertWarning assay assayNames as_tibble cacheURL camelCase capture.output
#'   colData complete.cases decode dl do.call encode geneSynonyms h1 head import
#'   lapply leftJoin makeDimnames makeSummarizedExperiment mapGenesToRownames
#'   mcols mean median melt metadata metadata<- order packageName packageVersion
#'   pasteURL reorder rbind rowData showHeader showSlotInfo split str_match t
#' @importFrom ggplot2 aes facet_wrap geom_boxplot geom_density geom_jitter
#'   geom_point geom_violin geom_vline ggplot labs scale_y_discrete
#' @importFrom goalie allAreURLs areIntersectingSets assert bapply hasDimnames
#'   hasLength hasRownames isAFile isAURL isCharacter isFlag isInt isPositive
#'   isScalar isString isSubset validate validateClasses
#' @importFrom methods as is new setClass setValidity show validObject
#' @importFrom rlang !! sym
#' @importFrom tidytext reorder_within scale_y_reordered
"_PACKAGE"



#' @name params
#' @inherit AcidRoxygen::params return title
#' @keywords internal
#'
#' @param dataset `character(1)` or `NULL`.
#'   DepMap dataset release name.
#'   If left `NULL`, defaults to latest supported dataset.
NULL
