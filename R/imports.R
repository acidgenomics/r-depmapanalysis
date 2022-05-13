## S4 classes ==================================================================

#' @importClassesFrom S4Vectors DataFrame
#' @importClassesFrom SummarizedExperiment SummarizedExperiment
NULL



## S4 generics and methods =====================================================

#' @importFrom AcidGenerics camelCase encode leftJoin makeDimnames
#'   mapGenesToRownames melt showHeader
#' @importFrom BiocGenerics do.call lapply mean order rbind t
#' @importFrom IRanges median
#' @importFrom S4Vectors complete.cases decode head mcols metadata metadata<-
#'   split
#' @importFrom SummarizedExperiment assay assayNames colData rowData
#' @importFrom methods show
#' @importFrom pipette import
#'
#' @importMethodsFrom AcidBase showHeader
#' @importMethodsFrom AcidExperiment mapGenesToRownames melt
#' @importMethodsFrom AcidPlyr leftJoin melt
#' @importMethodsFrom pipette import
#' @importMethodsFrom syntactic camelCase makeDimnames
NULL



## S3 generics =================================================================

#' @importFrom stats reorder
NULL



## Standard functions ==========================================================

#' @importFrom AcidBase pasteURL showSlotInfo
#' @importFrom AcidCLI abort alert alertInfo alertWarning dl h1 toInlineString
#' @importFrom AcidExperiment makeSummarizedExperiment
#' @importFrom AcidGenomes EntrezGeneInfo geneSynonyms
#' @importFrom AcidPlots !! acid_geom_label_repel sym
#' @importFrom IRanges CharacterList
#' @importFrom S4Vectors DataFrame
#' @importFrom ggplot2 aes facet_wrap geom_boxplot geom_density geom_jitter
#'   geom_point geom_violin geom_vline ggplot labs scale_y_discrete
#' @importFrom goalie allAreURLs areIntersectingSets assert bapply hasDimnames
#'   hasLength hasRownames isAFile isAURL isCharacter isFlag isInt isInstalled
#'   isPositive isScalar isString isSubset validate validateClasses
#' @importFrom methods as is new setClass setValidity validObject
#' @importFrom pipette cacheURL
#' @importFrom stringi stri_match_first_regex
#' @importFrom utils capture.output packageName packageVersion
NULL
