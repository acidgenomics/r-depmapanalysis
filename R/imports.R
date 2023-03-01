## S4 classes ==================================================================

#' @importClassesFrom S4Vectors DFrame
#' @importClassesFrom SummarizedExperiment SummarizedExperiment
NULL



## S4 generics and methods =====================================================

#' @importFrom AcidGenerics autopadZeros camelCase droplevels2 encode factorize
#' leftJoin makeDimnames makeNames mapGenesToRownames melt showHeader
#' @importFrom BiocGenerics %in% do.call lapply mean order rbind t which
#' @importFrom IRanges median
#' @importFrom S4Vectors complete.cases decode head mcols metadata metadata<-
#' split unname
#' @importFrom SummarizedExperiment assay assayNames colData colData<- rowData
#' @importFrom methods show
#' @importFrom pipette import
NULL

#' @importMethodsFrom AcidBase showHeader
#' @importMethodsFrom AcidExperiment droplevels2 mapGenesToRownames melt
#' @importMethodsFrom AcidPlyr leftJoin melt
#' @importMethodsFrom pipette factorize import
#' @importMethodsFrom syntactic autopadZeros camelCase makeDimnames makeNames
NULL



## S3 generics =================================================================

#' @importFrom stats reorder
NULL



## Standard functions ==========================================================

#' @importFrom AcidBase pasteURL showSlotInfo
#' @importFrom AcidCLI abort alert alertInfo alertWarning dl h1 toInlineString
#' @importFrom AcidExperiment makeSummarizedExperiment
#' @importFrom AcidGenomes EntrezGeneInfo HGNC emptyRanges geneSynonyms
#' @importFrom AcidPlots !! acid_geom_label_repel sym
#' @importFrom IRanges CharacterList
#' @importFrom S4Vectors DataFrame
#' @importFrom ggplot2 aes facet_wrap geom_boxplot geom_density geom_jitter
#' geom_point geom_violin geom_vline ggplot labs scale_y_discrete
#' @importFrom goalie allAreMatchingRegex allAreURLs areIntersectingSets
#' areSetEqual assert bapply hasColnames hasDuplicates hasLength hasNoDuplicates
#' hasRownames isAFile isAURL isCharacter isFlag isInt isInstalled isPositive
#' isScalar isString isSubset requireNamespaces validate validateClasses
#' @importFrom methods as is new setClass setValidity validObject
#' @importFrom pipette cacheURL
#' @importFrom stringi stri_match_first_regex
#' @importFrom utils capture.output packageName packageVersion
NULL
