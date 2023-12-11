#' DepMapAnalysis
#'
#' Cancer Dependency Map (DepMap) analysis toolkit.
#'
#' @aliases NULL
#' @keywords internal
"_PACKAGE"



## S4 classes ==================================================================

#' @importClassesFrom S4Vectors DFrame
#' @importClassesFrom SummarizedExperiment RangedSummarizedExperiment
#' SummarizedExperiment
NULL



## S4 generics and methods =====================================================

#' @importFrom AcidGenerics as.DataFrame autopadZeros camelCase diffExp
#' droplevels2 encode excludeContaminatedCells import leftJoin makeDimnames
#' makeNames mapGenesToRownames melt predictSensitivity plotGeneEffect
#' plotGeneEffectVsExpression plotTopGeneEffectPerCell plotTopGeneEffectPerGroup
#' selectCells showHeader tpm zscore
#' @importFrom BiocGenerics %in% do.call lapply mean order rbind t which
#' @importFrom IRanges gsub median sub
#' @importFrom S4Vectors I complete.cases decode head mcols metadata metadata<-
#' split unname
#' @importFrom SummarizedExperiment assay assayNames colData colData<- rowData
#' rowRanges
#' @importFrom methods show
NULL

#' @importMethodsFrom AcidBase showHeader
#' @importMethodsFrom AcidExperiment droplevels2 mapGenesToRownames melt
#' @importMethodsFrom AcidPlyr leftJoin melt
#' @importMethodsFrom pipette import
#' @importMethodsFrom syntactic autopadZeros camelCase makeDimnames makeNames
NULL



## S3 generics =================================================================

#' @importFrom stats reorder
NULL



## Standard functions ==========================================================

#' @importFrom AcidBase pasteUrl showSlotInfo strMatch
#' @importFrom AcidCLI abort alert alertInfo alertWarning dl h1 toInlineString
#' @importFrom AcidExperiment makeSummarizedExperiment
#' @importFrom AcidGenomes Hgnc NcbiGeneInfo emptyRanges makeGRangesFromEnsembl
#' @importFrom AcidPlots .data acid_geom_label_repel
#' @importFrom Cellosaurus Cellosaurus
#' @importFrom IRanges CharacterList
#' @importFrom S4Vectors DataFrame
#' @importFrom ggplot2 aes facet_wrap geom_boxplot geom_density geom_jitter
#' geom_point geom_violin geom_vline ggplot labs scale_y_discrete
#' @importFrom goalie allAreMatchingFixed allAreMatchingRegex allAreUrls
#' areDisjointSets areIntersectingSets areSetEqual assert bapply hasColnames
#' hasDuplicates hasLength hasNoDuplicates hasRam hasRownames isAFile isAUrl
#' isCharacter isFlag isInt isInstalled isPositive isScalar isScalarNumeric
#' isString isSubset requireNamespaces validate validateClasses
#' @importFrom matrixStats colSums2 rowMaxs rowMins
#' @importFrom methods as is new setClass setValidity validObject
#' @importFrom pipette cacheUrl
#' @importFrom utils capture.output packageName packageVersion
NULL
