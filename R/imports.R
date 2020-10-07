#' @importClassesFrom S4Vectors DataFrame
#' @importClassesFrom SummarizedExperiment SummarizedExperiment
#'
#' @importFrom S4Vectors complete.cases mcols
#' @importFrom SummarizedExperiment assay
#' @importFrom basejump Ensembl2Entrez as_tibble cacheURL camelCase decode
#'   geneSynonyms import makeDimnames makeGRangesFromEnsembl
#'   makeSummarizedExperiment mapGenesToRownames melt snakeCase
#' @importFrom cli cli_alert cli_alert_warning cli_dl
#' @importFrom ggplot2 aes coord_flip facet_wrap geom_boxplot geom_density
#'   geom_hline geom_vline ggplot labs scale_x_discrete
#' @importFrom goalie assert hasDimnames hasLength hasRownames isAFile isAURL
#'   isCharacter isFlag isScalar isString isSubset validate validateClasses
#' @importFrom methods as is new setClass setValidity validObject
#' @importFrom rlang !! sym
#' @importFrom stats reorder
#' @importFrom stringr str_extract
#' @importFrom utils packageName
NULL
