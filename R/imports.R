#' @importClassesFrom S4Vectors DataFrame
#' @importClassesFrom SummarizedExperiment SummarizedExperiment
#'
#' @importFrom BiocFileCache BiocFileCache bfcadd bfccache bfcdownload
#'   bfcneedsupdate bfcquery bfcrpath
#' @importFrom S4Vectors complete.cases mcols
#' @importFrom basejump Ensembl2Entrez as_tibble camelCase decode geneSynonyms
#'   import makeDimnames makeGRangesFromEnsembl makeSummarizedExperiment
#'   mapGenesToRownames melt snakeCase
#' @importFrom cli cli_alert cli_alert_warning cli_dl
#' @importFrom ggplot2 aes coord_flip facet_wrap geom_boxplot geom_density
#'   geom_hline geom_vline ggplot labs scale_x_discrete
#' @importFrom goalie assert hasLength hasRownames isAFile isAURL isCharacter
#'   isFlag isScalar isString isSubset
#' @importFrom methods as is new setClass setValidity
#' @importFrom rappdirs user_cache_dir
#' @importFrom rlang !! sym
#' @importFrom stats reorder
#' @importFrom stringr str_extract
#' @importFrom utils packageName
NULL
