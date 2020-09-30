#' Import DepMap data file
#'
#' @note Updated 2020-09-30.
#' @noRd
.importDataFile <- function(
    fileName,
    release,
    type = c("cellular_models", "genetic_dependency"),
    format = c("csv", "tsv"),
    rownamesCol = NULL,
    return = c("DataFrame", "matrix")
) {
    if (is.null(release)) {
        release <- .currentRelease
    }
    assert(
        isString(fileName),
        isString(release),
        isScalar(rownamesCol) || is.null(rownamesCol)
    )
    type <- match.arg(type)
    format <- match.arg(format)
    return <- match.arg(return)
    cli_alert(sprintf(
        "Importing {.file %s} from DepMap {.var %s} release.",
        fileName, release
    ))
    fileID <- .depmap[[tolower(release)]][[type]][[fileName]]
    file <- .cacheDataFile(fileName = fileName, fileID = fileID)
    suppressMessages({
        df <- import(file = file, format = format)
    })
    if (isScalar(rownamesCol)) {
        if (!isString(rownamesCol)) {
            rownamesCol <- colnames(df)[[rownamesCol]]
        }
        assert(isSubset(rownamesCol, colnames(df)))
        rownames(df) <- df[[rownamesCol]]
    }
    df <- switch(
        EXPR = return,
        "DataFrame" = as(df, "DataFrame"),
        "matrix" = {
            if (hasRownames(df)) df[[rownamesCol]] <- NULL
            as.matrix(df)
        }
    )
    df <- snakeCase(df, rownames = TRUE, colnames = TRUE)
    df
}



## Cellular models files =======================================================
#' Import CCLE copy number data
#'
#' @export
#' @note Updated 2020-09-30.
#'
#' @return `DataFrame`.
#'
#' @examples
#' df <- importCCLECopyNumberData()
importCCLECopyNumberData <- function(release = NULL) {
    df <- .importDataFile(
        fileName = "ccle_gene_cn.csv",
        type = "cellular_models",
        release = release,
        rownamesCol = 1L
    )
}



#' Import CCLE expression data
#'
#' @export
#' @note Updated 2020-09-30.
#'
#' @inheritParams params
#'
#' @return `DataFrame`.
#'
#' @examples
#' df <- importCCLEExpressionData()
importCCLEExpressionData <- function(release = NULL) {
    .importDataFile(
        fileName = "ccle_expression.csv",
        type = "cellular_models",
        release = release,
        rownamesCol = 1L
    )
}



#' Import CCLE mutation data
#'
#' @export
#' @note This file is large and takes a while to import.
#' @note Updated 2020-09-30.
#'
#' @inheritParams params
#'
#' @return `DataFrame`.
#'
#' @examples
#' ## This file is large and takes a while to import.
#' df <- importCCLEMutationData()
importCCLEMutationData <- function(release = NULL) {
    .importDataFile(
        fileName = "ccle_mutations.csv",
        type = "cellular_models",
        ## Note that this is a TSV, even though extension is CSV!
        format = "tsv",
        release = release,
        ## Consider returning as SplitDataFrame or grouped tibble.
        rownamesCol = NULL
    )
}



#' Import cell line sample info
#'
#' @export
#' @note Updated 2020-09-30.
#'
#' @inheritParams params
#'
#' @return `DataFrame`.
#'
#' @examples
#' df <- importCellLineSampleInfo()
importCellLineSampleInfo <- function(release = NULL) {
    .importDataFile(
        fileName = "sample_info.csv",
        type = "cellular_models",
        release = release,
        rownamesCol = 1L
    )
}



## Genetic dependency files ====================================================
#' Import Achilles gene dependency data
#'
#' @export
#' @note Updated 2020-09-30.
#'
#' @return `matrix`.
#'
#' @examples
#' mat <- importAchillesGeneDependencyData()
importAchillesGeneDependencyData <- function(release = NULL) {
    .importDataFile(
        fileName = "achilles_gene_dependency.csv",
        type = "genetic_dependency",
        release = release,
        rownamesCol = 1L,
        return = "matrix"
    )
}



#' Import Achilles gene effect data
#'
#' @export
#' @note Updated 2020-09-30.
#'
#' @return `matrix`.
#'
#' @examples
#' mat <- importAchillesGeneEffectData()
importAchillesGeneEffectData <- function(release = NULL) {
    .importDataFile(
        fileName = "achilles_gene_effect.csv",
        type = "genetic_dependency",
        release = release,
        rownamesCol = 1L,
        return = "matrix"
    )
}



#' Import DEMETER2 RNAi screen gene effect data
#'
#' @export
#' @note Updated 2020-09-30.
#'
#' @return `matrix`.
#'
#' @examples
#' mat <- importDEMETER2GeneEffectData()
importDEMETER2GeneEffectData <- function() {
    .importDataFile(
        fileName = "d2_combined_gene_dep_scores.csv",
        type = "genetic_dependency",
        release = "demeter2_data_v6",
        rownamesCol = 1L,
        return = "matrix"
    )
}
