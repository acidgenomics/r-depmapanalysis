#' Import DepMap data file
#'
#' @note Updated 2020-09-30.
#' @noRd
.importDataFile <- function(
    fileName,
    release,
    type = c("cellular_models", "genetic_dependency"),
    format = c("csv", "tsv"),
    rownamesCol = NULL
) {
    if (is.null(release)) {
        release <- .currentRelease
    }
    assert(
        isString(fileName),
        isString(fileID),
        isString(release),
        isScalar(rownamesCol) || is.null(rownamesCol)
    )
    type <- match.arg(type)
    format <- match.arg(format)
    cli_alert(sprintf(
        "Importing {.file %s} from DepMap {.var %s} release.",
        fileName, release
    ))
    fileID <- .depmap[[tolower(release)]][["cellular_models"]][[fileName]]
    file <- .cacheDataFile(fileName = fileName, fileID = fileID)
    suppressMessages({
        df <- import(file = file, format = format)
    })
    df <- snakeCase(df)
    if (isScalar(rownamesCol)) {
        if (!isString(rownamesCol)) {
            rownamesCol <- colnames(df)[[rownamesCol]]
        }
        assert(isSubset(rownamesCol, colnames(df)))
        rownames(df) <- df[[rownamesCol]]
        df <- makeDimnames(df)
    }
    df <- as(df, "DataFrame")
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



## FIXME THIS IS A TSV, NOT CSV.

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
        format = "tsv",
        release = release,
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
# "genetic_dependency" = list(
#     "achilles_gene_dependency.csv" = "24613298",
#     "achilles_gene_effect.csv" = "24613292"
# )
# ),
# ## RNAi screens.
# "demeter2_data_v6" = list(
#     "genetic_dependency" = list(
#         ## > "d2_achilles_gene_dep_scores.csv" = "11489669",
#         ## > "d2_drive_gene_dep_scores.csv" = "11489693",
#         "d2_combined_gene_dep_scores.csv" = "13515395"
#     )
# )
