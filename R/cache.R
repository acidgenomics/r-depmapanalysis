#' Download and cache a data file from DepMap into BiocFileCache
#'
#' @note Updated 2020-10-07.
#' @noRd
#'
#' @param fileID `character(1)`.
#'   DepMap file ID on figshare.com.
#' @param fileName `character(1)`.
#'   File name.
#'
#' @return `character(1)`.
#'   Cached file path on disk.
#'
#' @examples
#' fileName <- "sample_info.csv"
#' fileID <- .depmap[["20q3"]][["cellular_models"]][[fileName]]
#' .cacheDataFile(fileName = fileName, fileID = fileID)
.cacheDataFile <- function(fileName, fileID, verbose = TRUE) {
    urlStem <- .depmap[["url_stem"]]
    assert(
        isAURL(urlStem),
        isFlag(verbose)
    )
    url <- paste0(urlStem, fileID)
    file <- cacheURL(
        url = url,
        fileName = fileName,
        pkg = packageName(),
        verbose = verbose
    )
    assert(isAFile(file))
    file
}



#' Popular (starred) DepMap file downloads
#'
#' @note Updated 2020-10-01.
#' @noRd
#'
#' @seealso https://depmap.org/portal/download/
.depmap <- list(
    "url_stem" = "https://ndownloader.figshare.com/files/",
    ## CRISPR screens.
    "20q3" = list(
        ## List of genes identified as dependencies in all lines, one per line.
        "achilles_common_essentials.csv" = "24613283",
        "achilles_gene_dependency.csv" = "24613298",
        "achilles_gene_effect.csv" = "24613292",
        "ccle_expression.csv" = "24613325",
        "ccle_gene_cn.csv" = "24613352",
        "ccle_mutations.csv" = "24613355",
        ## List of genes used as positive controls, intersection of Biomen
        ## (2014) and Hart (2015) essentials in the format "HUGO (Entrez)". Each
        ## entry is separated by a newline.The scores of these genes are used as
        ## the dependent distribution for inferring dependency probability.
        "common_essentials.csv" = "24613385",
        ## List of genes used as negative controls (Hart (2014) nonessentials)
        ## in the format "HUGO (Entrez)". Each entry is separated by a newline.
        "nonessentials.csv" = "24613388",
        "sample_info.csv" = "24613394"
    ),
    ## RNAi screens.
    "demeter2_data_v6" = list(
        "d2_combined_gene_dep_scores.csv" = "13515395",
        "sample_info.csv" = "11489717"
    )
)



#' Import cell line sample metadata
#'
#' @note Updated 2020-10-01.
#' @noRd
.importCellLineSampleData <-  # nolint
    function(release) {
        df <- .importDataFile(
            fileName = "sample_info.csv",
            release = release,
            rownamesCol = 1L
        )
        assert(is(df, "DataFrame"))
        df <- snakeCase(df)
        df
    }



#' Import DepMap data file
#'
#' @note Updated 2020-09-30.
#' @noRd
.importDataFile <- function(
    fileName,
    release,
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
    format <- match.arg(format)
    return <- match.arg(return)
    cli_alert(sprintf(
        "Importing {.file %s} from DepMap {.var %s} release.",
        fileName, release
    ))
    fileID <- .depmap[[tolower(release)]][[fileName]]
    file <- .cacheDataFile(fileName = fileName, fileID = fileID)
    df <- import(file = file, format = format)
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
    df <- makeDimnames(df)
    df
}
