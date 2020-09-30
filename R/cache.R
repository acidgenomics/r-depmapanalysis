

##path <- tempfile()
##bfc <- BiocFileCache(path, ask = FALSE)

##bfccache(bfc)
##length(bfc)
##show(bfc)
##bfcinfo(bfc)



#' Prepare BiocFileCache for package
#'
#' @note Updated 2020-09-30.
#' @noRd
#'
#' @seealso
#' https://www.bioconductor.org/packages/release/bioc/vignettes/
#'     BiocFileCache/inst/doc/BiocFileCache.html
.bfc <- function(pkg = packageName()) {
    BiocFileCache(
        cache = user_cache_dir(appname = pkg),
        ask = TRUE
    )
}



#' Popular (starred) DepMap file downloads
#'
#' @note Updated 2020-09-29.
#' @noRd
#'
#' @seealso https://depmap.org/portal/download/
.depmap <- list(
    "url_stem" = "https://ndownloader.figshare.com/files/",
    ## CRISPR screens.
    "20q3" = list(
        "cellular_models" = list(
            "ccle_expression.csv" = "24613325",  # Expression
            "ccle_gene_cn.csv" = "24613352",     # Copy number
            "ccle_mutations.csv" = "24613355",   # Mutation
            "sample_info.csv" = "24613394"       # Cell line sample info
        ),
        "genetic_dependency" = list(
            "achilles_gene_dependency.csv" = "24613298",
            "achilles_gene_effect.csv" = "24613292"
        )
    ),
    ## RNAi screens.
    "demeter2_data_v6" = list(
        "genetic_dependency" = list(
            ## > "d2_achilles_gene_dep_scores.csv" = "11489669",
            ## > "d2_drive_gene_dep_scores.csv" = "11489693",
            "d2_combined_gene_dep_scores.csv" = "13515395"
        )
    )
)



#' Download a data file into BiocFileCache
#'
#' @note Updated 2020-09-30.
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
#' .downloadDataFile(fileID = fileID, fileName = fileName)
.downloadDataFile <- function(fileID, fileName, verbose = TRUE) {
    urlStem <- .depmap[["url_stem"]]
    assert(
        isAURL(urlStem),
        isFlag(verbose)
    )
    fileURL <- paste0(urlStem, fileID)
    bfc <- .bfc()
    rid <- bfcquery(
        x = bfc,
        query = fileName,
        field = "rname",
        exact = TRUE
    )[["rid"]]
    if (!hasLength(rid)) {
        if (isTRUE(verbose)) {
            cli_alert(sprintf(
                "Caching {.file %s} at {.path %s}.",
                fileName, bfccache(bfc)
            ))
        }
        rid <- names(bfcadd(
            x = bfc,
            rname = fileName,
            fpath = fileURL,
            download = TRUE
        ))
    }
    if (!isFALSE(bfcneedsupdate(x = bfc, rids = rid))) {
        bfcdownload(x = bfc, rid = rid, ask = FALSE)
    }
    out <- bfcrpath(x = bfc, rids = rid)
    assert(isAFile(out))
    out
}
