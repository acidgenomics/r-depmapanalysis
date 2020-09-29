## https://depmap.org/portal/download/
## https://www.bioconductor.org/packages/release/bioc/vignettes/BiocFileCache/inst/doc/BiocFileCache.html

##path <- tempfile()
##bfc <- BiocFileCache(path, ask = FALSE)

##bfccache(bfc)
##length(bfc)
##show(bfc)
##bfcinfo(bfc)



#' Popular (starred) DepMap file downloads
#'
#' @note Updated 2020-09-29.
#' @noRd
#'
#' @seealso https://depmap.org/portal/download/
.downloads <- list(
    "url_stem" = "https://ndownloader.figshare.com/files/",
    "depmap_releases" = list(
        "20q3" = list(
            "cellular_models" = list(
                "ccle_expression.csv" = "24613325",      # Expression
                "ccle_gene_cn.csv" = "24613352",         # Copy number
                "ccle_mutations.csv" = "24613355",       # Mutation
                "sample_info.csv" = "24613394"           # Cell line sample info
            ),
            "genetic_dependency" = list(
                "achilles_gene_dependency.csv" = "24613298",
                "achilles_gene_effect.csv" = "24613292"
            )
        )
    ),
    "rnai_screens" = list(
        "demeter2_data_v6" = list(
            "genetic_dependency" = list(
                ## > "d2_achilles_gene_dep_scores.csv" = "11489669",
                ## > "d2_drive_gene_dep_scores.csv" = "11489693",
                "d2_combined_gene_dep_scores.csv" = "13515395"
            )
        )
    )
)



#' Get cache directory
#'
#' @note Updated 2020-09-29.
#' @noRd
.getCache <- function() {
    cache <- user_cache_dir(appname = "DepMapAnalysis")
    bfc <- BiocFileCache(cache)
    bfc
}



#' Download a data file into BiocFileCache
#'
#' @note Updated 2020-09-29.
#' @noRd
.downloadDataFile <- function( verbose = FALSE ) {
    fileURL <- "http://a_path_to/someremotefile.tsv.gz"
    bfc <- .getCache()
    rid <- bfcquery(bfc, "geneFileV2", "rname")$rid
    if (!length(rid)) {
        if (verbose) {
            message( "Downloading GENE file" )
        }
        rid <- names(bfcadd(bfc, "geneFileV2", fileURL ))
    }
    if (!isFALSE(bfcneedsupdate(bfc, rid)))
        bfcdownload(bfc, rid)
    bfcrpath(bfc, rids = rid)
}


