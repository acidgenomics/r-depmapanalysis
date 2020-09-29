## https://depmap.org/portal/download/
## https://www.bioconductor.org/packages/release/bioc/vignettes/BiocFileCache/inst/doc/BiocFileCache.html

##path <- tempfile()
##bfc <- BiocFileCache(path, ask = FALSE)

##bfccache(bfc)
##length(bfc)
##show(bfc)
##bfcinfo(bfc)

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
downloadDataFile <- function( verbose = FALSE ) {
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


