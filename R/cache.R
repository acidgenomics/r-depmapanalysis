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
