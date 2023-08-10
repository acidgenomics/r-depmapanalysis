#' Cache URL into package
#'
#' @note Updated 2023-03-08.
#' @noRd
.cacheURL <-
    function(url) {
        cacheURL(url = url, pkg = .pkgName)
    }
