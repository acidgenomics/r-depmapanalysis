#' Cache URL into package
#'
#' @note Updated 2023-03-08.
#' @noRd
.cacheUrl <-
    function(url) {
        cacheUrl(url = url, pkg = .pkgName)
    }
