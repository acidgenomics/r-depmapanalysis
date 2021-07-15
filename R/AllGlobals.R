.pkgName <- packageName()
.pkgVersion <- packageVersion(.pkgName)



## FIXME Save this as an object and load from package instead of parsing
## the YAML manually here...

#' DepMap dataset URLs and other metadata in YAML format
#'
#' @note Updated 2021-07-07.
#' @noRd
.datasets <- import(
    file = system.file(
        "extdata",
        "datasets.yaml",
        package = .pkgName,
        mustWork = TRUE
    ),
    quiet = TRUE
)



.formalsList <- list(
    "dataset" = names(.datasets),
    "depmapDataset" = grep(
        pattern = "^depmap_",
        x = names(.datasets),
        value = TRUE
    )
)
