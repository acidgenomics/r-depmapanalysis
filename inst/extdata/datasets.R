## nolint start
suppressPackageStartupMessages({
    library(devtools)
    library(usethis)
})
## nolint end
load_all()
datasets <- import(
    file = system.file(
        "extdata",
        "datasets.yaml",
        package = .pkgName,
        mustWork = TRUE
    ),
    quiet = TRUE
)
use_data(datasets, overwrite = TRUE, internal = TRUE)
