suppressPackageStartupMessages({
    library(devtools)
    library(usethis)
})
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
