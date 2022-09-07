## nolint start
suppressPackageStartupMessages({
    library(devtools)
    library(usethis)
})
## nolint end
load_all()
datasets <- import(
    con = system.file(
        "extdata",
        "datasets.json",
        package = .pkgName,
        mustWork = TRUE
    ),
    quiet = TRUE
)
use_data(datasets, overwrite = TRUE, internal = TRUE)
