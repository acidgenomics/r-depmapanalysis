## nolint start
suppressPackageStartupMessages({
    library(devtools)
    library(usethis)
    library(syntactic)
    library(pipette)
})
## nolint end
load_all(helpers = FALSE)

## datasets ====================================================================

## Datasets JSON from "datasets.json".
datasets <- import(
    con = system.file(
        "extdata",
        "datasets.json",
        package = .pkgName,
        mustWork = TRUE
    )
)

## Save `R/sysdata.rda` ========================================================

use_data(
    datasets,
    overwrite = TRUE,
    internal = TRUE
)
