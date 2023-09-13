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

datasets <- import("datasets.json")

## Save `R/sysdata.rda` ========================================================

use_data(
    datasets,
    overwrite = TRUE,
    internal = TRUE
)
