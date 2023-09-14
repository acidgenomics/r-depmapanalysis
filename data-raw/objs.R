## nolint start
suppressPackageStartupMessages({
    library(devtools)
    library(usethis)
    library(goalie)
    library(AcidBase)
    library(AcidExperiment)
})
## nolint end
load_all(helpers = FALSE)
objs <- list(
    "crispr" = DepMapCrisprGeneEffect(),
    "rnai" = DepMapRnaiGeneEffect(),
    "rnaseq" = DepMapGeneExpression()
)
i <- intersectAll(lapply(X = objs, FUN = rownames))
j <- intersectAll(lapply(X = objs, FUN = colnames))
assert(hasLength(i), hasLength(j))
i <- head(i, n = 100L)
j <- head(j, n = 50L)
objs <- lapply(
    X = objs,
    FUN = function(object) {
        object <- object[i, j]
        object <- droplevels2(object)
        colData(object)[["cellosaurus"]] <-
            droplevels2(colData(object)[["cellosaurus"]])
        object
    }
)
crispr <- objs[["crispr"]]
rnai <- objs[["rnai"]]
rnaseq <- objs[["rnaseq"]]
use_data(crispr, rnai, rnaseq, overwrite = TRUE, internal = FALSE)
