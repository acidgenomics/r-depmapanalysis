## FIXME Run droplevels2 on nested cellosaurus inside colData to reduce the
## example object size.



## nolint start
suppressPackageStartupMessages({
    library(devtools)
    library(usethis)
    library(goalie)
    library(AcidBase)
    library(AcidExperiment)
})
## nolint end
load_all()
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
objs <- lapply(X = objs, FUN = `[`, i = i, j = j)
objs <- lapply(X = objs, FUN = droplevels2)
crispr <- objs[["crispr"]]
rnai <- objs[["rnai"]]
rnaseq <- objs[["rnaseq"]]
use_data(crispr, rnai, rnaseq, overwrite = TRUE, internal = FALSE)
