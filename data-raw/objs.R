## FIXME Find cells and genes that intersect across all 3.
## FIXME Take the top 100 genes and the top 50 cells.
## FIXME Subset CCLECopyNumberData to match dims.
## nolint start
suppressPackageStartupMessages({
    library(devtools)
    library(usethis)
    library(AcidBase)
    library(AcidExperiment)
})
## nolint end
load_all()
objs <- list(
    "crispr" = GeneEffect(dataset = "depmap_public_22q2"),
    "rnai" = GeneEffect(dataset = "demeter2_data_v6"),
    "rnaseq" = CCLEExpressionData(dataset = "depmap_public_22q2")
)
genes <- sort(intersectAll(lapply(X = objs, FUN = rownames)))
cells <- sort(intersectAll(lapply(X = objs, FUN = colnames)))
i <- head(genes, n = 100L)
j <- head(cells, n = 50L)
objs <- lapply(X = objs, FUN = `[`, i = i, j = j)
objs <- lapply(X = objs, FUN = droplevels2)
crispr <- objs[["crispr"]]
rnai <- objs[["rnai"]]
rnaseq <- objs[["rnaseq"]]
use_data(crispr, rnai, rnaseq, overwrite = TRUE, internal = FALSE)
