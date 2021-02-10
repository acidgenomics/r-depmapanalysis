## Internal Entrez-to-Ensembl identifier mappings.
## Updated 2020-10-02.

## FIXME UPDATE THIS.

library(basejump)        # 0.14.3
library(tidyverse)       # 1.3.0
library(DepMapAnalysis)  # 0.0.4

organism <- "Homo sapiens"

## Download the latest version of the Achilles dataset.
achilles <- Achilles(rowRanges = FALSE, colData = FALSE)

## Map the Entrez identifiers to Ensembl using NCBI OrgDb from AnnotationHub.
entrez <- sort(as.integer(str_extract(
    string = rownames(achilles),
    pattern = "[0-9]+$"
)))

## There are some Entrez IDs missing in the OrgDb that we need to map.
## Look up at https://www.ncbi.nlm.nih.gov/gene/
manualEntrez <-
    import("manual-entrez-ids.csv") %>%
    as_tibble() %>%
    camelCase(strict = TRUE) %>%
    select(entrezId, ensemblId, retired) %>%
    ## FIXME RETHINK THIS STEP?
    rename(
        entrez = entrezId,
        ensembl = ensemblId
    )

## Skipping the bad keys, let's map primarily using the OrgDb lookup.
badKeys <- manualEntrez[["entrez"]]
entrez2ensembl <- Entrez2Ensembl(
    object = setdiff(entrez, badKeys),
    organism = organism,
    format = "1:1"
) %>%
    as_tibble(rownames = NULL)

## As a fall back, get the genomic ranges from Ensembl Ensdb, which also
## contains Ensembl-to-Entrez identifier mappings.
## > gr <- makeGRangesFromEnsembl(
## >     organism = organism,
## >     release = 101L
## > )
## > ensembl2entrez <- Ensembl2Entrez(
## >     object = gr,
## >     format = "long"
## > )
## Double checking, none of these Entrez IDs map from ensembldb either.
## > any(badKeys %in% ensembl2entrez[["entrez"]])
## FALSE

## Now we're ready to combine the automatic mappings with our manual ones.
entrez2ensembl <-
    bind_rows(entrez2ensembl, manualEntrez) %>%
    arrange_all() %>%
    as("DataFrame")
saveData(entrez2ensembl, dir = ".")
