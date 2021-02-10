## Internal Entrez-to-Ensembl identifier mappings.
## Updated 2020-10-02.

## FIXME UPDATE THIS.

library(basejump)        # 0.14.3
library(DepMapAnalysis)  # 0.0.4
library(tidyverse)       # 1.3.0

## Download the latest version of the Achilles dataset.
achilles <- Achilles(
    release = "21Q1",
    rowRanges = FALSE,
    colData = FALSE
)

## Map the Entrez identifiers to Ensembl using NCBI OrgDb from AnnotationHub.
entrezIds <- sort(as.integer(str_extract(
    string = rownames(achilles),
    pattern = "[0-9]+$"
)))

## There are some Entrez IDs missing in the OrgDb that we need to map.
## Look up at https://www.ncbi.nlm.nih.gov/gene/
manual <-
    import("manual-entrez-ids.csv") %>%
    camelCase(strict = TRUE) %>%
    .[, c("entrezId", "ensemblId", "ensemblRetired")]
colnames(manual)[colnames(manual) == "ensemblRetired"] <- "retired"

## Skipping the bad keys, let's map primarily using the OrgDb lookup.
keys <- setdiff(x = entrezIds, y = manual[["entrezId"]])
## NOTE If you encounter match failures here, add them to the manual Entrez
## CSV file above.
orgdb <- Entrez2Ensembl(
    object = keys,
    organism = "Homo sapiens",
    format = "1:1"
)

## As a fall back, get the genomic ranges from Ensembl Ensdb, which also
## contains Ensembl-to-Entrez identifier mappings.
## > gr <- makeGRangesFromEnsembl(
## >     organism = organism,
## >     release = 102L
## > )
## > ensembl2entrez <- Ensembl2Entrez(
## >     object = gr,
## >     format = "long"
## > )
## Double checking, none of these Entrez IDs map from ensembldb either.
## > any(badKeys %in% ensembl2entrez[["entrez"]])
## FALSE

## Now we're ready to combine the automatic mappings with our manual ones.
out <-
    bind_rows(
        as_tibble(orgdb, rownames = NULL),
        as_tibble(manual, rownames = NULL)
    ) %>%
    arrange_all() %>%
    as("DataFrame")
stopifnot(
    !any(is.na(out[["entrezId"]])),
    !any(is.na(out[["ensemblId"]]))
)
saveRDS(object = out, file = "entrez2ensembl.rds")
