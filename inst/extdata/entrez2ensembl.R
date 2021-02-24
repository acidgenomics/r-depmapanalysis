## Internal Entrez-to-Ensembl identifier mappings.
## Updated 2021-02-24.

library(basejump)        # 0.14.13
library(DepMapAnalysis)  # 0.0.4

## Download the latest version of the Achilles dataset.
achilles <- Achilles(
    release = "21Q1",
    rowRanges = FALSE,
    colData = FALSE
)
## Extract the NCBI Entrez identifiers, so we can map to Ensembl below.
entrezIds <-
    gsub(
        pattern = "(.+)_([0-9]+)$",
        replacement = "\\2",
        x = rownames(achilles)
    )
entrezIds <- sort(as.integer(entrezIds))
rm(achilles)

## There are some Entrez IDs missing in the OrgDb that we need to map.
## Look up at https://www.ncbi.nlm.nih.gov/gene/
manual <-
    import("entrez2ensembl.csv") %>%
    camelCase(strict = TRUE) %>%
    .[, c("entrezId", "ensemblId", "ensemblRetired")]
colnames(manual)[colnames(manual) == "ensemblRetired"] <- "retired"

## Skipping the bad keys, let's map primarily using the OrgDb lookup.
keys <- setdiff(x = entrezIds, y = manual[["entrezId"]])

## NOTE If you encounter match failures here, add them to the manual Entrez
## CSV file above.
## FIXME THIS STEP IS GETTING STUCK WHEN RUNNING IN SCRIPT.
## SOME SORT OF BIOCONDUCTOR NAMESPACE ISSUE?
## FIXME NEED TO IMPROVE THE PROGRESS FOR THIS STEP.
orgdb <- Entrez2Ensembl(
    object = keys,
    organism = "Homo sapiens",
    format = "1:1"
)

## Now we're ready to combine the automatic mappings with our manual ones.
out <-
    dplyr::bind_rows(
        as_tibble(orgdb, rownames = NULL),
        as_tibble(manual, rownames = NULL)
    ) %>%
    dplyr::arrange_all() %>%
    as("DataFrame")
stopifnot(
    !any(is.na(out[["entrezId"]])),
    !any(is.na(out[["ensemblId"]]))
)
saveRDS(object = out, file = "entrez2ensembl.rds")
