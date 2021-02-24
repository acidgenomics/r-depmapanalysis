## Internal Entrez-to-Ensembl identifier mappings.
## Updated 2021-02-24.

library(goalie)          # 0.5.0
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
    as("DataFrame") %>%
    camelCase(strict = TRUE) %>%
    .[, c("entrezId", "ensemblId", "ensemblRetired")]
colnames(manual)[colnames(manual) == "ensemblRetired"] <- "retired"
manual[["source"]] <- "manual"

## Skipping the bad keys, let's map primarily using the OrgDb lookup.
keys <- setdiff(x = entrezIds, y = manual[["entrezId"]])

## NOTE If you encounter match failures here, add them to the manual Entrez
## CSV file above.
orgdb <- Entrez2Ensembl(
    object = keys,
    organism = "Homo sapiens",
    format = "1:1"
)
orgdb <- as(orgdb, "DataFrame")
orgdb[["retired"]] <- FALSE
orgdb[["source"]] <- "orgdb"

## Now we're ready to combine the automatic mappings with our manual ones.
assert(areDisjointSets(manual[["entrezId"]], orgdb[["entrezId"]]))
out <- rbind(manual, orgdb)
out[["source"]] <- as.factor(out[["source"]])
out <- out[order(out[["entrezId"]]), , drop = FALSE]
assert(
    !any(is.na(out[["entrezId"]])),
    !any(is.na(out[["ensemblId"]]))
)
saveRDS(object = out, file = "entrez2ensembl.rds")
