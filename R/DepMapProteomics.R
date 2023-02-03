## FIXME Allow the user to pick the dataset.
## FIXME For Sanger, we need to cache the download and extract.
## https://cellmodelpassports.sanger.ac.uk/downloads
## FIXME Can we standardize the gygi and sanger datasets?



## FIXME WORK IN PROGRESS
##
## Relevant files:
## - https://gygi.hms.harvard.edu/data/ccle/Table_S1_Sample_Information.xlsx
## - https://gygi.hms.harvard.edu/data/ccle/Table_S2_Protein_Quant_Normalized.xlsx
## - https://gygi.hms.harvard.edu/data/ccle/protein_quant_current_normalized.csv.gz
## - https://gygi.hms.harvard.edu/data/ccle/Table_S3_Biological_Replicates_Protein_Quant_Normalized.xlsx
## - https://gygi.hms.harvard.edu/data/ccle/Table_S4_Protein_RNA_Correlation_and_Enrichments.xlsx
## - https://gygi.hms.harvard.edu/data/ccle/Table_S5_PCA_PC1_Enriched_Gene_Sets.xlsx
## - https://gygi.hms.harvard.edu/data/ccle/Table_S6_Correlation_Network_Solid_Organ_Lineages.xlsx
## - https://gygi.hms.harvard.edu/data/ccle/Table_S7_Mutation_Associations.xlsx



#' Import DepMap proteomics data
#'
#' @export
#' @note Updated 2023-02-03.
#'
#' @return `DepMapProteomics`.
#'
#' @seealso
#' - https://depmap.org/portal/download/all/?releasename=Proteomics
#' - https://gygi.hms.harvard.edu/publications/ccle.html
DepMapProteomics <- function() {
    baseUrl <- pasteURL(.extdataUrl, "proteomics", "gygi")
    url <- pasteURL(baseUrl, "table-s1-sample-information.csv")
    colData <- import(con = .cacheURL(url))
    colnames(colData) <- camelCase(colnames(colData), strict = TRUE)
    ## [1] "Cell_Line"          "CCLE_Code"
    ## [3] "Tissue_of_Origin"   "Protein_10_Plex_ID"
    ## [5] "Protein_TMT_Label"  "Notes"
    ## FIXME Consider renaming these to match current DepMap metadata:
    ## - "ccleCode"
    ## - "cellLine"
    url <- pasteURL(baseUrl, "table-s2-protein-quant-normalized.csv")
    ## This step can fail when using readr engine without increasing default
    ## `VROOM_CONNECTION_SIZE`. Using data.table here instead to avoid.
    assay <- import(con = .cacheURL(url), engine = "data.table")
    ## Peptide fragments are annotated as "tr", which we are dropping here.
    ## > keep <- grepl(pattern = "^sp\\|", x = assay[["Protein_Id"]])
    ## > assay <- assay[keep, , drop = FALSE]
    colData
}
