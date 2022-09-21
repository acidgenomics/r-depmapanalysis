## Relevant files:
## - https://gygi.hms.harvard.edu/data/ccle/Table_S1_Sample_Information.xlsx
## - https://gygi.hms.harvard.edu/data/ccle/Table_S2_Protein_Quant_Normalized.xlsx
## - https://gygi.hms.harvard.edu/data/ccle/protein_quant_current_normalized.csv.gz
## - https://gygi.hms.harvard.edu/data/ccle/Table_S3_Biological_Replicates_Protein_Quant_Normalized.xlsx
## - https://gygi.hms.harvard.edu/data/ccle/Table_S4_Protein_RNA_Correlation_and_Enrichments.xlsx
## - https://gygi.hms.harvard.edu/data/ccle/Table_S5_PCA_PC1_Enriched_Gene_Sets.xlsx
## - https://gygi.hms.harvard.edu/data/ccle/Table_S6_Correlation_Network_Solid_Organ_Lineages.xlsx
## - https://gygi.hms.harvard.edu/data/ccle/Table_S7_Mutation_Associations.xlsx



#' CCLE proteomics data
#'
#' @export
#' @note Updated 2022-09-21.
#'
#' @seealso
#' - https://depmap.org/portal/download/all/?releasename=Proteomics
#' - https://gygi.hms.harvard.edu/publications/ccle.html
CCLEProteomicsData <- function() {
    url <- pasteURL(
        "gygi.hms.harvard.edu",
        "data",
        "ccle",
        "Table_S1_Sample_Information.xlsx",
        protocol = "https"
    )
    df <- import(con = .cacheURL(url), sheet = 2L)
    colnames(df) <- camelCase(colnames(df), strict = TRUE)
    ## FIXME Consider renaming these to match current DepMap metadata:
    ## - "ccleCode"
    ## - "cellLine"
    df
}
