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
    df <- import(con = .cacheURL(url), engine = "data.table")
    ## Process columns ---------------------------------------------------------
    keep <- !grepl(pattern = "^TenPx[0-9]{2}_Peptides$", x = colnames(df))
    df <- df[, keep]
    keep <- !grepl(pattern = "^Column[0-9]+$", x = colnames(df))
    df <- df[, keep]
    assayCols <- grepl(pattern = "_TenPx[0-9]+$", x = colnames(df))
    assay <- df[, assayCols]
    ## Process rows ------------------------------------------------------------
    rowData <- df[, setdiff(colnames(df), colnames(assay))]
    rowData[["idx"]] <- seq_len(nrow(rowData))
    keep <- !is.na(rowData[["Gene_Symbol"]])
    rowData <- rowData[keep, ]
    keep <- grepl(pattern = "^sp\\|", x = rowData[["Protein_Id"]])
    rowData <- rowData[keep, ]

    ## FIXME Assign by Uniprot accession ID.
    #x <- paste0(rowData[["Gene_Symbol"]], "_HUMAN")
    #y <- rowData[["Uniprot"]]
    #keep <- !grepl(pattern = "-[0-9]+$", x = rowData[["Uniprot_Acc"]])
    #rowData <- rowData[keep, ]#
    spl <- split(rowData, f = rowData[["Gene_Symbol"]])
    xxx <- vapply(X = spl, FUN = nrow, FUN.VALUE = integer(1L)) > 1L
    xxx <- spl[xxx]

    xxx <- unique(df[["Gene_Symbol"]][which(duplicated(df[["Gene_Symbol"]]))])
    ## Duplicates:
    ## [1] "FUBP1"   "NCL"     "HSH2D"   "UBQLN1"  "MICAL2"
    ## [6] "NCKIPSD"
    ##
    ## sp|Q96AE4|FUBP1_HUMAN
    ## sp|Q96AE4-2|FUBP1_HUMAN
    ##
    ## sp|P19338|NUCL_HUMAN
    ## tr|H7BY16|H7BY16_HUMAN
    ##
    ## sp|Q96JZ2-2|HSH2D_HUMAN
    ## tr|K7ERI2|K7ERI2_HUMAN
    ##
    ## sp|Q9UMX0|UBQL1_HUMAN
    ## sp|Q9UMX0-2|UBQL1_HUMAN
    ##
    ## sp|O94851|MICA2_HUMAN
    ## sp|O94851-4|MICA2_HUMAN
    ##
    ## sp|Q9NZQ3-2|SPN90_HUMAN
    ## sp|Q9NZQ3|SPN90_HUMAN

    ## FIXME Split by gene symbol and determine individual hits.

    ## FIXME Only keep gene symbol and uniprot matches.


    ## Peptide fragments are annotated as "tr", which we are dropping here.
    ## > keep <- grepl(pattern = "^sp\\|", x = assay[["Protein_Id"]])
    ## > assay <- assay[keep, , drop = FALSE]
    colData
    ## FIXME Consider removing peptides that don't map to any genes....
    ## Which data point corresponds to genes in depmap website?
    ## Check with our biomarker of interest.
}
