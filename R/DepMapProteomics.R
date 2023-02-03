#' Import DepMap proteomics data
#'
#' @export
#' @note Updated 2023-02-03.
#'
#' @section Nusinow et al. 2020 (Gygi lab) dataset:
#'
#' Citation:
#' https://doi.org/10.1016/j.cell.2019.12.023
#'
#' - [Table_S1_Sample_Information.xlsx](https://gygi.hms.harvard.edu/data/ccle/Table_S1_Sample_Information.xlsx)
#' - [Table_S2_Protein_Quant_Normalized.xlsx](https://gygi.hms.harvard.edu/data/ccle/Table_S2_Protein_Quant_Normalized.xlsx)
#' - [Table_S3_Biological_Replicates_Protein_Quant_Normalized.xlsx](https://gygi.hms.harvard.edu/data/ccle/Table_S3_Biological_Replicates_Protein_Quant_Normalized.xlsx)
#' - [Table_S4_Protein_RNA_Correlation_and_Enrichments.xlsx](https://gygi.hms.harvard.edu/data/ccle/Table_S4_Protein_RNA_Correlation_and_Enrichments.xlsx)
#' - [Table_S5_PCA_PC1_Enriched_Gene_Sets.xlsx](https://gygi.hms.harvard.edu/data/ccle/Table_S5_PCA_PC1_Enriched_Gene_Sets.xlsx)
#' - [Table_S6_Correlation_Network_Solid_Organ_Lineages.xlsx](https://gygi.hms.harvard.edu/data/ccle/Table_S6_Correlation_Network_Solid_Organ_Lineages.xlsx)
#' - [Table_S7_Mutation_Associations.xlsx](https://gygi.hms.harvard.edu/data/ccle/Table_S7_Mutation_Associations.xlsx)
#' - [protein_quant_current_normalized.csv.gz](https://gygi.hms.harvard.edu/data/ccle/protein_quant_current_normalized.csv.gz)
#'
#' @section UniProt:
#'
#' Note that `"sp"` for UniProtKB/Swiss-Prot and `"tr"` for UniProtKB/TrEMBL.
#'
#' See also:
#' - https://www.uniprot.org/help/fasta-headers
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
    colData <- as(colData, "DataFrame")
    colnames(colData) <- camelCase(colnames(colData), strict = TRUE)
    assert(identical(
        sort(colnames(colData)),
        c(
            "ccleCode", "cellLine", "notes",
            "protein10PlexId", "proteinTmtLabel", "tissueOfOrigin"
        )
    ))
    ## Drop the bridge lines.
    colData <- colData[colData[["protein10PlexId"]] != "0", , drop = FALSE]
    rownames(colData) <- makeNames(paste0(
        colData[["ccleCode"]], "_TenPx",
        autopadZeros(colData[["protein10PlexId"]])
    ))
    url <- pasteURL(baseUrl, "table-s2-protein-quant-normalized.csv")
    ## This step can fail when using readr engine without increasing default
    ## `VROOM_CONNECTION_SIZE`. Using data.table here instead to avoid.
    df <- import(con = .cacheURL(url), engine = "data.table")
    assert(
        isSubset(c("Gene_Symbol", "Uniprot_Acc"), colnames(df)),
        hasNoDuplicates(df[["Uniprot_Acc"]])
    )
    rownames(df) <- df[["Uniprot_Acc"]]
    ## We're only interested in peptides that map to a gene.
    keepRows <- !is.na(df[["Gene_Symbol"]])
    df <- df[keepRows, , drop = FALSE]
    keepCols <- !grepl(pattern = "^TenPx[0-9]{2}_Peptides$", x = colnames(df))
    df <- df[, keepCols, drop = FALSE]
    keepCols <- !grepl(pattern = "^Column[0-9]+$", x = colnames(df))
    df <- df[, keepCols, drop = FALSE]
    assayCols <- grepl(pattern = "_TenPx[0-9]+$", x = colnames(df))
    assay <- df[, assayCols, drop = FALSE]
    assert(all(bapply(X = assay, FUN = is.numeric)))
    assay <- as.matrix(assay)
    rowData <- df[, setdiff(colnames(df), colnames(assay))]
    rowData <- as(rowData, "DataFrame")
    colnames(rowData) <- camelCase(colnames(rowData), strict = TRUE)
    assert(identical(
        sort(colnames(rowData)),
        c(
            "description", "geneSymbol", "groupId",
            "proteinId", "uniprot", "uniprotAcc"
        )
    ))
    colnames(rowData)[colnames(rowData) == "geneSymbol"] <- "geneName"
    assays <- list("proteinQuantNormalized" = assay)
    metadata <- list(
        "dataset" = "nusinow2020",
        "packageName" = .pkgName,
        "packageVersion" = .pkgVersion
    )
    ## FIXME Improve metadata assignment.
    se <- makeSummarizedExperiment(
        assays = assays,
        rowData = rowData,
        colData = colData,
        metadata = metadata
    )
    ## FIXME Change the CCLE name to the DepMap identifier...
    new(Class = "DepMapProteomics", se)
}
