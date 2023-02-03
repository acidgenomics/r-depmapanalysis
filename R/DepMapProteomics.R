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
    .importNusinow2020()
}



#' Import the Nusinow et al 2020 proteomics dataset
#'
#' @note Updated 2023-02-03.
#' @noRd
#'
#' @details
#' We've observed connection issues with the Gygy lab website at
#' `"https://gygi.hms.harvard.edu/publications/ccle.html"`, so caching a copy
#' for our pacakge instead.
.importNusinow2020 <- function() {
    baseUrl <- pasteURL(.extdataUrl, "proteomics", "nusinow-2020")
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
    rownames(df) <- makeNames(df[["Uniprot_Acc"]])
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
    assays <- list("normalized" = assay)
    metadata <- list(
        "dataset" = "nusinow_2020",
        "packageName" = .pkgName,
        "packageVersion" = .pkgVersion
    )
    se <- makeSummarizedExperiment(
        assays = assays,
        rowData = rowData,
        colData = colData,
        metadata = metadata
    )
    se <- .standardizeNusinow2020(se)
    new(Class = "DepMapProteomics", se)
}



## NOTE Consider moving this to the 'internal-SE.R' file.

#' Standardize the Nusinow et al 2020 proteomics dataset
#'
#' @note Updated 2023-01-27.
#' @noRd
.standardizeNusinow2020 <- function(object) {
    assert(is(object, "SummarizedExperiment"))
    currentDataset <- .formalsList[["dataset"]][[1L]]
    alert(sprintf(
        "Standardizing {.var %s} annotations to DepMap {.var %s}.",
        "Nusinow et al 2020",
        currentDataset
    ))
    assert(isString(currentDataset))
    rowData <- rowData(object)
    colnames(rowData)[colnames(rowData) == "geneSymbol"] <- "geneName"
    rowData(object) <- rowData
    cd1 <- colData(object)
    colnames(cd1)[colnames(cd1) == "ccleCode"] <- "ccleName"
    cd1[["cellLine"]] <- NULL
    cd1[["notes"]] <- NULL
    cd1[["tissueOfOrigin"]] <- NULL
    cd2 <- .importCellLineSampleData(dataset = currentDataset)
    cd2 <- cd2[!is.na(cd2[["ccleName"]]), ]
    cd <- leftJoin(x = cd1, y = cd2, by = "ccleName")
    assert(!any(is.na(cd[["depmapId"]])))
    cd <- cd[, sort(colnames(cd))]
    colData(object) <- cd
    colnames(object) <- makeNames(paste0(
        cd[["depmapId"]], "_TENPX",
        autopadZeros(cd[["protein10PlexId"]])
    ))
    object <- object[, sort(colnames(object))]
    object
}
