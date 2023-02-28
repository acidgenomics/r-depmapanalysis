#' Import UniProtKB identifier mappings
#'
#' @note Updated 2023-02-28.
#' @noRd
.importUniprot <- function() {
    ## FIXME Ugh this file is huge, is there another way?
    url <- pasteURL(
        "ftp.uniprot.org",
        "pub",
        "databases",
        "uniprot",
        "current_release",
        "knowledgebase",
        "idmapping",
        "idmapping_selected.tab.gz",
        protocol = "ftp"
    )
    df <- import(
        con = .cacheURL(url),
        format = "tsv",
        colnames = FALSE
    )
    ## FIXME Work on simplifying these"
    colnames <- camelCase(c(
        "UniProtKB-AC",
        "UniProtKB-ID",
        "GeneID (EntrezGene)",
        "RefSeq",
        "GI",
        "PDB",
        "GO",
        "UniRef100",
        "UniRef90",
        "UniRef50",
        "UniParc",
        "PIR",
        "NCBI-taxon",
        "MIM",
        "UniGene",
        "PubMed",
        "EMBL",
        "EMBL-CDS",
        "Ensembl",
        "Ensembl_TRS",
        "Ensembl_PRO",
        "Additional PubMed"
    ))
}
