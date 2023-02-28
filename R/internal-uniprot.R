## FIXME A2A2Z9 maps to ANKRD18B but is failing here...argh.
## Seems like the org.Hs.eg.db annotations are out of date / problematic.



#' Import UniProtKB identifier mappings
#'
#' @note Updated 2023-02-28.
#' @noRd
#'
#' @seealso
#' - https://support.bioconductor.org/p/71702/
.uniprotToGene <- function(ids) {
    assert(
        requireNamespaces("org.Hs.eg.db"),
        isCharacter(ids)
    )
    org <- org.Hs.eg.db::org.Hs.eg.db
    df <- data.frame(
        "uniprotId" = ids,
        "uniprotId2" = sub(pattern = "-.+$", replacement = "", x = ids)
    )
    suppressMessages({
        map <- select(
            x = org,
            keys = unique(df[["uniprotId2"]]),
            columns = c(
                #"ENTREZID",
                #"ENSEMBL",
                "SYMBOL",
                "GENENAME"
            ),
            keytype = "UNIPROT"
        )
    })
    map <- map[complete.cases(map), ]
    idx <- order(
        map[["UNIPROT"]],
        ##as.integer(map[["ENTREZID"]]),
        ##map[["ENSEMBL"]]
        map[["SYMBOL"]]
    )
    map <- map[idx, ]
    idx <- which(!duplicated(map[["UNIPROT"]]))
    map <- map[idx, ]
    colnames(map) <- c(
        "uniprotId2",
        "entrezId",
        "ensemblId",
        "geneName",
        "geneDescription"
    )
    xxx <- leftJoin(
        as(df, "DataFrame"),
        as(map, "DataFrame"),
        by = "uniprotId2"
    )
    xxx[["uniprotId2"]] <- NULL




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
