#' Import UniProtKB identifier mappings
#'
#' @note Updated 2023-02-28.
#' @noRd
#'
#' @param ids `character`.
#' UniProtKB accession identifiers.
#'
#' @seealso
#' - https://support.bioconductor.org/p/71702/
#' - https://support.bioconductor.org/p/9145991/
#'
#' @return `DataFrame`.
.uniprotToGene <- function(ids) {
    assert(
        requireNamespaces("org.Hs.eg.db"),
        isCharacter(ids)
    )
    df <- data.frame(
        "uniprotId" = ids,
        "uniprotId2" = sub(pattern = "-.+$", replacement = "", x = ids)
    )
    org <- org.Hs.eg.db::org.Hs.eg.db
    suppressMessages({
        map <- select(
            x = org,
            keys = unique(df[["uniprotId2"]]),
            columns = c(
                "ENTREZID",
                "ENSEMBL",
                "SYMBOL",
                "GENENAME"
            ),
            keytype = "UNIPROT"
        )
    })
    map <- map[complete.cases(map), ]
    idx <- order(
        map[["UNIPROT"]],
        as.integer(map[["ENTREZID"]]),
        map[["ENSEMBL"]]
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
    out <- leftJoin(
        x = as(df, "DataFrame"),
        y = as(map, "DataFrame"),
        by = "uniprotId2"
    )
    out[["uniprotId2"]] <- NULL
    out
}
