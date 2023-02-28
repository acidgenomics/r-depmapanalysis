## FIXME A2A2Z9 maps to ANKRD18B but is failing here...argh.
## Seems like the org.Hs.eg.db annotations are out of date / problematic.



#' Import UniProtKB identifier mappings
#'
#' @note Updated 2023-02-28.
#' @noRd
#'
#' @seealso
#' - https://support.bioconductor.org/p/71702/
#' - https://support.bioconductor.org/p/9145991/
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
    xxx <- leftJoin(
        as(df, "DataFrame"),
        as(map, "DataFrame"),
        by = "uniprotId2"
    )
    xxx[["uniprotId2"]] <- NULL
}
