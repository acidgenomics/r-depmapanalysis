#' Import UniProtKB identifier mappings
#'
#' @note Updated 2023-03-01.
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
    org <- org.Hs.eg.db::org.Hs.eg.db
    df <- data.frame(
        "uniprotId" = ids,
        "uniprotId2" = sub(pattern = "-.+$", replacement = "", x = ids)
    )
    suppressMessages({
        map <- AnnotationDbi::select(
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
    map <- map[complete.cases(map), , drop = FALSE]
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
        "ncbiGeneId",
        "ensemblGeneId",
        "geneName",
        "geneDescription"
    )
    out <- leftJoin(
        x = as(df, "DataFrame"),
        y = as(map, "DataFrame"),
        by = "uniprotId2"
    )
    assert(identical(ids, out[["uniprotId"]]))
    out[["uniprotId2"]] <- NULL
    out
}
