#' HGNC (HUGO) gene annotations
#'
#' @note Updated 2023-03-01
#' @noRd
.hgnc <- function() {
    hgnc <- HGNC()
    cols <- c("hgncId", "symbol", "name", "ensemblGeneId", "entrezId")
    assert(isSubset(cols, colnames(hgnc)))
    df <- as(hgnc, "DataFrame")
    df <- df[, cols]
    colnames(df)[colnames(df) == "entrezId"] <- "entrezGeneId"
    colnames(df)[colnames(df) == "name"] <- "geneDescription"
    colnames(df)[colnames(df) == "symbol"] <- "geneName"
    df[["entrezGeneId"]] <- as.integer(df[["entrezGeneId"]])
    df[["hgncId"]] <- as.integer(df[["hgncId"]])
    df
}
