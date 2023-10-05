## nolint start

#' Calculate co-dependency scores
#'
#' @name DepMapCodependencies
#' @note Updated 2023-02-03.
#'
#' @inheritParams params
#'
#' @param gene1 `character(1)`.
#' Gene identifier.
#'
#' @param gene2 `character(1)` or `NULL`.
#' Gene identifier or `NULL`.
#' If `NULL`, calculate correlations against all genes.
#'
#' @return `DepMapCodependencies`.
#'
#' @seealso
#' - [How to download codependency scores](https://forum.depmap.org/t/down-load-of-co-dependencies/175/)
#' - [Python script for processing of all codependency scores](https://gist.github.com/pgm/ac2ac4c664ef81200ce49133cc4cee02)
#'
#' @examples
#' data(crispr)
#'
#' ## DepMapGeneEffect ====
#' object <- crispr
#' genes <- as.character(rowData(object)[["geneName"]])
#' gene1 <- genes[[1L]]
#' gene2 <- genes[[2L]]
#' print(c(gene1, gene2))
#' ## Calculate all co-dependencies for a gene of interest.
#' x <- DepMapCodependencies(
#'     object = object,
#'     gene1 = 1,
#'     gene2 = NULL
#' )
#' print(head(x))
#' ## Lineage restrict and compare 2 genes.
#' diseases <- as.character(colData(object)[["cellosaurusNcItDisease"]])
#' disease <- diseases[[1L]]
#' keep <- colData(object)[["cellosaurusNcItDisease"]] %in% disease
#' object2 <- object[, keep]
#' x <- DepMapCodependencies(
#'     object = object2,
#'     gene1 = gene1,
#'     gene2 = gene2
#' )
#' print(x)
NULL

## nolint end



## Updated 2023-02-03.
`DepMapCodependencies,DepMapGeneEffect` <- # nolint
    function(object, gene1, gene2 = NULL) {
        assert(
            validObject(object),
            isString(gene1),
            isString(gene2, nullOk = TRUE)
        )
        assay <- assay(object, i = "effect")
        assert(is.matrix(assay))
        gene1 <- unname(mapGenesToRownames(object, genes = gene1))
        if (isString(gene2)) {
            gene2 <- unname(mapGenesToRownames(object, genes = gene2))
            x <- assay[gene1, , drop = TRUE]
            y <- assay[gene2, , drop = TRUE]
            pearson <- cor(x = x, y = y, method = "pearson")
            df <- DataFrame(
                "gene1" = gene1,
                "gene2" = gene2,
                "pearson" = pearson
            )
        } else {
            ## FIXME This step is now broken...need to rethink.
            idx <- match(x = gene1, table = rownames(assay))
            x <- assay[-idx, , drop = FALSE]
            y <- assay[idx, , drop = TRUE]
            pearson <- apply(
                X = x,
                MARGIN = 1L,
                FUN = cor,
                y = y,
                method = "pearson",
                simplify = TRUE
            )
            assert(is.numeric(pearson))
            pearson <- sort(pearson, decreasing = TRUE)
            df <- DataFrame(
                "gene1" = gene1,
                "gene2" = names(pearson),
                "pearson" = unname(pearson)
            )
        }
        metadata(df) <- list(
            "dataset" = metadata(object)[["dataset"]],
            "date" = Sys.Date(),
            "dim" = dim(object),
            "packageName" = .pkgName,
            "packageVersion" = .pkgVersion
        )
        new(Class = "DepMapCodependencies", df)
    }



#' @rdname DepMapCodependencies
#' @export
setMethod(
    f = "DepMapCodependencies",
    signature = signature(object = "DepMapGeneEffect"),
    definition = `DepMapCodependencies,DepMapGeneEffect`
)
