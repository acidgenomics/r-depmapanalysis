#' @name plotGeneEffectVsExpression
#' @inherit AcidGenerics::plotGeneEffectVsExpression description return title
#' @note Updated 2023-09-14.
#'
#' @inheritParams AcidRoxygen::params
#' @inheritParams params
#'
#' @param effect `DepMapGeneEffect`.
#'
#' @param expression `DepMapGeneExpression`.
#'
#' @param subtype `character(1)` or `NULL`.
#' Cell subtype name to use for filtering.
#'
#' @param subtypeCol `character(1)`.
#' Column name of metadata defined in `colData` of `effect`.
#'
#' @param ... Additional arguments.
#'
#' @examples
#' data(crispr, rnaseq)
#'
#' ## DepMapGeneEffect,DepMapExpression ====
#' effect <- crispr
#' expression <- rnaseq
#' gene <- rownames(effect)[[1L]]
#' plotGeneEffectVsExpression(
#'     effect = effect,
#'     expression = expression,
#'     gene = gene
#' )
NULL



## Updated 2023-01-27.
`plotGeneEffectVsExpression,Effect,Expression` <- # nolint
    function(effect,
             expression,
             gene,
             subtype = NULL,
             subtypeCol = "subtype",
             label = FALSE) {
        assert(
            isString(gene),
            isString(subtype, nullOK = TRUE),
            isString(subtypeCol),
            isSubset(
                x = subtypeCol,
                y = colnames(colData(effect))
            ),
            isFlag(label)
        )
        x <- as(effect, "SummarizedExperiment")
        y <- as(expression, "SummarizedExperiment")
        i <- mapGenesToRownames(object = x, genes = gene, strict = TRUE)
        j <- intersect(colnames(x), colnames(y))
        x <- x[i, j, drop = FALSE]
        y <- y[i, j, drop = FALSE]
        gene <- as.character(rowData(x)[["geneName"]])
        labels <- list(
            "subtitle" = metadata(effect)[["dataset"]],
            "title" = gene,
            x = paste0(
                "gene effect (", metadata(effect)[["scoringMethod"]], ")"
            ),
            y = "expression (log2 tpm)"
        )
        ## Only keep that cells that match desired subtype.
        if (isString(subtype)) {
            cd <- colData(x)
            j <- which(cd[[subtypeCol]] == subtype)
            assert(
                hasLength(j),
                msg = sprintf(
                    "Failed to to match '%s' in '%s'.",
                    subtype, subtypeCol
                )
            )
            x <- x[, j, drop = FALSE]
            y <- y[, j, drop = FALSE]
            labels[["subtitle"]] <-
                paste0(
                    labels[["subtitle"]],
                    " (", subtypeCol, ": ", subtype, ")"
                )
        }
        p <- ggplot(
            data = data.frame(
                x = assay(x)[i, , drop = TRUE],
                y = assay(y)[i, , drop = TRUE],
                label = colData(x)[["cellLineName"]]
            ),
            mapping = aes(
                x = x,
                y = y,
                label = label
            )
        ) +
            geom_point(size = 1L)
        if (isTRUE(label)) {
            p <- p + acid_geom_label_repel()
        }
        p <- p + do.call(what = labs, args = labels)
        p
    }



#' @rdname plotGeneEffectVsExpression
#' @export
setMethod(
    f = "plotGeneEffectVsExpression",
    signature = signature(
        effect = "DepMapGeneEffect",
        expression = "DepMapGeneExpression"
    ),
    definition = `plotGeneEffectVsExpression,Effect,Expression`
)
