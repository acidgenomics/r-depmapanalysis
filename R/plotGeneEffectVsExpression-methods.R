#' Plot gene effect vs. expression
#'
#' @name plotGeneEffectVsExpression
#' @note Updated 2022-08-05.
#'
#' @inheritParams AcidRoxygen::params
#' @inheritParams params
#'
#' @param effect `GeneEffect`.
#'
#' @param expression `CCLEExpressionData`.
#'
#' @param subtype `character(1)` or `NULL`.
#' Cell subtype name to use for filtering.
#'
#' @param subtypeCol `character(1)`.
#' Column name of metadata defined in `colData` of `effect`.
#'
#' @examples
#' data(crispr, rnaseq)
#'
#' effect <- crispr
#' expression <- rnaseq
#'
#' ## GeneEffect,CCLEExpressionData ====
#' gene <- rownames(effect)[[1L]]
#' plotGeneEffectVsExpression(
#'     effect = effect,
#'     expression = expression,
#'     gene = gene
#' )
NULL



## Updated 2022-03-09.
`plotGeneEffectVsExpression,GE,CCLE` <- # nolint
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
        effect = "GeneEffect",
        expression = "CCLEExpressionData"
    ),
    definition = `plotGeneEffectVsExpression,GE,CCLE`
)
