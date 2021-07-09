#' Plot gene effect vs. expression
#'
#' @name plotGeneEffectVsExpression
#' @note Updated 2021-07-08.
#'
#' @inheritParams AcidRoxygen::params
#' @inheritParams params
#'
#' @param effect `GeneEffect`.
#' @param expression `CCLEExpressionData`.
#'
#' @examples
#' data(crispr)
#'
#' effect <- crispr
#' expression <- CCLEExpressionData()
#'
#' ## GeneEffect,CCLEExpressionData ====
#' gene <- rownames(effect)[[1L]]
#' plotGeneEffectVsExpression(
#'     effect = effect,
#'     expression = expression,
#'     gene = gene
#' )
NULL



`plotGeneEffectVsExpression,GE,CCLE` <-  # nolint
    function(
        effect,
        expression,
        gene,
        subtype = NULL,
        subtypeCol = "subtype",
        label = FALSE
    ) {
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
        labels <- list(
            "subtitle" = metadata(effect)[["dataset"]],
            "title" = gene,
            x = paste0(
                "gene effect (", metadata(effect)[["scoringMethod"]], ")"
            ),
            y = "expression (log2 tpm)"
        )
        x <- as(effect, "SummarizedExperiment")
        y <- as(expression, "SummarizedExperiment")
        i <- gene
        j <- intersect(colnames(x), colnames(y))
        x <- x[i, j, drop = FALSE]
        y <- y[i, j, drop = FALSE]
        ## Only keep that cells that match desired subtype.
        if (isString(subtype)) {
            cd <- colData(x)
            j <- which(cd[[subtypeCol]] == subtype)
            x <- x[, j, drop = FALSE]
            y <- y[, j, drop = FALSE]
            labels[["subtitle"]] <-
                paste0(labels[["subtitle"]], " (", subtype, ")")
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