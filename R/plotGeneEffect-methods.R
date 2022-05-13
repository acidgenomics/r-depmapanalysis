#' Plot gene effect
#'
#' @name plotGeneEffect
#' @note Updated 2022-09-09.
#'
#' @inheritParams AcidRoxygen::params
#'
#' @return `ggplot`.
#'
#' @examples
#' data(crispr)
#'
#' ## GeneEffect ====
#' object <- crispr
#' genes <- head(rownames(object), n = 5L)
#' plotGeneEffect(object, genes = genes)
NULL



## Updated 2021-07-07.
`plotGeneEffect,GeneEffect` <-  # nolint
    function(
        object,
        genes,
        geom = c("boxplot", "violin", "density")
    ) {
        validObject(object)
        assert(
            isCharacter(genes),
            length(genes) <= 100L
        )
        geom <- match.arg(geom)
        se <- as(object, "SummarizedExperiment")
        rownames <- mapGenesToRownames(
            object = se,
            genes = genes,
            strict = TRUE
        )
        se <- se[rownames, , drop = FALSE]
        mat <- assay(se, i = "effect")
        rownames(mat) <- as.character(rowData(se)[["geneName"]])
        data <- melt(mat)
        data <- data[complete.cases(data), , drop = FALSE]
        switch(
            EXPR = geom,
            "boxplot" = {
                p <- ggplot(
                    data = as.data.frame(data),
                    mapping = aes(
                        x = !!sym("value"),
                        y = reorder(
                            x = !!sym("rowname"),
                            X = !!sym("value"),
                            FUN = median
                        ),
                        fill = !!sym("rowname")
                    )
                ) +
                    geom_boxplot(
                        color = "black",
                        show.legend = FALSE,
                        size = 0.75
                    ) +
                    scale_y_discrete(limits = rev) +
                    labs(
                        title = "gene effect",
                        x = "gene",
                        y = "gene effect"
                    )
            },
            "violin" = {
                p <- ggplot(
                    data = as.data.frame(data),
                    mapping = aes(
                        x = !!sym("value"),
                        y = reorder(
                            x = !!sym("rowname"),
                            X = !!sym("value"),
                            FUN = median
                        ),
                        fill = !!sym("rowname")
                    )
                ) +
                    geom_violin(
                        color = "black",
                        ## > draw_quantiles = c(0.25, 0.5, 0.75),
                        draw_quantiles = 0.5,
                        show.legend = FALSE,
                        scale = "count",
                        size = 0.75,
                        trim = TRUE
                    ) +
                    scale_y_discrete(limits = rev) +
                    labs(
                        title = "gene effect",
                        x = "gene",
                        y = "gene effect"
                    )
            },
            "density" = {
                p <- ggplot(
                    data = as.data.frame(data),
                    mapping = aes(
                        x = !!sym("value"),
                        fill = !!sym("rowname")
                    )
                ) +
                    geom_density(
                        color = "black",
                        show.legend = FALSE,
                        size = 0.75
                    ) +
                    facet_wrap(
                        facets = sym("rowname"),
                        scales = "fixed"
                    ) +
                    labs(
                        title = "gene effect",
                        x = "gene effect"
                    )
            }
        )
        p <- p +
            geom_vline(
                color = "red",
                linetype = "dashed",
                size = 1.25,
                xintercept = -1L
            )
        p
    }



#' @rdname plotGeneEffect
#' @export
setMethod(
    f = "plotGeneEffect",
    signature = signature(object = "GeneEffect"),
    definition = `plotGeneEffect,GeneEffect`
)
