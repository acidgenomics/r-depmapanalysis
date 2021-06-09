#' Plot gene effect
#'
#' @name plotGeneEffect
#' @note Updated 2021-06-09.
#'
#' @inheritParams AcidRoxygen::params
#'
#' @return `ggplot`.
#'
#' @examples
#' data(crispr)
#'
#' ## DepMapAnalysis ====
#' object <- crispr
#' genes <- head(rownames(object), n = 5L)
#' plotGeneEffect(object, genes = genes)
NULL



## Updated 2021-06-09.
`plotGeneEffect,DepMapAnalysis` <-  # nolint
    function(
        object,
        genes,
        geom = c("boxplot", "density")
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
        data <- as_tibble(melt(mat))
        data <- data[complete.cases(data), , drop = FALSE]
        if (identical(geom, "boxplot")) {
            p <- ggplot(
                data = data,
                mapping = aes(
                    x = reorder(
                        !!sym("rowname"),
                        !!sym("value"),
                        mean
                    ),
                    ## > x = !!sym("rowname"),
                    y = !!sym("value"),
                    fill = !!sym("rowname")
                )
            ) +
                geom_hline(
                    color = "red",
                    linetype = "dashed",
                    size = 1.25,
                    yintercept = -1L
                ) +
                geom_boxplot(
                    color = "black",
                    show.legend = FALSE,
                    size = 0.75
                ) +
                scale_x_discrete(limits = rev) +
                coord_flip() +
                labs(
                    title = "gene effect",
                    x = "gene",
                    y = "gene effect"
                )
        } else if (identical(geom, "density")) {
            p <- ggplot(
                data = data,
                mapping = aes(
                    x = !!sym("value"),
                    fill = !!sym("rowname")
                )
            ) +
                geom_vline(
                    color = "red",
                    linetype = "dashed",
                    size = 1.25,
                    xintercept = -1L
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
        p
    }



#' @rdname plotGeneEffect
#' @export
setMethod(
    f = "plotGeneEffect",
    signature = signature("DepMapAnalysis"),
    definition = `plotGeneEffect,DepMapAnalysis`
)
