#' Plot gene effect
#'
#' @name plotGeneEffect
#' @note Updated 2020-10-02.
#'
#' @inheritParams acidroxygen::params
#'
#' @return `ggplot`.
NULL



`plotGeneEffect,Achilles` <-  # nolint
    function(object, genes) {
        validObject(object)
        assert(isCharacter(genes))
        mat <- assay(object, i = "effect")
        rownames <- mapGenesToRownames(object, genes = genes)
        mat <- mat[rownames, , drop = FALSE]
        data <- as_tibble(melt(mat))
        p <- ggplot(
            data = data,
            mapping = aes(
                x = !!sym("value")
            )
        ) +
            geom_density(color = NA, fill = "black") +
            facet_wrap(
                facets = sym("rowname"),
                scales = "free_y"
            ) +
            labs(
                title = "Achilles gene effect",
                x = "gene effect"
            )
        ## https://gist.github.com/tomhopper/9076152
        range <- ggplot_build(p)$layout$panel_scales_x[[1]]$range$range
        # p <- p + scale_x_continuous(
        #     breaks = seq(
        #         from = xlimits[[1L]],
        #         to = xlimits[[2L]],
        #         by = 0.25
        #     )
        # )
        p
    }



#' @rdname plotGeneEffect
#' @export
setMethod(
    f = "plotGeneEffect",
    signature = signature("Achilles"),
    definition = `plotGeneEffect,Achilles`
)
