#' Plot top gene effect (dependencies) per cell
#'
#' @name plotTopGeneEffectPerCell
#' @note Updated 2023-08-16.
#'
#' @inheritParams AcidRoxygen::params
#'
#' @return `ggplot`.
#'
#' @examples
#' data(crispr)
#'
#' ## DepMapGeneEffect ====
#' object <- crispr
#' cells <- head(colnames(object), n = 6L)
#' plotTopGeneEffectPerCell(object, cells = cells, n = 5L)
NULL



## Updated 2023-08-16.
`plotTopGeneEffectPerCell,GE` <- # nolint
    function(object,
             cells,
             n = 5L) {
        validObject(object)
        assert(
            requireNamespaces("tidytext"),
            isCharacter(cells),
            isInt(n)
        )
        se <- as(object, "SummarizedExperiment")
        j <- .mapCellsToColnames(se, cells = cells)
        se <- se[, j, drop = FALSE]
        mat <- assay(se, i = "effect")
        rownames(mat) <- as.character(rowData(se)[["geneName"]])
        colnames(mat) <- as.character(colData(se)[["cellLineName"]])
        data <- melt(mat)
        data <- data[complete.cases(data), , drop = FALSE]
        data <- data[order(data[["value"]]), , drop = FALSE]
        split <- split(x = data, f = data[["colname"]])
        split <- lapply(X = split, FUN = head, n = n)
        data <- do.call(what = rbind, args = split)
        ## For reference, here's a guide to `reorder_within()`:
        ## https://juliasilge.com/blog/reorder-within/
        p <- ggplot(
            data = as.data.frame(data),
            mapping = aes(
                x = .data[["value"]],
                y = tidytext::reorder_within(
                    x = .data[["rowname"]],
                    by = .data[["value"]],
                    within = .data[["colname"]]
                ),
                color = .data[["colname"]]
            )
        ) +
            geom_point(
                fill = NA,
                show.legend = FALSE,
                size = 3L
            ) +
            labs(
                title = "top dependencies per cell",
                x = "gene effect",
                y = "gene name"
            ) +
            facet_wrap(
                facets = vars(.data[["colname"]]),
                scales = "free"
            ) +
            tidytext::scale_y_reordered()
        p
    }



#' @rdname plotTopGeneEffectPerCell
#' @export
setMethod(
    f = "plotTopGeneEffectPerCell",
    signature = signature(object = "DepMapGeneEffect"),
    definition = `plotTopGeneEffectPerCell,GE`
)
