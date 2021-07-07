#' Plot top gene effect per group of interest
#'
#' @name plotTopGeneEffectPerGroup
#' @note Updated 2021-07-07.
#'
#' @inheritParams AcidRoxygen::params
#' @param gene `character(1)`.
#' @param group `character(1)` or `NULL`.
#'   Column name defined in `colData` to use for grouping.
#'   (e.g. `"subtype"`).
#' @param minNPerGroup `integer(1)`.
#'   Minimum number of samples per group to consider for inclusion in
#'   plot.
#' @return `ggplot`.
#'
#' @examples
#' data(crispr)
#'
#' ## GeneEffect ====
#' object <- crispr
#' gene <- rownames(object)[[1L]]
#' group <- "subtype"
#' plotTopGeneEffectPerGroup(object, gene = gene, group = group)
NULL



## Updated 2021-07-07.
`plotTopGeneEffectPerGroup,GeneEffect` <-  # nolint
    function(
        object,
        gene,
        group = c(
            "subtype",
            "lineage",
            "lineageMolecularSubtype",
            "lineageSubSubtype",
            "lineageSubtype",
            "primaryDisease"
        ),
        n = 10L,
        minNPerGroup = 3L
    ) {
        validObject(object)
        assert(
            isString(gene),
            isInt(n), isPositive(n),
            isInt(minNPerGroup), isPositive(minNPerGroup)
        )
        group <- match.arg(group)
        se <- as(object, "SummarizedExperiment")
        rownames <- mapGenesToRownames(
            object = se,
            genes = gene,
            strict = TRUE
        )
        assert(isString(rownames))
        ## Prepare the data frame, containing gene effect values.
        se <- se[rownames, , drop = FALSE]
        geneName <- as.character(rowData(se)[["geneName"]])
        assert(isString(geneName))
        mat <- assay(se, i = "effect")
        rownames(mat) <- as.character(rowData(se)[["geneName"]])
        data <- melt(mat)
        ## Join the "group" value of interest.
        colData <- colData(object)[, group, drop = FALSE]
        colnames(colData)[colnames(colData) == group] <- "group"
        colData[["colname"]] <- rownames(colData)
        rownames(colData) <- NULL
        data <- leftJoin(x = data, y = colData, by = "colname")
        ## Ensure we drop data points that don't contain sufficient metadata.
        data <- data[complete.cases(data), ]
        split <- split(data, f = data[["group"]])
        summary <- DataFrame(
            "group" = names(split),
            "median" = median(split[, "value"]),
            "mean" = mean(split[, "value"]),
            "n" = vapply(
                X = split,
                FUN = nrow,
                FUN.VALUE = integer(1L)
            ),
            row.names = names(split)
        )
        keep <- summary[["n"]] >= minNPerGroup
        summary <- summary[keep, ]
        summary <- summary[order(summary[["median"]]), ]
        summary <- head(summary, n = n)
        data <- data[data[["group"]] %in% summary[["group"]], ]
        data <- as_tibble(data)
        p <- ggplot(
            data = data,
            mapping = aes(
                x = !!sym("value"),
                y = reorder(
                    !!sym("group"),
                    !!sym("value"),
                    median
                ),
                color = !!sym("group"),
                fill = !!sym("group")
            )
        ) +
            geom_jitter(
                show.legend = FALSE,
                size = 2L
            ) +
            geom_boxplot(
                fill = NA,
                outlier.shape = NA,
                show.legend = FALSE,
                size = 0.5
            ) +
            scale_y_discrete(limits = rev) +
            labs(
                title = geneName,
                subtitle = "ranked by median dependency score",
                x = "gene effect",
                y = NULL
            )
        p
    }



#' @rdname plotTopGeneEffectPerGroup
#' @export
setMethod(
    f = "plotTopGeneEffectPerGroup",
    signature = signature("GeneEffect"),
    definition = `plotTopGeneEffectPerGroup,GeneEffect`
)
