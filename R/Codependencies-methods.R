## NOTE Consider adding parameterized support for multiple genes into
## `geneName1` which may be useful for quickly testing multiple genes of
## interest per condition.



## nolint start

#' Calculate co-dependency scores
#'
#' @name Codependencies
#' @note Updated 2022-08-05.
#'
#' @inheritParams params
#'
#' @param geneName1 `character(1)`.
#' Gene name.
#'
#' @param geneName2 `character(1)` or `NULL`.
#' Gene name or `NULL`.
#' If `NULL`, calculate correlations against all genes.
#'
#' @return `Codependencies`.
#'
#' @seealso
#' - [How to download codependency scores](https://forum.depmap.org/t/down-load-of-co-dependencies/175/)
#' - [Python script for processing of all codependency scores](https://gist.github.com/pgm/ac2ac4c664ef81200ce49133cc4cee02)
#'
#' @examples
#' data(crispr)
#'
#' ## GeneEffect ====
#' object <- crispr
#'
#' geneNames <- as.character(rowData(object)[["geneName"]])
#' geneName1 <- geneNames[[1L]]
#' geneName2 <- geneNames[[2L]]
#'
#' ## Calculate all co-dependencies for a gene of interest.
#' x <- Codependencies(
#'     object = object,
#'     geneName1 = geneName1,
#'     geneName2 = NULL
#' )
#' print(head(x))
#'
#' ## Lineage restrict and compare 2 genes.
#' diseases <- as.character(colData(object)[["cellosaurusNcItDisease"]])
#' disease <- diseases[[1L]]
#' keep <- colData(object)[["cellosaurusNcItDisease"]] %in% disease
#' object <- object[, keep]
#' x <- Codependencies(
#'     object = object,
#'     geneName1 = geneName1,
#'     geneName2 = geneName2
#' )
#' print(x)

## nolint end



## Updated 2022-08-05.
`Codependencies,GeneEffect` <- # nolint
    function(object, geneName1, geneName2 = NULL) {
        assert(
            validObject(object),
            isString(geneName1),
            isString(geneName2, nullOK = TRUE),
            hasNoDuplicates(rowData(object)[["geneName"]])
        )
        effect <- assay(object, i = "effect")
        assert(is.matrix(effect))
        rownames(effect) <- as.character(rowData(object)[["geneName"]])
        if (isString(geneName2)) {
            xIdx <- which(rowData(object)[["geneName"]] %in% geneName1)
            assert(
                isInt(xIdx),
                msg = sprintf(
                    "Invalid gene name: {.var %s}.",
                    geneName1
                )
            )
            yIdx <- which(rowData(object)[["geneName"]] %in% geneName2)
            assert(
                isInt(yIdx),
                msg = sprintf(
                    "Invalid gene name: {.var %s}.",
                    geneName2
                )
            )
            x <- effect[xIdx, , drop = TRUE]
            y <- effect[yIdx, , drop = TRUE]
            pearson <- cor(x = x, y = y, method = "pearson")
            df <- DataFrame(
                "geneName1" = geneName1,
                "geneName2" = geneName2,
                "pearson" = pearson
            )
        } else {
            idx <- which(rowData(object)[["geneName"]] %in% geneName1)
            assert(
                isInt(idx),
                msg = sprintf(
                    "Invalid gene name: {.var %s}.",
                    geneName1
                )
            )
            pearson <- apply(
                X = effect[-idx, ],
                MARGIN = 1L,
                FUN = cor,
                y = effect[idx, , drop = TRUE],
                method = "pearson",
                simplify = TRUE
            )
            assert(is.numeric(pearson))
            pearson <- sort(pearson, decreasing = TRUE)
            df <- DataFrame(
                "geneName1" = geneName1,
                "geneName2" = names(pearson),
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
        new(Class = "Codependencies", df)
    }



#' @rdname Codependencies
#' @export
setMethod(
    f = "Codependencies",
    signature = signature(object = "GeneEffect"),
    definition = `Codependencies,GeneEffect`
)
