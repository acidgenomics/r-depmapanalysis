## NOTE Be sure to update 'entrez2ensembl' mapping data inside 'inst/extdata'
## for a DepMap release update.



#' Import Project Achilles CRISPR gene effect data
#'
#' @section Assays:
#'
#' - `effect`: **CERES data** with principle components strongly related to
#'   known batch effects removed, then shifted and scaled per cell line so the
#'   median nonessential KO effect is 0 and the median essential KO effect is
#'   -1.
#' - `probability`: **Probability** that knocking out the gene has a real
#'   depletion effect using `gene_effect`.
#'
#' @export
#' @note Updated 2021-02-24.
#'
#' @inheritParams params
#'
#' @return `Achilles`.
#'
#' @examples
#' object <- Achilles(rowRanges = FALSE, colData = FALSE)
#' print(object)
Achilles <-  # nolint
    function(
        release = NULL,
        rowRanges = TRUE,
        colData = TRUE
    ) {
        retired <- NULL
        if (is.null(release)) {
            release <- .currentDepMapRelease
        }
        assert(
            isString(release),
            isFlag(rowRanges),
            isFlag(colData)
        )
        ## e.g. "depmap_public_21q1", "depmap_public_20q4v2".
        release <- snakeCase(paste(
            "depmap", "public",
            gsub(pattern = " ", replacement = "", x = tolower(release))
        ))
        ## CSV formatting: genes in columns, cells in rows.
        assays <- list(
            "effect" = .importDataFile(
                fileName = "achilles_gene_effect.csv",
                release = release,
                rownamesCol = 1L,
                return = "matrix"
            ),
            "probability" = .importDataFile(
                fileName = "achilles_gene_dependency.csv",
                release = release,
                rownamesCol = 1L,
                return = "matrix"
            )
        )
        ## Transposing here to match DEMETER2 formatting, and standard
        ## SummarizedExperiment conventions for NGS data.
        assays <- lapply(X = assays, FUN = t)
        ## Sample metadata.
        if (isTRUE(colData)) {
            colData <- .importCellLineSampleData(release = release)
        } else {
            colData <- NULL
        }
        ## Gene metadata.
        if (isTRUE(rowRanges)) {
            entrezIds <- as.integer(str_extract(
                string = rownames(assays[[1L]]),
                pattern = "[0-9]+$"
            ))
            ## FIXME ENSG00000286169 in here?
            e2e <- readRDS(system.file(
                "extdata", "entrez2ensembl.rds",
                package = .pkgName
            ))
            assert(
                is(e2e, "DataFrame"),
                identical(
                    x = colnames(e2e),
                    y = c("entrezId", "ensemblId", "retired")
                ),
                isSubset(entrezIds, e2e[["entrezId"]])
            )
            idx <- match(x = entrezIds, table = e2e[["entrezId"]])
            assert(!any(is.na(idx)))
            e2e <- e2e[idx, , drop = FALSE]
            ## Drop any retired genes from analysis.
            e2e[["retired"]][is.na(e2e[["retired"]])] <- FALSE
            drop <- e2e[["retired"]]
            if (any(drop)) {
                keep <- !drop
                retired <- rownames(assays[[1L]])[drop]
                alertWarning(sprintf(
                    "Dropping %d retired %s: %s.",
                    length(retired),
                    ngettext(
                        n = length(retired),
                        msg1 = "gene",
                        msg2 = "genes"
                    ),
                    toString(retired, width = 200L)
                ))
                e2e <- e2e[keep, , drop = FALSE]
                assays <- lapply(
                    X = assays,
                    FUN = function(assay) {
                        assay[keep, , drop = FALSE]
                    }
                )
            }
            rowRanges <- makeGRangesFromEnsembl(
                organism = "Homo sapiens",
                level = "genes",
                release = 102L,
                ignoreVersion = TRUE,
                synonyms = TRUE  # FIXME REVERT TO TRUE HERE.
            )
            idx <- match(x = e2e[["ensemblId"]], table = names(rowRanges))
            ## If you encounter any mismatches here (e.g. "ENSG00000286169"),
            ## need to update our internal database.
            if (any(is.na(idx))) {
                fail <- e2e[["ensemblId"]][is.na(idx)]
                stop(sprintf(
                    "Failed to match Ensembl genes: %s.",
                    toString(fail, width = 200L)
                ))
            }
            rowRanges <- rowRanges[idx]
            names(rowRanges) <- rownames(assays[[1L]])
        } else {
            rowRanges <- NULL
        }
        commonEssentials <-
            .importCommonEssentials(release = release)
        controlCommonEssentials <-
            .importControlCommonEssentials(release = release)
        controlNonessentials <-
            .importControlNonessentials(release = release)
        metadata <- list(
            "commonEssentials" = commonEssentials,
            "controlCommonEssentials" = controlCommonEssentials,
            "controlNonessentials" = controlNonessentials,
            "packageVersion" = .pkgVersion,
            "release" = release,
            "retired" = retired
        )
        metadata <- Filter(Negate(is.null), metadata)
        args <- list(
            "assays" = assays,
            "rowRanges" = rowRanges,
            "colData" = colData,
            "metadata" = metadata
        )
        args <- Filter(Negate(is.null), args)
        rse <- do.call(what = makeSummarizedExperiment, args = args)
        if (
            is(rse, "SummarizedExperiment") &&
            !is(rse, "RangedSummarizedExperiment")
        ) {
            rse <- as(rse, "RangedSummarizedExperiment")
        }
        assert(is(rse, "RangedSummarizedExperiment"))
        rownames(rse) <- tolower(rownames(rse))
        colnames(rse) <- tolower(colnames(rse))
        validObject(rse)
        new("Achilles", rse)
    }

#' @include AllGlobals.R
formals(Achilles)[["release"]] <- .currentDepMapRelease
