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
        rowData = TRUE,
        colData = TRUE
    ) {
        retired <- NULL
        if (is.null(release)) {
            release <- .currentDepMapRelease
        }
        assert(
            isString(release),
            isFlag(rowData),
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
        ## SummarizedExperiment conventions for NGS data, with samples (i.e.
        ## cells) in the columns and features (i.e. genes) in the rows.
        assays <- lapply(X = assays, FUN = t)
        ## Sample metadata.
        if (isTRUE(colData)) {
            colData <- .importCellLineSampleData(release = release)
        } else {
            colData <- NULL
        }
        ## Gene metadata.
        if (isTRUE(rowData)) {
            ## Extract the NCBI Entrez identifiers from the row names.
            match <- str_match(
                string = rownames(assays[[1L]]),
                pattern = "^(.+)_([0-9]+)$"
            )
            entrezIds <- as.integer(match[, 3L, drop = TRUE])
            assert(!any(is.na(entrezIds)))
            rowData <- EntrezGeneInfo(
                organism = "Homo sapiens",
                taxonomicGroup = "Mammalia"
            )
            assert(
                is(rowData, "EntrezGeneInfo"),
                isSubset("geneId", colnames(rowData))
            )
            rowData <- as(rowData, "DataFrame")
            ## Retired NCBI Entrez gene identifiers will return NA here.
            idx <- match(
                x = entrezIds,
                table = as.integer(rowData[["geneId"]])
            )
            ## Inform the user regarding any retired gene identifiers.
            if (any(is.na(idx))) {
                keep <- !is.na(idx)
                retired <- entrezIds[!keep]
                alertWarning(sprintf(
                    "%d retired NCBI Entrez %s in data set: %s.",
                    length(retired),
                    ngettext(
                        n = length(retired),
                        msg1 = "identifier",
                        msg2 = "identifiers"
                    ),
                    toString(sort(retired), width = 100L)
                ))
                assays <- lapply(
                    X = assays,
                    keep = keep,
                    FUN = function(x, keep) {
                        x[keep, , drop = FALSE]
                    }
                )
                match <- str_match(
                    string = rownames(assays[[1L]]),
                    pattern = "^(.+)_([0-9]+)$"
                )
                entrezIds <- as.integer(match[, 3L, drop = TRUE])
                assert(!any(is.na(entrezIds)))
                idx <- match(
                    x = entrezIds,
                    table = as.integer(rowData[["geneId"]])
                )
                assert(!any(is.na(idx)))
                rowData <- rowData[idx, , drop = FALSE]
            }
        } else {
            rowData <- NULL
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
            "colData" = colData,
            "metadata" = metadata,
            "rowData" = rowData
        )
        args <- Filter(Negate(is.null), args)
        se <- do.call(what = makeSummarizedExperiment, args = args)
        assert(is(se, "SummarizedExperiment"))
        rownames(se) <- tolower(rownames(se))
        colnames(se) <- tolower(colnames(se))
        validObject(se)
        new("Achilles", se)
    }

#' @include AllGlobals.R
formals(Achilles)[["release"]] <- .currentDepMapRelease
