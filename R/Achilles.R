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
#' object <- Achilles()
#' print(object)
Achilles <-  # nolint
    function(
        release = NULL,
        rowData = TRUE,
        colData = TRUE
    ) {
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
        ## Sample (i.e. cell line) metadata.
        if (isTRUE(colData)) {
            colData <- .importCellLineSampleData(release = release)
        } else {
            colData <- NULL
        }
        ## Gene metadata.
        if (isTRUE(rowData)) {
            l <- .rowDataFromEntrez(assays = assays)
            assert(
                is.list(l),
                identical(
                    x = names(l),
                    y = c("assays", "retired", "rowData")
                )
            )
            assays <- l[["assays"]]
            retired <- l[["retired"]]
            rowData <- l[["rowData"]]
        } else {
            retired <- NULL
            rowData <- NULL
        }
        metadata <- list(
            "commonEssentials" =
                .importCommonEssentials(release = release),
            "controlCommonEssentials" =
                .importControlCommonEssentials(release = release),
            "controlNonessentials" =
                .importControlNonessentials(release = release),
            "packageVersion" = .pkgVersion,
            "retired" = retired,
            "release" = release
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
