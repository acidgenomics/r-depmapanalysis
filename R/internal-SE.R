#' Make SummarizedExperiment object from CCLE data
#'
#' @note Updated 2022-03-09.
#' @noRd
.makeCcleSE <-
    function(dataset,
             assayKey,
             assayName,
             class) {
        assert(
            isString(dataset),
            isString(assayKey),
            isString(assayName),
            isString(class)
        )
        url <- datasets[[dataset]][["ccle"]][[assayKey]][["url"]]
        assert(isAURL(url))
        mat <- .importDataFile(
            url = url,
            rownamesCol = 1L,
            return = "matrix"
        )
        assays <- list(mat)
        names(assays) <- assayName
        se <- .makeSummarizedExperiment(
            dataset = dataset,
            assays = assays,
            transposeAssays = TRUE,
            class = class
        )
        se
    }



#' Make SummarizedExperiment object (from DepMap or CCLE data)
#'
#' @note Updated 2022-03-09.
#' @noRd
.makeSummarizedExperiment <-
    function(dataset,
             assays,
             transposeAssays = FALSE,
             metadata = list(),
             class) {
        assert(
            is.list(assays),
            isFlag(transposeAssays),
            is.list(metadata)
        )
        retiredGenes <- NULL
        missingCells <- NULL
        ## Assays --------------------------------------------------------------
        assays <- lapply(
            X = assays,
            i = Reduce(
                f = intersect,
                x = lapply(X = assays, FUN = rownames)
            ),
            j = Reduce(
                f = intersect,
                x = lapply(X = assays, FUN = colnames)
            ),
            FUN = function(x, i, j) {
                x[i, j, drop = FALSE]
            }
        )
        if (isTRUE(transposeAssays)) {
            assays <- lapply(X = assays, FUN = t)
        }
        ## Row data (gene annotations) -----------------------------------------
        rowData <- EntrezGeneInfo(
            organism = "Homo sapiens",
            taxonomicGroup = "Mammalia"
        )
        assert(
            is(rowData, "EntrezGeneInfo"),
            isSubset(c("geneId", "geneName"), colnames(rowData))
        )
        rowData <- as(rowData, "DataFrame")
        ## Extract the NCBI Entrez identifiers from the row names.
        match <- stri_match_first_regex(
            str = rownames(assays[[1L]]),
            pattern = "^(.+)_([0-9]+)$"
        )
        entrezIds <- as.integer(match[, 3L, drop = TRUE])
        assert(
            !any(is.na(entrezIds)),
            msg = "Failed to extract Entrez identifiers from rownames."
        )
        ## Retired NCBI Entrez gene identifiers will return `NA` here.
        ## These correspond to the rows we want to keep in assays.
        idx <- match(
            x = entrezIds,
            table = as.integer(rowData[["geneId"]])
        )
        if (any(is.na(idx))) {
            retiredGenes <-
                sort(match[which(is.na(idx)), 1L, drop = TRUE])
            alertWarning(sprintf(
                "%d retired NCBI Entrez %s in data set: %s.",
                length(retiredGenes),
                ngettext(
                    n = length(retiredGenes),
                    msg1 = "identifier",
                    msg2 = "identifiers"
                ),
                toInlineString(retiredGenes, n = 5L)
            ))
        }
        assays <- lapply(
            X = assays,
            i = !is.na(idx),
            FUN = function(x, i) {
                x[i, , drop = FALSE]
            }
        )
        ## Now we need to resize the rowData to match assays.
        match <- stri_match_first_regex(
            str = rownames(assays[[1L]]),
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
        rownames(rowData) <- match[, 1L, drop = TRUE]
        ## Column data (cell line annotations) ---------------------------------
        colData <- .importCellLineSampleData(dataset = dataset)
        assert(areIntersectingSets(colnames(assays[[1L]]), rownames(colData)))
        if (!isSubset(colnames(assays[[1L]]), rownames(colData))) {
            missingCells <- setdiff(colnames(assays[[1L]]), rownames(colData))
            alertWarning(sprintf(
                "%d missing cell %s in {.var %s}: %s.",
                length(missingCells),
                ngettext(
                    n = length(missingCells),
                    msg1 = "line",
                    msg2 = "lines"
                ),
                "colData",
                toInlineString(missingCells, n = 5L)
            ))
        }
        assays <- lapply(
            X = assays,
            j = intersect(
                x = colnames(assays[[1L]]),
                y = rownames(colData)
            ),
            FUN = function(x, j) {
                x[, j, drop = FALSE]
            }
        )
        args <- list(
            "assays" = assays,
            "rowData" = rowData,
            "colData" = colData,
            "metadata" = append(
                x = metadata,
                values = list(
                    "dataset" = dataset,
                    "missingCells" = missingCells,
                    "packageName" = .pkgName,
                    "packageVersion" = .pkgVersion,
                    "retiredGenes" = retiredGenes,
                    "yaml" = datasets[[dataset]]
                )
            )
        )
        args <- Filter(Negate(is.null), args)
        se <- do.call(what = makeSummarizedExperiment, args = args)
        new(Class = class, se)
    }
