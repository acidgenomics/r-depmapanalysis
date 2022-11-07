## FIXME Need to drop any cell lines with NA cellLineName (e.g. ACH-002260)
## from analysis here.



#' Make SummarizedExperiment object from CCLE data
#'
#' @note Updated 2022-09-21.
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
        url <- datasets[[dataset]][["files"]][["ccle"]][[assayKey]][["url"]]
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



## FIXME Censor any cells that don't contain `cellLineName`.

#' Make SummarizedExperiment object (from DepMap or CCLE data)
#'
#' @note Updated 2022-08-19.
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
            !anyNA(entrezIds),
            msg = "Failed to extract Entrez identifiers from rownames."
        )
        ## Retired NCBI Entrez gene identifiers will return `NA` here.
        ## These correspond to the rows we want to keep in assays.
        idx <- match(
            x = entrezIds,
            table = as.integer(rowData[["geneId"]])
        )
        if (anyNA(idx)) {
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
        assert(!anyNA(entrezIds))
        idx <- match(
            x = entrezIds,
            table = as.integer(rowData[["geneId"]])
        )
        assert(!anyNA(idx))
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
                    "json" = datasets[[dataset]],
                    "missingCells" = missingCells,
                    "packageName" = .pkgName,
                    "packageVersion" = .pkgVersion,
                    "retiredGenes" = retiredGenes
                )
            )
        )
        args <- Filter(Negate(is.null), args)
        se <- do.call(what = makeSummarizedExperiment, args = args)
        if (identical(dataset, "demeter2_data_v6")) {
            se <- .standardizeDemeter2(se)
        }
        new(Class = class, se)
    }



## FIXME This approach is currently erroring, need to rethink...
## .importCellLineSampleData step is problematic...

#' Standardize the DEMETER2 RNAi dataset
#'
#' @note Updated 2022-08-05.
#' @noRd
.standardizeDemeter2 <- function(object) {
    currentDataset <- .formalsList[["dataset"]][[1L]]
    assert(isString(currentDataset))
    cd <- list(
        "x" = colData(object),
        "y" = .importCellLineSampleData(dataset = currentDataset)
    )
    assert(
        isSubset(
            x = c("ccleId", "cellLineName"),
            y = colnames(cd[["x"]])
        ),
        isSubset(
            x = c("ccleName", "depMapId", "strippedCellLineName"),
            y = colnames(cd[["y"]])
        ),
        identical(
            x = intersect(colnames(cd[["x"]]), colnames(cd[["y"]])),
            y = "cellLineName"
        )
    )
    cd[["x"]][[".join"]] <- cd[["x"]][["cellLineName"]]
    cols <- setdiff(
        x = colnames(cd[["x"]]),
        y = c(
            "ccleId",
            "cellLineName",
            "disease",
            "diseaseSubSubtype",
            "diseaseSubtype"
        )
    )
    cd[["x"]] <- cd[["x"]][, cols]
    cd[["y"]][[".join"]] <- cd[["y"]][["strippedCellLineName"]]
    cd[["y"]] <- cd[["y"]][!is.na(cd[["y"]][[".join"]]), ]
    cd <- leftJoin(x = cd[["x"]], y = cd[["y"]], by = ".join")
    cd[[".join"]] <- NULL
    cd <- cd[, sort(colnames(cd))]
    colData(object) <- cd
    if (isTRUE(anyNA(colData(object)[["depMapId"]]))) {
        keep <- !is.na(colData(object)[["depMapId"]])
        missingCells <- colnames(object)[!keep]
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
        metadata(object)[["missingCells"]] <- append(
            x = metadata(object)[["missingCells"]],
            values = missingCells
        )
        metadata(object)[["missingCells"]] <-
            sort(unique(metadata(object)[["missingCells"]]))
        object <- object[, keep]
    }
    colnames(object) <- makeNames(colData(object)[["depMapId"]])
    object <- object[, sort(colnames(object))]
    object
}
