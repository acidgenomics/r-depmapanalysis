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
#' @note Updated 2022-08-05.
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
        if (identical(dataset, "demeter2_data_v6")) {
            se <- .standardizeDemeter2(se)
        }
        new(Class = class, se)
    }



## FIXME Consider matching by `strippedCellLineName`.
## FIXME Rework the mapping:
## Use `cellLineName` for old colData
## Map this to `strippedCellLineName` for new metadata.
## Drop ccleName from legacy metadata.
## Drop cellLineName from joined metadata.

#' Standardize the DEMETER2 RNAi dataset
#'
#' @note Updated 2022-08-05.
#' @noRd
#'
#' We get 7 cell line metadata mapping failures when attempting to match
#' by `ccleId` / `ccleName`. This is because some of the CCLE cell lines have
#' been renamed at DepMap, or have been removed.
#'
#' @section Problematic cell line metadata:
#'
#' - `AZ521_STOMACH` (now `AZ521_SMALL_INTESTINE`).
#' - `COLO699_LUNG`.
#' - `GISTT1_GASTROINTESTINAL_TRACT` (now `GISTT1_STOMACH`).
#' - `KP1NL_PANCREAS`.
#' - `MB157_BREAST`.
#' - `NCIH1339_LUNG`.
#' - `SW527_BREAST`.
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
            x = c("ccleName", "depMapId"),
            y = colnames(cd[["y"]])
        )
    )
    colnames(cd[["x"]])[colnames(cd[["x"]]) == "ccleId"] <- "ccleName"
    assert(identical(
        x = intersect(colnames(cd[["x"]]), colnames(cd[["y"]])),
        y = c("ccleName","cellLineName")
    ))
    cd[["x"]][["cellLineName"]] <- NULL
    cd[["y"]] <- cd[["y"]][!is.na(cd[["y"]][["ccleName"]]), ]
    cd <- leftJoin(x = cd[["x"]], y = cd[["y"]], by = "ccleName")
    cd <- cd[, sort(colnames(cd))]
    colData(object) <- cd


    ## FIXME Need to inform the user about which cells we're dropping from
    ## the analysis. Slot these into metadata.
    bad <- cd[is.na(cd[["depMapId"]]), ]

    ## Drop cells that aren't in DepMap.
    ## Ensure we drop any remaining all NA columns.

    ## FIXME This step is now erroring, due to some elements containing empty
    ## values AAAARGH.
    ## FIXME Now we're hitting a missing values issue...aaaargh
    colnames(object) <- makeNames(cd[["depMapId"]])

    object
}
