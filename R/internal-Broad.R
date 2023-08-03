#' Import a Broad DepMap file containing gene identifiers
#'
#' @note Updated 2023-03-08.
#' @noRd
.importBroadGeneDataFile <-
    function(url) {
        df <- .importDataFile(
            url = url,
            colnames = TRUE,
            engine = "base",
            return = "DataFrame"
        )
        if (
            identical(colnames(df), "Essentials") ||
            identical(colnames(df), "Gene") ||
            identical(colnames(df), "gene")
        ) {
            # e.g. DepMap 22Q4 ("Essentials", "Gene") and 22Q2 ("gene").
            df <- stringi::stri_split_fixed(
                str = df[[1L]],
                pattern = " ",
                n = 2L,
                simplify = TRUE
            )
            df <- as(df, "DataFrame")
            colnames(df) <- c("geneName", "ncbiGeneId")
            df[["ncbiGeneId"]] <- gsub(
                pattern = "[\\(\\)]",
                replacement = "",
                x = df[["ncbiGeneId"]]
            )
        } else if (identical(colnames(df), c("Gene_Symbol", "Gene_ID"))) {
            ## e.g. DEMETER2.
            colnames(df) <- c("geneName", "ncbiGeneId")
        } else {
            abort("Unsupported file.")
        }
        assert(
            is(df, "DataFrame"),
            identical(colnames(df), c("geneName", "ncbiGeneId"))
        )
        df[["ncbiGeneId"]] <- as.integer(df[["ncbiGeneId"]])
        df <- df[order(df), , drop = FALSE]
        df
    }



## FIXME Need to add support for 23q2.

#' Import Broad DepMap cell line model info
#'
#' Sample metadata now indicates that there are merged cells we should drop
#' from analysis (e.g. ACH-002260).
#'
#' @note Updated 2023-07-03.
#' @noRd
.importBroadModelInfo <-
    function(dataset) {
        if (identical(dataset, "demeter2_data_v6")) {
            df <- .importDemeter2ModelInfo()
            return(df)
        }
        files <- datasets[[dataset]][["files"]]
        if (isSubset("sample_info.csv", names(files))) {
            ## Legacy CRISPR pipeline.
            fileKey <- "sample_info.csv"
        } else {
            ## Current CRISPR pipeline.
            fileKey <- "Model.csv"
        }
        assert(isSubset(fileKey, names(files)))
        url <- datasets[[dataset]][["files"]][[fileKey]]
        broad <- .importDataFile(
            url = url,
            format = "csv",
            rownameCol = NULL,
            colnames = TRUE,
            return = "DataFrame"
        )
        cello <- Cellosaurus()
        assert(
            is(cello, "Cellosaurus"),
            isSubset(
                c(
                    "accession",
                    "cellLineName",
                    "depmapId",
                    "isProblematic",
                    "sangerModelId"
                ),
                colnames(cello)
            )
        )
        alert("Filtering cell lines annotated as 'problematic' by Cellosaurus.")
        cello <- cello[!cello[["isProblematic"]], , drop = FALSE]
        ids <- list()
        ids[["broad"]] <- broad[[1L]]
        ids[["cello"]] <- decode(cello[["depmapId"]])
        ids[["intersect"]] <- sort(intersect(
            x = na.omit(ids[["broad"]]),
            y = na.omit(ids[["cello"]])
        ))
        ids[["setdiff"]] <- sort(setdiff(
            x = na.omit(ids[["broad"]]),
            y = na.omit(ids[["cello"]])
        ))
        assert(allAreMatchingFixed(x = ids[["intersect"]], pattern = "ACH-"))
        broad <- broad[
            match(x = ids[["intersect"]], table = ids[["broad"]]),
            ,
            drop = FALSE
        ]
        cello <- cello[
            match(x = ids[["intersect"]], table = ids[["cello"]]),
            ,
            drop = FALSE
        ]
        cello <- droplevels2(cello)
        df <- DataFrame(
            "cellLineName" = decode(cello[["cellLineName"]]),
            "cellosaurusId" = decode(cello[["accession"]]),
            "depmapId" = decode(cello[["depmapId"]]),
            "sangerModelId" = decode(cello[["sangerModelId"]]),
            "cellosaurus" = I(cello),
            "broad" = I(broad),
            row.names = makeNames(decode(cello[["depmapId"]]))
        )
        metadata(df) <- list("missingCells" = ids[["setdiff"]])
        df
    }

formals(.importBroadModelInfo)[["dataset"]] <-
    .formalsList[["dataset"]][[1L]]



#' Import Broad DEMETER2 RNAi cell line model info
#'
#' @note Updated 2023-03-08.
#' @noRd
#'
#' @details
#' Matching the cell lines here by CCLE identifier.
.importDemeter2ModelInfo <- function() {
    url <- datasets[["demeter2_data_v6"]][["files"]][["sample_info.csv"]]
    d2 <- .importDataFile(
        url = url,
        format = "csv",
        rownameCol = NULL,
        colnames = TRUE,
        return = "DataFrame"
    )
    broad <- .importBroadModelInfo()
    ids <- list()
    ids[["d2"]] <- d2[[1L]]
    ids[["d2"]][ids[["d2"]] == "COLO699_LUNG"] <-
        "CHL1DM_SKIN"
    ids[["d2"]][ids[["d2"]] == "GISTT1_GASTROINTESTINAL_TRACT"] <-
        "GISTT1_STOMACH"
    ids[["d2"]][ids[["d2"]] == "MB157_BREAST"] <-
        "MDAMB157_BREAST"
    ids[["broad"]] <- broad[["broad"]][["CCLEName"]]
    ids[["intersect"]] <- sort(intersect(
        x = na.omit(ids[["d2"]]),
        y = na.omit(ids[["broad"]])
    ))
    ids[["setdiff"]] <- sort(setdiff(
        x = na.omit(ids[["d2"]]),
        y = na.omit(ids[["broad"]])
    ))
    d2 <- d2[
        match(x = ids[["intersect"]], table = ids[["d2"]]),
        ,
        drop = FALSE
    ]
    broad <- broad[
        match(x = ids[["intersect"]], table = ids[["broad"]]),
        ,
        drop = FALSE
    ]
    df <- broad
    df[["demeter2"]] <- d2
    rownames(df) <- makeNames(d2[[1L]])
    metadata(df) <- list("missingCells" = ids[["setdiff"]])
    df
}



## FIXME Only keep cells that have a Cellosaurus identifier.
## FIXME Return the gene identifiers as the NCBI gene ID instead.

#' Make SummarizedExperiment object from Broad DepMap data
#'
#' @note Updated 2023-08-03.
#' @noRd
.makeBroadSE <-
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
        retiredGenes <- character()
        rowData <- NcbiGeneInfo(
            organism = "Homo sapiens",
            taxonomicGroup = "Mammalia"
        )
        assert(
            is(rowData, "NcbiGeneInfo"),
            isSubset(c("geneId", "geneName"), colnames(rowData))
        )
        rowData <- as(rowData, "DataFrame")
        ## Extract the NCBI gene identifiers from the row names.
        match <- stri_match_first_regex(
            str = rownames(assays[[1L]]),
            pattern = "^(.+)_([0-9]+)$"
        )
        ncbiGeneIds <- as.integer(match[, 3L, drop = TRUE])
        assert(
            !anyNA(ncbiGeneIds),
            msg = "Failed to extract NCBI gene identifiers from row names."
        )
        ## Retired NCBI gene identifiers will return `NA` here. These correspond
        ## to the rows we want to keep in assays.
        idx <- match(
            x = ncbiGeneIds,
            table = as.integer(rowData[["geneId"]])
        )
        if (anyNA(idx)) {
            retiredGenes <-
                sort(match[which(is.na(idx)), 1L, drop = TRUE])
            alertWarning(sprintf(
                "%d retired NCBI gene %s in data set: %s.",
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
        ncbiGeneIds <- as.integer(match[, 3L, drop = TRUE])
        assert(!anyNA(ncbiGeneIds))
        idx <- match(
            x = ncbiGeneIds,
            table = as.integer(rowData[["geneId"]])
        )
        assert(!anyNA(idx))
        rowData <- rowData[idx, , drop = FALSE]
        rownames(rowData) <- match[, 1L, drop = TRUE]
        ## Column data (cell line annotations) ---------------------------------
        missingCells <- character()
        colData <- .importBroadModelInfo(dataset = dataset)
        assert(
            areIntersectingSets(colnames(assays[[1L]]), rownames(colData)),
            !anyNA(colData[["cellLineName"]])
        )
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
        ok <- !is.na(colSums(assay(se)))
        if (!all(ok)) {
            missingCells <- append(x = missingCells, values = colnames(se)[!ok])
            se <- se[, ok]
        }
        ## FIXME Assert that there are no missing cellLineName values.
        assert(!anyNA(assay(se)))
        ## FIXME Return the rownames as NCBI gene ID instead.
        ## FIXME Return the colnames as Cellosaurus ID instead.
        new(Class = class, se)
    }



## FIXME Rethink this approach, matching to current broad annotations
## instead.

#' Standardize the DEMETER2 RNAi dataset
#'
#' @note Updated 2023-03-08.
#' @noRd
.standardizeDemeter2 <- function(object) {
    assert(is(object, "SummarizedExperiment"))
    currentDataset <- .formalsList[["dataset"]][[1L]]
    alert(sprintf(
        "Standardizing DEMETER2 annotations to DepMap {.var %s}.",
        currentDataset
    ))
    assert(isString(currentDataset))
    cd <- list(
        "x" = colData(object),
        "y" = .importBroadModelInfo(dataset = currentDataset)
    )
    assert(
        isSubset(
            x = c("ccleId", "cellLineName"),
            y = colnames(cd[["x"]])
        ),
        isSubset(
            x = c("ccleName", "depmapId", "strippedCellLineName"),
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
    if (isTRUE(anyNA(colData(object)[["depmapId"]]))) {
        keep <- !is.na(colData(object)[["depmapId"]])
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
    colnames(object) <- makeNames(as.character(colData(object)[["depmapId"]]))
    object <- object[, sort(colnames(object))]
    object
}
