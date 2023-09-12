## FIXME Can we remap old NCBI gene identifiers rather than excluding?
## "TIAF1_9220", now 399687.
## https://www.ncbi.nlm.nih.gov/gene/?term=9220
## FIXME May be able to handle this in an AcidGenomes package update.



#' Import a Broad DepMap data file
#'
#' @note Updated 2023-08-09.
#' @noRd
.importBroadDataFile <-
    function(url,
             format = c("csv", "tsv"),
             colnames = TRUE,
             rownameCol = NULL,
             engine = getOption(
                 x = "acid.import.engine",
                 default = "base"
             ),
             return = c("DFrame", "matrix")) {
        assert(
            isAURL(url),
            isFlag(colnames),
            isScalar(rownameCol) || is.null(rownameCol),
            isString(engine)
        )
        format <- match.arg(format)
        return <- match.arg(return)
        ## Engine overrides for malformed DepMap files.
        malformedIds <- c(31316011L, 35020903L)
        if (isSubset(x = as.integer(basename(url)), y = malformedIds)) {
            requireNamespaces("data.table")
            engine <- "data.table"
        }
        tmpfile <- .cacheURL(url = url)
        df <- import(
            con = tmpfile,
            format = format,
            rownameCol = rownameCol,
            colnames = colnames,
            engine = engine
        )
        out <- switch(
            EXPR = return,
            "DFrame" = as(df, "DFrame"),
            "matrix" = as.matrix(df)
        )
        out <- makeDimnames(out)
        out
    }



#' Import a Broad DepMap file containing gene identifiers
#'
#' @note Updated 2023-08-09.
#' @noRd
.importBroadGeneDataFile <-
    function(url) {
        df <- .importBroadDataFile(
            url = url,
            colnames = TRUE,
            engine = "base",
            return = "DFrame"
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
            df <- as(df, "DFrame")
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
            is(df, "DFrame"),
            identical(colnames(df), c("geneName", "ncbiGeneId"))
        )
        df[["ncbiGeneId"]] <- as.integer(df[["ncbiGeneId"]])
        df <- df[order(df), , drop = FALSE]
        df
    }



#' Import Broad DepMap gene effect data (CRISPR or DEMETER2 RNAi)
#'
#' @note Updated 2023-08-09.
#' @noRd
.importBroadGeneEffect <- # nolint
    function(dataset, class) {
        assert(
            isString(dataset),
            isString(class)
        )
        json <- datasets[[dataset]]
        assert(is.list(json))
        urls <- unlist(x = json[["files"]], recursive = FALSE, use.names = TRUE)
        dict <- list(
            "libraryType" = json[["metadata"]][["library_type"]],
            "releaseDate" = json[["metadata"]][["date"]],
            "scoringMethod" = json[["metadata"]][["scoring_method"]],
            "transposeAssays" = json[["metadata"]][["transpose_assays"]]
        )
        assert(
            isString(dict[["libraryType"]]),
            isString(dict[["releaseDate"]]),
            isString(dict[["scoringMethod"]]),
            isFlag(dict[["transposeAssays"]]),
            allAreURLs(urls)
        )
        h1(sprintf("{.cls %s}: {.var %s}", class, dataset))
        dl(c(
            "libraryType" = dict[["libraryType"]],
            "scoringMethod" = dict[["scoringMethod"]],
            "releaseDate" = dict[["releaseDate"]]
        ))
        if (identical(dataset, "demeter2_data_v6")) {
            ## DEMETER2 RNAi dataset.
            urls <- list(
                "assays" = list(
                    "effect" =
                        urls[["D2_combined_gene_dep_scores.csv"]],
                    "sd" =
                        urls[["D2_combined_gene_dep_score_SDs.csv"]]
                ),
                "metadata" = list(
                    "controlCommonEssentials" =
                        urls[["Hart-pos-controls.csv"]],
                    "controlNonessentials" =
                        urls[["Hart-neg-controls.csv"]]
                )
            )
        } else if (isSubset("CRISPR_gene_effect.csv", names(urls))) {
            ## Legacy CRISPR pipeline.
            urls <- list(
                "assays" = list(
                    "effect" =
                        urls[["CRISPR_gene_effect.csv"]],
                    "probability" =
                        urls[["CRISPR_gene_dependency.csv"]]
                ),
                "metadata" = list(
                    "commonEssentials" =
                        urls[["CRISPR_common_essentials.csv"]],
                    "controlCommonEssentials" =
                        urls[["common_essentials.csv"]],
                    "controlNonessentials" =
                        urls[["nonessentials.csv"]]
                )
            )
        } else {
            ## Current CRISPR pipeline.
            assert(isSubset("CRISPRGeneEffect.csv", names(urls)))
            urls <- list(
                "assays" = list(
                    "effect" =
                        urls[["CRISPRGeneEffect.csv"]],
                    "probability" =
                        urls[["CRISPRGeneDependency.csv"]]
                ),
                "metadata" = list(
                    "commonEssentials" =
                        urls[["CRISPRInferredCommonEssentials.csv"]],
                    "controlCommonEssentials" =
                        urls[["AchillesCommonEssentialControls.csv"]],
                    "controlNonessentials" =
                        urls[["AchillesNonessentialControls.csv"]]
                )
            )
        }
        assert(allAreURLs(unlist(urls, recursive = TRUE)))
        assays <- lapply(
            X = urls[["assays"]],
            FUN = .importBroadDataFile,
            format = "csv",
            rownameCol = 1L,
            colnames = TRUE,
            return = "matrix"
        )
        metadata <- lapply(
            X = urls[["metadata"]],
            FUN = .importBroadGeneDataFile
        )
        metadata <- append(
            x = metadata,
            values = list(
                "libraryType" = dict[["libraryType"]],
                "releaseDate" = dict[["releaseDate"]],
                "scoringMethod" = dict[["scoringMethod"]]
            )
        )
        se <- .makeBroadSE(
            dataset = dataset,
            assays = assays,
            transposeAssays = dict[["transposeAssays"]],
            metadata = metadata,
            class = class
        )
        se
    }



#' Import Broad DepMap cell line model info
#'
#' Sample metadata now indicates that there are merged cells we should drop
#' from analysis (e.g. ACH-002260).
#'
#' @note Updated 2023-09-12.
#' @noRd
.importBroadModelInfo <-
    function(dataset) {
        assert(isString(dataset))
        url <- datasets[[dataset]][["files"]][["Model.csv"]]
        assert(isAURL(url))
        broad <- .importBroadDataFile(
            url = url,
            format = "csv",
            rownameCol = NULL,
            colnames = TRUE,
            return = "DFrame"
        )
        assert(is(broad, "DFrame"))
        cello <- Cellosaurus()
        assert(
            is(cello, "Cellosaurus"),
            isSubset(
                c(
                    "accession",
                    "cellLineName",
                    "depmapId",
                    "sangerModelId"
                ),
                colnames(cello)
            )
        )
        cello <- excludeContaminatedCells(cello)
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
        assert(
            is(cello, "Cellosaurus"),
            is(broad, "DFrame"),
            validObject(cello)
        )
        df <- as.DataFrame(list(
            "cellLineName" = decode(cello[["cellLineName"]]),
            "cellosaurusId" = decode(cello[["accession"]]),
            "depmapId" = decode(cello[["depmapId"]]),
            "sangerModelId" = decode(cello[["sangerModelId"]]),
            "cellosaurus" = cello,
            "broad" = broad
        ))
        rownames(df) <- makeNames(df[["depmapId"]])
        metadata(df) <- list("excludedCells" = ids[["setdiff"]])
        df
    }

formals(.importBroadModelInfo)[["dataset"]] <- .currentBroadDataset



#' Import Broad DEMETER2 RNAi cell line model info
#'
#' @note Updated 2023-08-09.
#' @noRd
#'
#' @details
#' Matching the cell lines here by CCLE identifier.
.importDemeter2ModelInfo <- function() {
    url <- datasets[["demeter2_data_v6"]][["files"]][["sample_info.csv"]]
    assert(isAURL(url))
    d2 <- .importBroadDataFile(
        url = url,
        format = "csv",
        rownameCol = NULL,
        colnames = TRUE,
        return = "DFrame"
    )
    assert(is(d2, "DFrame"))
    broad <- .importBroadModelInfo()
    assert(is(broad, "DFrame"))
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
    metadata(df) <- list("excludedCells" = ids[["setdiff"]])
    df
}



#' Make SummarizedExperiment object from Broad DepMap data
#'
#' @note Updated 2023-08-09.
#' @noRd
.makeBroadSE <-
    function(dataset,
             assays,
             transposeAssays = FALSE,
             metadata = list(),
             class) {
        assert(
            isString(dataset),
            is.list(assays),
            isFlag(transposeAssays),
            is.list(metadata),
            isString(class)
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
        excludedGenes <- character()
        ## FIXME Need to add an option to slot transcript-level metadata here.
        rowData <- NcbiGeneInfo(
            organism = "Homo sapiens",
            taxonomicGroup = "Mammalia"
        )
        assert(
            is(rowData, "NcbiGeneInfo"),
            isSubset(c("geneId", "geneName"), colnames(rowData))
        )
        rowData <- as(rowData, "DFrame")
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
            excludedGenes <-
                sort(match[which(is.na(idx)), 1L, drop = TRUE])
            alertWarning(sprintf(
                "%d NCBI gene %s to exclude in data set: %s.",
                length(excludedGenes),
                ngettext(
                    n = length(excludedGenes),
                    msg1 = "identifier",
                    msg2 = "identifiers"
                ),
                toInlineString(excludedGenes, n = 5L)
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
        excludedCells <- character()
        if (identical(dataset, "demeter2_data_v6")) {
            colData <- .importDemeter2ModelInfo()
        } else {
            colData <- .importBroadModelInfo(dataset = dataset)
        }
        assert(
            areIntersectingSets(colnames(assays[[1L]]), rownames(colData)),
            !anyNA(colData[["cellLineName"]])
        )
        if (!isSubset(colnames(assays[[1L]]), rownames(colData))) {
            excludedCells <- setdiff(colnames(assays[[1L]]), rownames(colData))
            alertWarning(sprintf(
                "%d cell %s to exclude in {.var %s}: %s.",
                length(excludedCells),
                ngettext(
                    n = length(excludedCells),
                    msg1 = "line",
                    msg2 = "lines"
                ),
                "colData",
                toInlineString(excludedCells, n = 5L)
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
                    "excludedCells" = excludedCells,
                    "excludedGenes" = excludedGenes,
                    "packageName" = .pkgName,
                    "packageVersion" = .pkgVersion
                )
            )
        )
        args <- Filter(Negate(is.null), args)
        se <- do.call(what = makeSummarizedExperiment, args = args)
        rownames(se) <- decode(rowData(se)[["geneId"]])
        colnames(se) <- decode(colData(se)[["cellosaurusId"]])
        se <- se[
            order(as.integer(rownames(se))),
            order(colnames(se)),
            drop = FALSE
        ]
        new(Class = class, se)
    }



#' Make a SummarizedExperiment from Broad DepMap with a single assay
#'
#' @note Updated 2023-08-09.
#' @noRd
.makeBroadSingleAssaySE <- function(file, assayName, class) {
    dataset <- .currentBroadDataset
    json <- datasets[[dataset]]
    assert(
        isString(dataset),
        isString(file),
        isString(assayName),
        isString(class),
        is.list(json)
    )
    dict <- list(
        "releaseDate" = json[["metadata"]][["date"]],
        "transposeAssays" = json[["metadata"]][["transpose_assays"]]
    )
    assert(
        isString(dict[["releaseDate"]]),
        isFlag(dict[["transposeAssays"]])
    )
    url <- json[["files"]][[file]]
    assert(isAURL(url))
    ## FIXME This step is crashing RStudio for DepMapTxExpression.
    ## https://figshare.com/ndownloader/files/40449689
    assay <- .importBroadDataFile(
        url = url,
        format = "csv",
        rownameCol = 1L,
        colnames = TRUE,
        return = "matrix"
    )
    assays <- list(assay)
    names(assays) <- assayName
    metadata <- list(
        "releaseDate" = dict[["releaseDate"]]
    )
    .makeBroadSE(
        dataset = dataset,
        assays = assays,
        transposeAssays = dict[["transposeAssays"]],
        metadata = metadata,
        class = class
    )
}



#' Standardize the DEMETER2 RNAi dataset
#'
#' @note Updated 2023-08-09.
#' @noRd
.standardizeDemeter2 <- function(object) {
    currentDataset <- .currentBroadDataset
    assert(
        isString(currentDataset),
        is(object, "SummarizedExperiment")
    )
    alert(sprintf(
        "Standardizing DEMETER2 annotations to DepMap {.var %s}.",
        currentDataset
    ))
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
        excludedCells <- colnames(object)[!keep]
        alertWarning(sprintf(
            "%d missing cell %s in {.var %s}: %s.",
            length(excludedCells),
            ngettext(
                n = length(excludedCells),
                msg1 = "line",
                msg2 = "lines"
            ),
            "colData",
            toInlineString(excludedCells, n = 5L)
        ))
        metadata(object)[["excludedCells"]] <- append(
            x = metadata(object)[["excludedCells"]],
            values = excludedCells
        )
        metadata(object)[["excludedCells"]] <-
            sort(unique(metadata(object)[["excludedCells"]]))
        object <- object[, keep]
    }
    colnames(object) <- makeNames(as.character(colData(object)[["depmapId"]]))
    object <- object[, sort(colnames(object))]
    object
}
