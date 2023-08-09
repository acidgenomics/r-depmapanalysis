## FIXME Consider just defaulting to the new Sanger dataset?
## FIXME Only interested in cells that map to cellosaurus, broad, and sanger...



#' Import DepMap proteomics data
#'
#' @export
#' @note Updated 2023-08-09.
#'
#' @section Nusinow et al. 2020 (Gygi lab) dataset:
#'
#' Citation:
#' https://doi.org/10.1016/j.cell.2019.12.023
#'
#' @section UniProt:
#'
#' Note that `"sp"` for UniProtKB/Swiss-Prot and `"tr"` for UniProtKB/TrEMBL.
#'
#' See also:
#' - https://www.uniprot.org/help/fasta-headers
#'
#' @return `DepMapProteomics`.
#'
#' @seealso
#' - https://depmap.org/portal/download/all/?releasename=Proteomics
#' - https://gygi.hms.harvard.edu/publications/ccle.html
DepMapProteomics <-  # nolint
    function(dataset = c("goncalves_2022", "nusinow_2020")) {
        .fun <- switch(
            EXPR = match.arg(dataset),
            "goncalves_2022" = .importGoncalves2022,
            "nusinow_2020" = .importNusinow2020
        )
        .fun()
    }



#' Import the Goncalves et al 2022 proteomics dataset
#'
#' @note Updated 2023-03-01.
#' @noRd
#'
#' @seealso
#' - https://doi.org/10.1016/j.ccell.2022.06.010
#' - https://cellmodelpassports.sanger.ac.uk/downloads
#' - https://depmap.sanger.ac.uk/documentation/datasets/proteomics/
.importGoncalves2022 <- function() {
    baseUrl <- pasteURL(.extdataUrl, "proteomics", "goncalves-2022")
    date <- "20221214"
    assayUrls <- c(
        "log2" = pasteURL(
            baseUrl, date,
            paste0("protein-matrix-averaged-", date, ".tsv")
        ),
        "zscore" = pasteURL(
            baseUrl, date,
            paste0("protein-matrix-averaged-zscore-", date, ".tsv")
        )
    )
    .importAssay <- function(url) {
        con <- .cacheURL(url)
        df <- import(con = con, colnames = FALSE, skip = 3L)
        assert(identical(df[1L, 1L], "22RV1"))
        headers <- import(con = con, format = "lines", nMax = 2L, quiet = TRUE)
        headers <- strsplit(headers, split = "\t", fixed = TRUE)
        rowData <- DataFrame(
            "uniprotId" = headers[[1L]][3L:length(headers[[1L]])],
            "geneName" = headers[[2L]][3L:length(headers[[2L]])]
        )
        colData <- df[, c(1L:2L)]
        colnames(colData) <- c("cellLineName", "sangerModelId")
        assert(
            hasNoDuplicates(rowData[["uniprotId"]]),
            hasNoDuplicates(colData[["sangerModelId"]])
        )
        assay <- t(as.matrix(df[, -c(1L:2L)]))
        dimnames(assay) <- list(
            rowData[["uniprotId"]],
            colData[["sangerModelId"]]
        )
        assays <- list(assay)
        se <- SummarizedExperiment(
            assays = assays,
            rowData = rowData,
            colData = colData
        )
        se
    }
    seList <- lapply(X = assayUrls, FUN = .importAssay)
    assays <- lapply(X = seList, FUN = assay)
    rowData <- rowData(seList[[1L]])
    colData <- colData(seList[[1L]])
    metadata <- list(
        "dataset" = "goncalves_2022",
        "packageName" = .pkgName,
        "packageVersion" = .pkgVersion
    )
    se <- makeSummarizedExperiment(
        assays = assays,
        rowData = rowData,
        colData = colData,
        metadata = metadata
    )
    se <- .standardizeGoncalves2022(se)
    new(Class = "DepMapProteomics", se)
}



## FIXME Map to Cellosaurus ID instead.

#' Import the Nusinow et al 2020 proteomics dataset
#'
#' @note Updated 2023-02-28.
#' @noRd
#'
#' @details
#' We've observed connection issues with the Gygy lab website at
#' `"https://gygi.hms.harvard.edu/publications/ccle.html"`, so caching a copy
#' for our pacakge instead.
#'
#' @seealso
#' - https://doi.org/10.1016/j.cell.2019.12.023
#' - https://gygi.hms.harvard.edu/publications/ccle.html
.importNusinow2020 <- function() {
    baseUrl <- pasteURL(.extdataUrl, "proteomics", "nusinow-2020")
    url <- pasteURL(baseUrl, "table-s1-sample-information.csv")
    colData <- import(con = .cacheURL(url))
    colData <- as(colData, "DataFrame")
    colnames(colData) <- camelCase(colnames(colData), strict = TRUE)
    assert(identical(
        sort(colnames(colData)),
        c(
            "ccleCode", "cellLine", "notes",
            "protein10PlexId", "proteinTmtLabel", "tissueOfOrigin"
        )
    ))
    colData <- colData[colData[["protein10PlexId"]] != "0", , drop = FALSE]
    rownames(colData) <- makeNames(paste0(
        colData[["ccleCode"]], "_TenPx",
        autopadZeros(colData[["protein10PlexId"]])
    ))
    url <- pasteURL(baseUrl, "table-s2-protein-quant-normalized.csv")
    ## This step can fail when using readr engine without increasing default
    ## `VROOM_CONNECTION_SIZE`. Using data.table here instead to avoid.
    df <- import(con = .cacheURL(url), engine = "data.table")
    assert(
        isSubset(c("Gene_Symbol", "Uniprot_Acc"), colnames(df)),
        hasNoDuplicates(df[["Uniprot_Acc"]])
    )
    rownames(df) <- makeNames(df[["Uniprot_Acc"]])
    keepRows <- !is.na(df[["Gene_Symbol"]])
    df <- df[keepRows, , drop = FALSE]
    keepCols <- !grepl(pattern = "^TenPx[0-9]{2}_Peptides$", x = colnames(df))
    df <- df[, keepCols, drop = FALSE]
    keepCols <- !grepl(pattern = "^Column[0-9]+$", x = colnames(df))
    df <- df[, keepCols, drop = FALSE]
    assayCols <- grepl(pattern = "_TenPx[0-9]+$", x = colnames(df))
    assay <- df[, assayCols, drop = FALSE]
    assert(all(bapply(X = assay, FUN = is.numeric)))
    assay <- as.matrix(assay)
    rowData <- df[, setdiff(colnames(df), colnames(assay))]
    rowData <- as(rowData, "DataFrame")
    colnames(rowData) <- camelCase(colnames(rowData), strict = TRUE)
    colnames(rowData)[colnames(rowData) == "uniprotAcc"] <- "uniprotId"
    colnames(rowData)[colnames(rowData) == "uniprot"] <- "uniprotName"
    assert(identical(
        sort(colnames(rowData)),
        c(
            "description", "geneSymbol", "groupId",
            "proteinId", "uniprotId", "uniprotName"
        )
    ))
    assays <- list("normalized" = assay)
    metadata <- list(
        "dataset" = "nusinow_2020",
        "packageName" = .pkgName,
        "packageVersion" = .pkgVersion
    )
    se <- makeSummarizedExperiment(
        assays = assays,
        rowData = rowData,
        colData = colData,
        metadata = metadata
    )
    se <- .standardizeNusinow2020(se)
    new(Class = "DepMapProteomics", se)
}



## FIXME Use Sanger metadata and map to Cellosaurus ID.
## FIXME Drop cell lines that aren't at cellosaurus.

#' Standardize the Goncalvez et al 2022 proteomics dataset
#'
#' @note Updated 2023-08-09.
#' @noRd
.standardizeGoncalvez2022 <- function(object) {
    assert(is(object, "SummarizedExperiment"))
    currentDataset <- .currentDataset
    assert(isString(currentDataset))
    alert(sprintf(
        "Standardizing {.var %s} annotations to DepMap {.var %s}.",
        "goncalves_2022", currentDataset
    ))
    assert(isString(currentDataset))
    ## FIXME Bind richer data using uniprotId matching.
    rowData <- rowData(object)
    cd1 <- colData(object)
    cd1[["cellLineName"]] <- NULL
    ## FIXME Use sanger cell line metadata here instead.
    ## FIXME Also consider dropping cell lines not in Broad DepMap here too.
    cd2 <- .importBroadModelInfo()
    cd2 <- cd2[!is.na(cd2[["sangerModelId"]]), , drop = FALSE]
    cd <- leftJoin(x = cd1, y = cd2, by = "sangerModelId")
    ## FIXME Drop cells without a cellosaurusId?
    assert(
        identical(cd[["sangerModelId"]], cd1[["sangerModelId"]]),
        !any(is.na(cd[["depmapId"]]))
    )
    ## FIXME Error if we have any messing DepMapIDs...
    cd <- cd[, sort(colnames(cd))]
    colData(object) <- cd
    object <- object[, sort(colnames(object))]
    object
}



## FIXME Need to improve the rowData standardization here.

#' Standardize the Nusinow et al 2020 proteomics dataset
#'
#' @note Updated 2023-08-09.
#' @noRd
.standardizeNusinow2020 <- function(object) {
    assert(is(object, "SummarizedExperiment"))
    currentDataset <- .currentDataset
    assert(isString(currentDataset))
    alert(sprintf(
        "Standardizing {.var %s} annotations to DepMap {.var %s}.",
        "nusinow_2020", currentDataset
    ))
    ## Row data ----------------------------------------------------------------
    rowData <- rowData(object)
    colnames(rowData)[
        colnames(rowData) == "description"] <- "proteinDescription"
    colnames(rowData)[
        colnames(rowData) == "geneSymbol"] <- "geneName"
    ## FIXME Move this to AcidGenomes
    ## FIXME Also make a similar function for NcbiGeneInfo return.
    .mapGeneNamesToHgncIds <- function(hgnc, geneNames) {
        assert(is(hgnc, "HGNC"), isCharacter(geneNames))
        hgnc <- as(hgnc, "DataFrame")
        rownames(hgnc) <- NULL
        hgnc <- hgnc[, c("hgncId", "symbol", "prevSymbol", "aliasSymbol")]
        uniqueGeneNames <- unique(geneNames)
        map <- DataFrame("symbol" = character(), "hgncId" = character())
        ## First, match against current gene symbol.
        idx1 <- match(x = uniqueGeneNames, table = hgnc[["symbol"]])
        map1 <- hgnc[idx1, c("symbol", "hgncId")]
        map <- rbind(map, map1)
        ## Second, match against previous gene symbol.
        uniqueGeneNames <- uniqueGeneNames[which(is.na(idx1))]
        ## FIXME This step is slow but works...can we speed up?
        ## FIXME This is too slow to be generally useful...consider unlisting
        ## with an index position, similar to our cellosaurus match pool?
        ## FIXME Expand this as pool instead and track back to index position.
        xxx <- parallel::mclapply(
            X = uniqueGeneNames,
            prevSymbol = hgnc[["prevSymbol"]],
            FUN = function(x, prevSymbol) {
                which(bapply(
                    X = prevSymbol,
                    FUN = function(table) {
                        x %in% table
                    }
                ))
            }
        )
        ## FIXME Here's how to return the difficult to match gene symbols.
        ## > vec <- sort(unique(decode(zzz[["geneName"]])))
        ## > hits <- lapply(
        ## >     X = vec,
        ## >     prevSymbol = hgnc2[["prevSymbol"]],
        ## >     FUN = function(x, prevSymbol) {
        ## >         idx <- which(bapply(
        ## >             X = prevSymbol,
        ## >             FUN = function(table) {
        ## >                 x %in% table
        ## >             }
        ## >         ))
        ## >         if (length(idx) == 0L) {
        ## >             return(NULL)
        ## >         }
        ## >         hgnc2[["hgncId"]][[idx]]
        ## >     }
        ## > )
        ## > names(hits) <- vec
        ## Third, match against aliases.
        ## FIXME Need to think about return matching like this...
        idx <- match(x = geneNames, table = uniqueGeneNames)
        xxx <- uniqueGeneNames[idx]
        xxx
    }
    hgnc <- HGNC()
    geneNames <- decode(rowData[["geneName"]])
    ## Column data -------------------------------------------------------------
    cd1 <- colData(object)
    colnames(cd1)[colnames(cd1) == "ccleCode"] <- "ccleName"
    cd1[["cellLine"]] <- NULL
    cd1[["notes"]] <- NULL
    cd1[["tissueOfOrigin"]] <- NULL
    cd2 <- .importBroadModelInfo()
    cd2 <- cd2[!is.na(cd2[["ccleName"]]), , drop = FALSE]
    cd <- leftJoin(x = cd1, y = cd2, by = "ccleName")
    assert(
        identical(cd[["ccleName"]], cd1[["ccleName"]]),
        !any(is.na(cd[["depmapId"]]))
    )
    cd <- cd[, sort(colnames(cd))]
    colData(object) <- cd
    colnames(object) <- makeNames(paste0(
        cd[["depmapId"]], "_TENPX",
        autopadZeros(cd[["protein10PlexId"]])
    ))
    object <- object[, sort(colnames(object))]
    object
}
