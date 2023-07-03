## FIXME Consider just dropping this data type entirely.
## FIXME Consider reworking the cell line name mapping return.



#' Import DepMap microRNA expression data
#'
#' @details
#' NanoString microRNA panel data.
#'
#' @name DepMapMicroRNA
#' @note Updated 2023-01-26.
#'
#' @return `DepMapMicroRNA`.
#'
#' @examples
#' object <- DepMapMicroRNA()
#' dim(object)
NULL



#' Import microRNA-seq GCT file
#'
#' @note Updated 2023-01-26.
#' @noRd
#'
#' @return `list`.
.importMicroRnaGct <- function(dataset) {
    year <-substr(x = dataset, start = 12L, stop = 15L)
    url <- pasteURL(
        "depmap.org",
        "portal",
        "download",
        "api",
        paste0(
            "download?",
            "file_name", "=",
            "ccle%2Fccle_", year, "%2F", dataset,
            "&",
            "bucket", "=",
            "depmap-external-downloads"
        ),
        protocol = "https"
    )
    tmpfile <- .cacheURL(url)
    assay <- import(
        con = tmpfile,
        format = "gct",
        return = "matrix",
        quiet = FALSE
    )
    assert(is.matrix(assay))
    ## Need to reimport to map NanoString identifiers back to actual
    ## microRNA names.
    rowData <- import(
        con = tmpfile,
        format = "gct",
        return = "data.frame",
        quiet = TRUE
    )
    assert(
        is.matrix(assay),
        is.data.frame(rowData),
        areSetEqual(rownames(assay), rowData[["Name"]]),
        isSubset(c("Name", "Description"), colnames(rowData))
    )
    rowData <- rowData[rownames(assay), c("Name", "Description")]
    rowData <- as(rowData, "DataFrame")
    colnames(rowData) <- c("nanostringId", "mirName")
    rownames(rowData) <- makeNames(tolower(rowData[["mirName"]]))
    rownames(assay) <- rownames(rowData)
    list("assay" = assay, "rowData" = rowData)
}



#' Import miRBase GFF annotations
#'
#' @note Updated 2023-01-25.
#' @noRd
#'
#' @param df GCT expression data frame, containing microRNA annotations.
#'
#' @return `GRangeList`.
.importMirbaseGff <- function(rowData) {
    assert(is(rowData, "DataFrame"))
    url <- pasteURL(
        "www.mirbase.org",
        "ftp",
        "CURRENT",
        "genomes",
        "hsa.gff3",
        protocol = "https"
    )
    gr <- import(.cacheURL(url))
    assert(
        is(gr, "GenomicRanges"),
        identical(
            x = colnames(mcols(gr)),
            y = c(
                "source",
                "type",
                "score",
                "phase",
                "ID",
                "Alias",
                "Name",
                "Derives_from"
            )
        ),
        hasDuplicates(mcols(gr)[["Name"]])
    )
    colnames(mcols(gr)) <- camelCase(colnames(mcols(gr)))
    ## Need to correct "hsa-mir" to "hsa-miR".
    mcols(gr)[["name"]] <- gsub(
        pattern = "\\bmir\\b",
        replacement = "miR",
        x = mcols(gr)[["name"]]
    )
    f <- as.factor(vapply(
        X = strsplit(
            x = mcols(gr)[["name"]],
            split = "-",
            fixed = TRUE
        ),
        FUN = function(x) {
            paste(x[1L:3L], collapse = "-")
        },
        FUN.VALUE = character(1L)
    ))
    grl <- split(gr, f = f)
    assert(is(grl, "GenomicRangesList"))
    ## Add back in the "*-5p" and "-3p" matches, which are inconsistent in
    ## the CCLE NanoString data.
    vec <- intersect(
        x = setdiff(
            x = rowData[["mirName"]],
            y = names(grl)
        ),
        y = mcols(gr)[["name"]])
    gr2 <- gr[match(x = vec, table = mcols(gr)[["name"]])]
    grl2 <- split(x = gr2, f = as.factor(mcols(gr2)[["name"]]))
    grl <- c(grl, grl2)
    names(grl) <- makeNames(tolower(names(grl)))
    empty <- emptyRanges(names = setdiff(rownames(rowData), names(grl)))
    suppressWarnings({
        grl <- c(grl, empty)
    })
    grl <- grl[rownames(rowData)]
    grl
}



## Updated 2023-03-08.

#' @rdname DepMapMicroRNA
#' @export
DepMapMicroRNA <- # nolint
    function() {
        dataset <- "CCLE_miRNA_20180525.gct"
        currentDataset <- .formalsList[["dataset"]][[1L]]
        h1(sprintf(
            "{.cls %s}: {.var %s}",
            "CCLEMicroRNAExpressionData", dataset
        ))
        gct <- .importMicroRnaGct(dataset = dataset)
        rowRanges <- .importMirbaseGff(rowData = gct[["rowData"]])
        colData <- .importBroadModelInfo(dataset = currentDataset)
        colData <- colData[!is.na(colData[["ccleName"]]), ]
        rownames(colData) <- makeNames(as.character(colData[["ccleName"]]))
        assay <- gct[["assay"]]
        colnames(assay)[
            colnames(assay) == "HS274T_BREAST"] <-
            "HS274T_FIBROBLAST"
        colnames(assay)[
            colnames(assay) == "HS604T_HAEMATOPOIETIC_AND_LYMPHOID_TISSUE"] <-
            "HS604T_FIBROBLAST"
        colnames(assay)[
            colnames(assay) == "HS751T_HAEMATOPOIETIC_AND_LYMPHOID_TISSUE"] <-
            "HS751T_FIBROBLAST"
        colnames(assay)[
            colnames(assay) == "KE97_HAEMATOPOIETIC_AND_LYMPHOID_TISSUE"] <-
            "KE97_STOMACH"
        colnames(assay)[
            colnames(assay) == "NCIH684_LIVER"] <-
            "NCIH684_LARGE_INTESTINE"
        colnames(assay)[
            colnames(assay) == "S117_SOFT_TISSUE"] <-
            "S117_THYROID"
        keepCells <- colnames(assay) %in% rownames(colData)
        missingCells <- sort(colnames(assay)[!keepCells])
        assay <- assay[, keepCells, drop = FALSE]
        colData <- colData[colnames(assay), , drop = FALSE]
        rownames(colData) <- makeNames(as.character(colData[["depmapId"]]))
        colnames(assay) <- rownames(colData)
        assays <- list("counts" = assay)
        metadata <- list(
            "dataset" = dataset,
            "missingCells" = missingCells,
            "packageName" = .pkgName,
            "packageVersion" = .pkgVersion
        )
        se <- makeSummarizedExperiment(
            assays = assays,
            rowRanges = rowRanges,
            colData = colData,
            metadata = metadata
        )
        new(Class = "DepMapMicroRNA", se)
    }
