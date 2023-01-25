#' Import CCLE microRNA expression data
#'
#' @details
#' NanoString microRNA panel data.
#'
#' @export
#' @note Updated 2022-11-16.
#'
#' @return `CCLEMicroRNAExpressionData`.
#'
#' @examples
#' object <- CCLEMicroRNAExpressionData()
#' dim(object)
CCLEMicroRNAExpressionData <- # nolint
    function() {
        url <- pasteURL(
            "depmap.org",
            "portal",
            "download",
            "api",
            paste0(
                "download?",
                "file_name", "=",
                "ccle%2Fccle_2018%2FCCLE_miRNA_20180525.gct",
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
        ## Need to reimport to map NanoString identifiers back to actual
        ## microRNA names.
        df <- import(
            con = tmpfile,
            format = "gct",
            return = "data.frame",
            quiet = TRUE
        )
        assert(isSubset(c("Name", "Description"), colnames(df)))
        df <- df[, c("Name", "Description")]
        ## Import miRBase annotations as rowRanges.
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
        ## Need to correct "hsa-mir" to "hsa-miR".
        mcols(gr)[["Name"]] <- gsub(
            pattern = "\\bmir\\b",
            replacement = "miR",
            x = mcols(gr)[["Name"]]
        )
        f <- as.factor(vapply(
            X = strsplit(
                x = mcols(gr)[["Name"]],
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
        vec <- setdiff(df[["Description"]], names(grl))
        vec <- intersect(vec, mcols(gr)[["Name"]])
        idx <- na.omit(match(x = vec, table = mcols(gr)[["Name"]]))
        gr <- gr[idx]
        gr <- split(x = gr, f = as.factor(mcols(gr)[["Name"]]))
        grl <- c(grl, gr)


        ## FIXME Split this out as a function...
        ## FIXME Need to get colData and remap the identifiers...
        currentDataset <- .formalsList[["dataset"]][[1L]]
        cd <- .importCellLineSampleData(dataset = currentDataset)

        sort(setdiff(
            colnames(assay),
            makeNames(as.character(cd[["ccleName"]]))
        ))

        ## [1] "COLO699_LUNG"
        ## [2] "HS274T_BREAST"
        ## [3] "HS604T_HAEMATOPOIETIC_AND_LYMPHOID_TISSUE"
        ## [4] "HS751T_HAEMATOPOIETIC_AND_LYMPHOID_TISSUE"
        ## [5] "KE97_HAEMATOPOIETIC_AND_LYMPHOID_TISSUE"
        ## [6] "NCIH1339_LUNG"
        ## [7] "NCIH684_LIVER"
        ## [8] "S117_SOFT_TISSUE"


        TRUE
    }






## FIXME Refer to `.standardizeDemter2`, rework as remap from CCLE to Achilles...

#' Standardize the DEMETER2 RNAi dataset
#'
#' @note Updated 2022-11-16.
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
    colnames(object) <- makeNames(colData(object)[["depmapId"]])
    object <- object[, sort(colnames(object))]
    object
}

