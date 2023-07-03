## FIXME Can we integrate OncoKB metadata here? Would be cool.



#' Import DepMap mutation data
#'
#' @export
#' @note Updated 2023-02-03.
#'
#' @inheritParams params
#'
#' @return `DepMapMutation`.
#'
#' @examples
#' object <- DepMapMutation()
#' dim(object)
DepMapMutation <- # nolint
    function(dataset) {
        dataset <- match.arg(dataset)
        ## FIXME Need to rework this key.
        url <- datasets[[dataset]][["files"]][["ccle"]][["mutations"]][["url"]]
        assert(isAURL(url))
        df <- .importDataFile(url = url, format = "csv", rownamesCol = NULL)
        assert(is(df, "DataFrame"))
        ## FIXME These metadata columns need to change with 23q2 update.
        colnames(df)[colnames(df) == "cDNA_Change"] <- "cdnaChange"
        colnames(df)[colnames(df) == "COSMIChsCnt"] <- "cosmicHsCnt"
        colnames(df)[colnames(df) == "DepMap_ID"] <- "depmapId"
        colnames(df)[colnames(df) == "dbSNP_RS"] <- "dbsnpRs"
        colnames(df)[colnames(df) == "dbSNP_Val_Status"] <- "dbsnpValStatus"
        colnames(df)[colnames(df) == "ExAC_AF"] <- "exacAf"
        colnames(df)[colnames(df) == "Hugo_Symbol"] <- "geneName"
        colnames(df)[colnames(df) == "isCOSMIChotspot"] <- "isCosmicHotspot"
        colnames(df)[colnames(df) == "isTCGAhotspot"] <- "isTcgaHotspot"
        colnames(df)[colnames(df) == "RNAseq_AC"] <- "rnaseqAc"
        colnames(df)[colnames(df) == "SangerWES_AC"] <- "sangerWesAc"
        colnames(df)[colnames(df) == "TCGAhsCnt"] <- "tcgaHsCnt"
        colnames(df) <- camelCase(colnames(df), strict = TRUE)
        df <- encode(df)
        metadata(df) <- list(
            "date" = Sys.Date(),
            "dataset" = dataset,
            "packageName" = .pkgName,
            "packageVersion" = .pkgVersion
        )
        new(Class = "DepMapMutation", df)
    }

formals(DepMapMutation)[["dataset"]] <- # nolint
    .formalsList[["depmapDataset"]]
