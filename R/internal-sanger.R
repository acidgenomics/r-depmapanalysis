## FIXME rename modelId to sangerModelId
## FIXME rename rrid to cellosaurusId
## FIXME rename modelName to cellLineName
## FIXNE rename cancerTypeNcitId
## FIXME rename broadId to depmapId
## FIXME Split the synonyms by semicolon.



#' Import Sanger CellModelPassports cell line metadata
#'
#' @note Updated 2023-03-01.
#' @noRd
#'
#' @details
#' Alternatively, can use "model_list_latest.csv.gz".
.importSangerMetadata <- function() {

    url <- pasteURL(
        "cog.sanger.ac.uk",
        "cmp",
        "download",
        "model_list_20230110.csv",
        protocol = "https"
    )
    con <- .cacheURL(url)
    df <- import(con = con, format = "csv", engine = "data.table")
    df <- as(df, "DataFrame")
    colnames(df) <- camelCase(colnames(df))
    df
}
