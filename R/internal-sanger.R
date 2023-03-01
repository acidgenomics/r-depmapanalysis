## FIXME rename modelId to sangerModelId
## FIXME rename rrid to cellosaurusId
## FIXME rename modelName to cellLineName
## FIXNE rename cancerTypeNcitId
## FIXME rename broadId to depmapId


.importSangerMetadata <- function() {
    url <- pasteURL(
        "cog.sanger.ac.uk",
        "cmp",
        "download",
        "model_list_latest.csv.gz",
        protocol = "https"
    )
    con <- .cacheURL(url)
    df <- import(con = con, format = "csv", engine = "data.table")
    df <- as(df, "DataFrame")
    colnames(df) <- camelCase(colnames(df))
    ## FIXME Split the synonyms by semicolon.
    df
}
