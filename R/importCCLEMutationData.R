#' Import CCLE mutation data
#'
#' @export
#' @note Updated 2020-09-30.
#'
#' @inheritParams params
#'
#' @return `DataFrame`.
#'
#' @examples
#' df <- importCCLEMutationData()
#' dim(df)
importCCLEMutationData <- function(release = NULL) {
    .importDataFile(
        fileName = "ccle_mutations.csv",
        type = "cellular_models",
        ## Note that this is a TSV, even though extension is CSV!
        format = "tsv",
        release = release,
        ## Consider returning as SplitDataFrame or grouped tibble.
        rownamesCol = NULL
    )
}
