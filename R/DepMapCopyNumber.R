## FIXME Rework this as a single assay internal generator that we can share
## across other generators...such as DepMapExpression.

#' Import DepMap copy number data
#'
#' @export
#' @note Updated 2023-08-08.
#'
#' @return `DepMapCopyNumber`.
#'
#' @examples
#' object <- DepMapCopyNumber()
#' print(object)
DepMapCopyNumber <- # nolint
    function() {
        dataset <- .currentDataset
        assert(isString(dataset))
        json <- datasets[[dataset]]
        assert(is.list(json))
        urls <- unlist(x = json[["files"]], recursive = FALSE, use.names = TRUE)
        dict <- list(
            "releaseDate" = json[["metadata"]][["date"]],
            "transposeAssays" = json[["metadata"]][["transpose_assays"]]
        )
        assert(
            isString(dict[["releaseDate"]]),
            isFlag(dict[["transposeAssays"]]),
            allAreURLs(urls)
        )
        urls <- list(
            "assays" = c(
                "log2CopyNumber" = urls[["OmicsCNGene.csv"]]
            )
        )
        assert(allAreURLs(unlist(urls, recursive = TRUE)))
        assays <- lapply(
            X = urls[["assays"]],
            FUN = .importDataFile,
            format = "csv",
            rownameCol = 1L,
            colnames = TRUE,
            return = "matrix"
        )
        metadata <- list(
            "releaseDate" = dict[["releaseDate"]]
        )
        se <- .makeBroadSE(
            dataset = dataset,
            assays = assays,
            transposeAssays = dict[["transposeAssays"]],
            metadata = metadata,
            class = "DepMapCopyNumber"
        )
        se
    }
