#' Map genes
#'
#' @name mapGenes
#' @note Updated 2020-10-01.
#'
#' @inheritParams acidroxygen::params
#' @param GRanges `GRanges`.
#'   Must contain Entrez identifiers defined in `entrezID` column of `mcols`.
#' @param ids `character`.
#'   Ensembl gene identifiers, corresponding to the `names` of the `GRanges`
#'   that contains the Entrez identifiers.
#'
#' @seealso
#' - `basejump::makeGRangesFromEnsembl`, for generating `GenomicRanges`.
#'
#' @examples
#' object <- AchillesGeneDependencyData()
#' GRanges <- makeGRangesFromEnsembl("Homo sapiens")
#' ids <- head(names(GRanges))
#' mapGenes(
#'     object = object,
#'     GRanges = GRanges,
#'     ids = ids
#' )
NULL



## Note that some gene names (i.e. HUGO symbols) may be outdated on DepMap, so
## match primarily using the Entrez identifier instead.
## Updated 2020-09-30.
`mapGenes,AchillesGeneEffectData` <-  # nolint
    function(
        object,
        GRanges,  # nolint
        ids
    ) {
        gr <- GRanges
        validObject(object)
        assert(
            is(gr, "GRanges"),
            isCharacter(ids),
            isSubset(ids, names(gr)),
            isSubset("entrezID", colnames(mcols(gr)))
        )
        cli_alert("Mapping Ensembl identifiers to Entrez.")
        gr <- gr[ids]
        map <- as(Ensembl2Entrez(object = gr, format = "1:1"), "DataFrame")
        assert(identical(names(gr), map[["geneID"]]))
        map[["geneName"]] <- mcols(gr)[["geneName"]]
        map <- decode(map)
        colnames <- mapply(
            entrezID = map[["entrezID"]],
            MoreArgs = list(x = colnames(object)),
            FUN = function(entrezID, x) {
                pattern <- paste0("_", entrezID, "$")
                out <- grep(x = x, pattern = pattern, value = TRUE)
                if (!isString(out)) {
                    stop(sprintf("Match failure: '%s'.", entrezID))
                }
                out
            },
            SIMPLIFY = TRUE,
            USE.NAMES = FALSE
        )
        mismatches <- setdiff(
            x = toupper(colnames),
            y = toupper(paste0(map[["geneName"]], "_", map[["entrezID"]]))
        )
        if (hasLength(mismatches)) {
            cli_alert_warning(sprintf(
                "Gene symbol mismatch: {.var %s}.",
                toString(mismatches, width = 200L)
            ))
        }
        colnames
    }



#' @rdname mapGenes
#' @export
setMethod(
    f = "mapGenes",
    signature = signature("AchillesGeneEffectData"),
    definition = `mapGenes,AchillesGeneEffectData`
)
