## FIXME THIS DOESNT NEED TO BE USER ACCESSIBLE.



#' @name mapGenes
#' @inherit acidgenerics::mapGenes
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



#' @rdname mapGenes
#' @name mapGenes
#' @importFrom acidgenerics mapGenes
#' @usage mapGenes(object, ...)
#' @export
NULL



## FIXME WE SHOULD BE ABLE TO MAP ALL OF THEM TO ENSEMBL.

## FIXME Ensure we're handling this edge case:
## ## "C2orf54_79919"

## Note that some gene names (i.e. HUGO symbols) may be outdated on DepMap, so
## match primarily using the Entrez identifier instead.
## Updated 2020-10-01.
`mapGenes,Achilles` <-  # nolint
    function(
        object,
        GRanges  # nolint
    ) {
        gr <- GRanges
        validObject(object)
        assert(
            is(gr, "GRanges"),
            isSubset("entrezID", colnames(mcols(gr)))
        )

        ## FIXME THINK ABOUT MATCHING WITH THIS.
        synonyms <- geneSynonyms(organism = "Homo sapiens")
        assert(is(synonyms, "SplitDataFrameList"))

        cli_alert("Mapping Ensembl identifiers to Entrez.")

        ## FIXME SEE IF THIS WORKS 1:1 FIRST.
        map <- Ensembl2Entrez(object = gr, format = "long")
        map <- as.data.frame(map)
        map <- map[complete.cases(map), ]

        ## FIXME RETHINK THIS.
        ## > map <- as(map, "DataFrame")
        ## > assert(identical(names(gr), map[["geneID"]]))
        ## > map[["geneName"]] <- mcols(gr)[["geneName"]]
        ## > map <- decode(map)

        entrez <- as.integer(str_extract(
            string = rownames(object),
            pattern = "[0-9]+$"
        ))

        ## FIXME THIS IS SCREWING UP.
        xxx <- match(
            x = entrez,
            table = map[["entrezID"]]
        )
        bad <- which(is.na(xxx))
        entrez[bad]
        rownames[bad]

        match(
            x = 3105,
            table = map[["entrezID"]]
        )

        assert(!any(is.na(xxx)))

        ## THESE ARE DEAD ENTREZ IDs that need to be updated:
        ##
        ##  [1] "akap2_11217"     "arih2os_646450"  "c_xorf36_79742"  "c12orf49_79794"
        ##  [5] "c16orf78_123970" "c6orf118_168090" "c9orf153_389766" "creld2_79174"
        ##  [9] "dec1_50514"      "dirc1_116093"    "glra4_441509"    "kiaa1107_23285"
        ## [13] "oclm_10896"      "palm2_114299"    "prss45_377047"   "sphar_10638"
        ## [17] "st20_400410"     "tcp10l2_401285"

        ensembl <- mapply(
            entrez = entrez,
            MoreArgs = list(map = map),
            FUN = function(entrez, map) {
                match(
                    x = entrez,
                    table = map[["entrezID"]]
                )



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
            x = toupper(rownames),
            y = toupper(paste0(map[["geneName"]], "_", map[["entrezID"]]))
        )
        if (hasLength(mismatches)) {
            cli_alert_warning(sprintf(
                "Gene symbol mismatch: {.var %s}.",
                toString(mismatches, width = 200L)
            ))
        }
        rownames
    }



#' @rdname mapGenes
#' @export
setMethod(
    f = "mapGenes",
    signature = signature("Achilles"),
    definition = `mapGenes,Achilles`
)
