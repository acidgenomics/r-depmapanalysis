#' @name zscore
#' @inherit AcidGenerics::zscore
#' @note Updated 2023-09-11.
#'
#' @return `matrix`
#' Z-score normalized log2 transcripts per million.
#' Cells in columns, genes in rows.
#'
#' @examples
#' data(rnaseq)
#' zscore <- zscore(rnaseq)
#' print(zscore[1L:5L, 1L:5L])
NULL



## Updated 2023-09-11.
`zscore,DepMapGeneExpression` <- # nolint
    function(object) {
        assert(validObject(object))
        tpm <- tpm(object)
        zs <- zscore(tpm)
        assert(is.matrix(zs))
        zs
    }



#' @rdname zscore
#' @export
setMethod(
    f = "zscore",
    signature = signature(object = "DepMapGeneExpression"),
    definition = `zscore,DepMapGeneExpression`
)
