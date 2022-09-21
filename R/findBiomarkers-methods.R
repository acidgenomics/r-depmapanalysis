#' Find biomarkers
#'
#' @name findBiomarkers
#' @note Updated 2022-08-17.
#'
#' @inheritParams AcidRoxygen::params
#'
#' @param insensitiveCells,sensitiveCells `character`.
#' Cell line names.
#'
#' @return FIXME Varies, depending on method.
#'
#' @seealso
#' - limma User's Guide, Section 15.4.
#'
#' @examples
#' data(rnaseq)
#'
#' ## CCLEExpressionData ====
#' object <- rnaseq
#' sensitiveCells <- colnames(object)[c(1L:5L)]
#' insensitiveCells <- colnames(object)[c(6L:10L)]
#' x <- findBiomarkers(
#'     object = object,
#'     sensitiveCells = sensitiveCells,
#'     insensitiveCells = insensitiveCells
#' )
#' print(x)
NULL



## Updated 2022-08-17.
`findBiomarkers,CCLECopyNumberData` <-
    function(
        object,
        sensitiveCells,
        insensitiveCells
    ) {
        stop("FIXME Still a work in progress.")
    }



## Updated 2022-08-17.
`findBiomarkers,CCLEExpressionData` <-
    function(
        object,
        sensitiveCells,
        insensitiveCells
    ) {
        stop("FIXME Still a work in progress.")
        assert(
            validObject(object),
            requireNamespaces("limma"),
            isCharacter(sensitiveCells),
            isCharacter(insensitiveCells)
        )

        ## FIXME Need to run .mapCellsToColnames here.

        log2Tpm <- assay(object, i = "log2Tpm")
        fit <- limma::lmFit(log2Tpm, design)
        fit <- limma::eBayes(fit, trend=TRUE)
        ## FIXME Add an expression cutoff (e.g. baseMean) here.
        topTable(fit, coef=ncol(design))
    }



## Updated 2022-08-17.
`findBiomarkers,CCLEMutationData` <-
    function(
        object,
        sensitiveCells,
        insensitiveCells
    ) {
        stop("FIXME Still a work in progress.")
    }



#' @rdname findBiomarkers
#' @export
setMethod(
    f = "findBiomarkers",
    signature = signature(object = "CCLECopyNumberData"),
    definition = `findBiomarkers,CCLECopyNumberData`
)

#' @rdname findBiomarkers
#' @export
setMethod(
    f = "findBiomarkers",
    signature = signature(object = "CCLEExpressionData"),
    definition = `findBiomarkers,CCLEExpressionData`
)

#' @rdname findBiomarkers
#' @export
setMethod(
    f = "findBiomarkers",
    signature = signature(object = "CCLEMutationData"),
    definition = `findBiomarkers,CCLEMutationData`
)
