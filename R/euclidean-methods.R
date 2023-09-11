#' @name euclidean
#' @inherit AcidGenerics::euclidean
#' @note Updated 2023-09-11.
#'
#' @inheritParams AcidRoxygen::params
#'
#' @param upregulated `character`.
#' Genes observed to be upregulated by treatment.
#'
#' @param downregulated `character`.
#' Genes observed to be downregulated by treatment.
#'
#' @return `DFrame`.
#'
#' @examples
#' data(rnaseq)
#'
#' ## DepMapGeneExpression ====
#' x <- rnaseq
#' genes <- rownames(x)
#' upregulated <- genes[1L:5L]
#' downregulated <- genes[6L:10L]
#' euc <- euclidean(
#'     x = x,
#'     upregulated = upregulated,
#'     downregulated = downregulated
#' )
#' print(euc)
NULL



## Our S4 generic uses "x" instead of "object" as primary object to match the
## naming conventions of other common math functions.

## Updated 2023-09-11.
`euclidean,DepMapGeneExpression` <- # nolint
    function(x,
             upregulated,
             downregulated) {
        object <- x
        assert(
            validObject(object),
            isCharacter(upregulated),
            isCharacter(downregulated),
            hasNoDuplicates(upregulated),
            hasNoDuplicates(downregulated),
            areDisjointSets(x = upregulated, y = downregulated)
        )
        up <- mapGenesToRownames(
            object = object,
            genes = upregulated,
            strict = TRUE
        )
        down <- mapGenesToRownames(
            object = object,
            genes = downregulated,
            strict = TRUE
        )
        object <- object[c(up, down), ]
        ## We are returning genes in rows, cells in columns. Some of our legacy
        ## Python code uses a transposed matrix here.
        mat <- zscore(object)
        ## Determine the maximum and minimum values per gene across the cells.
        geneMax <- rowMaxs(mat)
        geneMin <- rowMins(mat)
        ## Calculate SI and RI per gene for up- and down-regulated genes.
        siUp <- geneMax[names(geneMax) %in% up]
        riUp <- geneMin[names(geneMin) %in% up]
        siDown <- geneMin[names(geneMin) %in% down]
        riDown <- geneMax[names(geneMax) %in% down]
        si <- c(siUp, siDown)
        ri <- c(riUp, riDown)
        ## Create SI and RI matrix to get max.
        siMat <- do.call(
            what = cbind,
            args = replicate(
                n = ncol(mat),
                expr = si,
                simplify = FALSE
            )
        )
        riMat <- do.call(
            what = cbind,
            args = replicate(
                n = ncol(mat),
                expr = ri,
                simplify = FALSE
            )
        )
        colnames(siMat) <- colnames(mat)
        colnames(riMat) <- colnames(mat)
        ## Calculate the summation of euclidean distances.
        sigmaS <- colSums2((mat - siMat) ** 2L)
        sigmaR <- colSums2((mat - riMat) ** 2L)
        ## Euclidean distance of sensitivity model.
        dsj <- sqrt(sigmaS)
        ## Euclidean distance of insensitivity model.
        drj <- sqrt(sigmaR)
        ratio <- dsj / drj
        ## Indicate whether we predict sensitive.
        pred <- ifelse(
            test = dsj <= drj,
            yes = "sensitive",
            no = "insensitive"
        )
        out <- DataFrame(
            "drj" = drj,
            "dsj" = dsj,
            "ratio" = ratio,
            "prediction" = pred,
            row.names = colnames(object)
        )
        cd <- .simpleColData(object)
        out <- cbind(out, cd)
        ## Ensure we stash the user input in the output.
        metadata(out) <- list(
            "upregulated" = upregulated,
            "downregulated" = downregulated
        )
        out
    }



#' @rdname euclidean
#' @export
setMethod(
    f = "euclidean",
    signature = signature(x = "DepMapGeneExpression"),
    definition = `euclidean,DepMapGeneExpression`
)
