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
#' @importFrom matrixStats colMaxs colMins rowSums2
#'
#' @examples
#' rnaseq <- DepMapGeneExpression()
NULL



## Updated 2023-09-11.
`euclidean,DepMapGeneExpression` <- # nolint
    function(x,
             upregulated,
             downregulated) {
        assert(
            validObject(x),
            isCharacter(upregulated),
            isCharacter(downregulated),
            hasNoDuplicates(upregulated),
            hasNoDuplicates(downregulated),
            areDisjointSets(x = upregulated, y = downregulated)
        )
        up <- mapGenesToRownames(
            object = x,
            genes = upregulated,
            strict = TRUE
        )
        down <- mapGenesToRownames(
            object = x,
            genes = downregulated,
            strict = TRUE
        )
        ## We are returning genes in rows, cells in columns. Some of our legacy
        ## Python code uses a transposed matrix here.
        mat <- zscore(x)
        mat <- mat[c(up, down), ]
        ## Determine the maximum and minimum values per gene across the cells.
        geneMax <- rowMaxs(mat)
        geneMin <- rowMins(mat)
        ## Calculate SI and RI per gene for up- and down-regulated genes.
        si_up <- geneMax[names(geneMax) %in% up]
        ri_up <- geneMin[names(geneMin) %in% up]
        si_down <- geneMin[names(geneMin) %in% down]
        ri_down <- geneMax[names(geneMax) %in% down]

        si_vector <- c(si_up, si_down)
        ri_vector <- c(ri_up, ri_down)

        ## Create SI and RI matrix to get max
        si_matrix <- do.call("rbind",
                             replicate(nrow(input_df), si_vector, simplify = FALSE))
        ri_matrix <- do.call("rbind",
                             replicate(nrow(input_df), ri_vector, simplify = FALSE))

        ## Get Summation of euclidean distances
        sigma_s <- rowSums2((gene_matrix - si_matrix) ** 2)
        sigma_r <- rowSums2((gene_matrix - ri_matrix) ** 2)

        ## Euclidean distance of sensitivity model
        dsj <- sqrt(sigma_s)
        ## Euclidean distance of insensitivity model
        drj <- sqrt(sigma_r)
        ratio <- dsj / drj

        ## Predict call
        pred <- ifelse(dsj <= drj, "sensitive", "insensitive")

        result <- data.frame(
            Cells = input_df$X,
            drj = drj,
            dsj = dsj,
            ratio = ratio,
            prediction = pred
        )

        print(result)

        ## Write results
        output_file <- file.path(getwd(), "calc_euclidean.csv")
        message("Writing results to :", output_file)
        write.csv(result,file = output_file)

        ## Print sensitivity count
        cat("sensitive count: ", sum(result$prediction %in% "sensitive"))

    }



#' @rdname euclidean
#' @export
setMethod(
    f = "euclidean",
    signature = signature(x = "DepMapGeneExpression"),
    definition = `euclidean,DepMapGeneExpression`
)
