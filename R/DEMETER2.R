## FIXME Rework this and move into map DepMapAnalysis function...

.DEMETER2 <-  # nolint
    function(rowData = TRUE, colData = TRUE) {
        ## CSV formatting: cells in columns, genes in rows.
        assays <- list(
            "effect" = .importDataFile(
                fileName = "d2_combined_gene_dep_scores.csv",
                release = release,
                rownamesCol = 1L,
                return = "matrix"
            )
        )

    }
