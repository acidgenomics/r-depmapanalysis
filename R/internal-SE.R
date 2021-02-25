## Updated 2021-02-25.
.makeSummarizedExperiment <-
    function(
        assays,
        rowData,
        colData,
        metadata,
        class
    ) {
        assert(
            is.list(assays),
            is.list(metadata)
        )
        metadata[["packageVersion"]] <- .pkgVersion
        metadata <- metadata[sort(names(metadata))]
        args <- list(
            "assays" = assays,
            "colData" = colData,
            "metadata" = Filter(Negate(is.null), metadata),
            "rowData" = rowData
        )
        args <- Filter(Negate(is.null), args)
        se <- do.call(what = makeSummarizedExperiment, args = args)
        assert(is(se, "SummarizedExperiment"))
        rownames(se) <- tolower(rownames(se))
        colnames(se) <- tolower(colnames(se))
        validObject(se)
        new(Class = class, se)
    }
