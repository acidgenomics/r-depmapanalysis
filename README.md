# DepMapAnalysis

[Cancer Dependency Map (DepMap)][depmap] analysis toolkit.

## Installation

### [R][] method

```r
if (!requireNamespace("BiocManager", quietly = TRUE)) {
    install.packages("BiocManager")
}
install.packages(
    pkgs = "DepMapAnalysis",
    repos = c(
        "https://r.acidgenomics.com",
        BiocManager::repositories()
    )
)
```

[depmap]: https://depmap.org/
[r]: https://www.r-project.org/
