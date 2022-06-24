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

### [Docker][] method

```sh
image='acidgenomics/r-packages:depmapanalysis'
workdir='/mnt/work'
docker pull "$image"
docker run -it \
    --volume="${PWD}:${workdir}" \
    --workdir="$workdir" \
    "$image" \
    R
```

[depmap]: https://depmap.org/
[docker]: https://www.docker.com/
[r]: https://www.r-project.org/
