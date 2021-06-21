## DepMapAnalysis 0.1.1 (UNRELEASED)

### New functions

- `plotTopGeneEffectPerGroup`: Added a new plotting function that makes it
  easier to visualize gene effects (dependency scores) across a subset of
  cell lines, such as specific lineages (solid tumors, blood cancer, etc.).

### Minor changes

- Ensuring legacy `Achilles` S4 class definition remains exported, to maintain
  better backward compatibility with objects saved prior to v0.1.0 release,
  where we switched to `DepMapAnalysis` object as the primary class for
  analysis of Broad Achilles data.
- `plotGeneEffect`: Added support for violin geom, and improved consistency
  of ggplot2, using a `switch` call internally.
- Reworked some imports from basejump dependency package.

## DepMapAnalysis 0.1.0 (2021-06-09)

### Major changes

- Initial release version suitable for use on Shiny Server.
- Reworked object approach to now use `DepMapAnalysis` for both CRISPR and RNAi
  gene effect datasets. Previously, these were classed as `Achilles` and
  `DEMETER2`, which is confusing. Now both CRISPR and RNAi experiments keep
  track of project source and scoring method more clearly.
- Simplifed internal code to improve consistency between import of gene effect
  and gene expression from CCLE.
- DepMap releases starting from 2020 are supported.

## DepMapAnalysis 0.0.6 (2021-03-12)

### Minor changes

- Updated basejump dependencies, and removed now unnecessary stringr import.

## DepMapAnalysis 0.0.5 (2021-02-25)

### Major changes

- Improved metadata support for some CCLE functions, returning as
  `SummarizedExperiment` instead of `DataFrame`, when possible.

## DepMapAnalysis 0.0.4 (2021-02-10)

### Major changes

- Added support for 21Q1, 20Q4 v2, and 20Q4 releases.
- Switched `Achilles` object to inherit from `SummarizedExperiment` instead of
  `RangedSummarizedExperiment`.
- Gene metadata now comes from NCBI FTP server.
- `DEMETER2` class objects now support `colData` and `rowData`.

## DepMapAnalysis 0.0.3 (2020-10-07)

### Minor changes

- `Achilles`: Gene synonyms are now fetched automatically, based on the new
  `makeGRangesFromEnsembl` update in basejump.
- Simplified internal BiocFileCache handling, using new `cacheURL` function
  defined in pipette package.

## DepMapAnalysis 0.0.2 (2020-10-02)

### New functions

- `plotGeneEffect`: Plot effect of gene knockdown as a boxplot or density plot.

### Major changes

- `Achilles` object now requires slotting of common essential genes from DepMap.
  These are now imported automatically in the main `Achilles()` call.

## DepMapAnalysis 0.0.1 (2020-10-02)

Initial release.
