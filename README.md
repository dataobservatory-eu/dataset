
<!-- README.md is generated from README.Rmd. Please edit that file -->

# The dataset R Package <a href='https://dataset.dataobservatory.eu/'><img src="man/figures/logo.png" align="right"/></a>

<!-- badges: start -->

[![rhub](https://github.com/dataobservatory-eu/dataset/actions/workflows/rhub.yaml/badge.svg)](https://github.com/dataobservatory-eu/dataset/actions/workflows/rhub.yaml)
[![lifecycle](https://lifecycle.r-lib.org/articles/figures/lifecycle-experimental.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![Project Status:
WIP](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip)
[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/dataset)](https://cran.r-project.org/package=dataset)
[![CRAN_time_from_release](https://www.r-pkg.org/badges/ago/dataset)](https://cran.r-project.org/package=dataset)
[![Status at rOpenSci Software Peer
Review](https://badges.ropensci.org/553_status.svg)](https://github.com/ropensci/software-review/issues/553)
[![DOI](https://zenodo.org/badge/DOI/10.32614/CRAN.package.dataset.svg)](https://zenodo.org/record/6950435#.YukDAXZBzIU)
[![devel-version](https://img.shields.io/badge/devel%20version-0.3.9-blue.svg)](https://github.com/dataobservatory-eu/dataset)
[![dataobservatory](https://img.shields.io/badge/ecosystem-dataobservatory.eu-3EA135.svg)](https://dataobservatory.eu/)
[![Codecov test
coverage](https://codecov.io/gh/dataobservatory-eu/dataset/graph/badge.svg)](https://app.codecov.io/gh/dataobservatory-eu/dataset)

<!-- badges: end -->

# dataset: Semantic Metadata for Datasets in R

The `dataset` package provides tools to create semantically rich and
interoperable datasets in R. It improves metadata handling by
introducing new S3 classes—`defined()`, `dataset_df()`, and
`bibrecord()`—that enhance the behaviour of `labelled`, `tibble`, and
`bibentry` objects to meet the requirements of:

- **Statistical Data and Metadata eXchange (SDMX)** standards,
- **Open Science** metadata practices,
- **Library and archive metadata** conventions (Dublin Core, DataCite).

## Motivation

Many tools exist to help document, describe, or publish datasets in R,
but most separate the metadata from the data itself. This separation
increases the risk of losing metadata, misaligning it with the data, or
making documentation hard to maintain.

The `dataset` package addresses this by storing all metadata directly in
R object attributes. This preserves semantic information as data is
transformed, combined, or exported, preventing the loss of vital
documentation and improving reproducibility.

## Key Features

### `defined()`

An extended version of `labelled()` vectors. Adds support for:

- Variable labels
- Units of measure (e.g. “million euros”)
- Concept URIs (standardized definitions)
- Namespaces (to support URI expansion)

``` r
library(dataset)
```

``` r
data(orange_df)
print(orange_df$age)
#> orange_df$age: The age of the tree
#> Measured in days since 1968/12/31 
#>  [1]  118  484  664 1004 1231 1372 1582  118  484  664 1004 1231 1372 1582  118
#> [16]  484  664 1004 1231 1372 1582  118  484  664 1004 1231 1372 1582  118  484
#> [31]  664 1004 1231 1372 1582
```

This ensures that, for example, “GDP” is always associated with a
precise concept and unit, avoiding ambiguity across analyses and
publications. See [Semantically Enriched Vectors with
`defined()`](https://dataset.dataobservatory.eu/articles/defined.html)

### `bibrecord()`

An extension of R’s built-in `bibentry()` class, with support for:

- Dublin Core Terms (`dcterms`)
- DataCite metadata
- Contributor roles (e.g. creator, publisher, data manager)
- Subject tagging and geolocation

``` r
as_dublincore(orange_df)
#> Dublin Core Metadata Record
#> --------------------------
#> Title:        Growth of Orange Trees 
#> Creator(s):   N.R. Draper [cre] (http://viaf.org/viaf/84585260); H Smith [cre] 
#> Contributor(s):  :unas 
#> Publisher:    Wiley 
#> Year:         1998 
#> Language:     en 
#> Description:  The Orange data frame has 35 rows and 3 columns of records of the growth of orange trees.
```

This makes it easier to produce citations and metadata suitable for
repositories like [Zenodo](https://zenodo.org/) or
[Dataverse](https://dataverse.org/). See more in the [Modernising
Citation Metadata in R: Introducing
`bibrecord`](https://dataset.dataobservatory.eu/articles/bibrecord.html)

### `dataset_df()`

A semantic wrapper around `data.frame` or `tibble`, aligning with SDMX’s
**data cube** model:

- Variables (columns) can have units, labels, and definitions.
- Observations (rows) can be assigned unique identifiers.
- Datasets can carry complete metadata inline (title, creator,
  description, etc.)
- Output can be serialized to linked data formats (N-Triples, RDF, etc.)

See more in the [Why Semantics Matter for R Data
Frames](https://dataset.dataobservatory.eu/articles/dataset_df.html)

## Why Use This?

- **Machine-readability**: Your data and metadata are tightly coupled
  and structured for reuse.
- **Preservation**: Data exported from R retains its full descriptive
  context.
- **Publication-ready**: Integration with modern repository standards
  (DataCite, DC Terms).
- **Tidy + semantic**: Extends tidy principles with semantic rigor.

## Example

``` r
my_data <- dataset_df(
  country = defined(
    c("AD", "LI"), 
    concept =  "http://data.europa.eu/bna/c_6c2bb82d"),
  gdp = defined(c(3897, 7365), 
                label = "GDP", 
                unit = "million euros"),
  dataset_bibentry = datacite(
    Title = "GDP Data for Small Countries",
    Description = "Example Dataset for the dataset package",
    Creator = person("Jane", "Doe"),
    Publisher = "Open Data Institute",
    Rights = "CC0", 
    Language = "en"
  )
)

head(my_data)
#> 
#> 
#>   rowid      country    gdp        
#>   <hvn_lbl_> <hvn_lbl_> <hvn_lbl_>
#> 1 eg:1       AD         3897      
#> 2 eg:2       LI         7365
```

``` r
as_datacite(my_data)
#> DataCite Metadata Record
#> --------------------------
#> Title:         GDP Data for Small Countries 
#> Creator(s):    Jane Doe 
#> Contributor(s): :unas 
#> Identifier:    :tba 
#> Publisher:     Open Data Institute 
#> Year:          :tba 
#> Language:      en 
#> Description:  Example Dataset for the dataset package
```

## 🧪 Contributing

We welcome contributions and discussion!

- Please see our
  [CONTRIBUTING.md](https://github.com/dataobservatory-eu/dataset/blob/main/CONTRIBUTING.md)
  guide.
- Ideas, bug reports, and feedback are welcome via [GitHub
  issues](https://github.com/dataobservatory-eu/dataset/issues).

## 📜 Code of Conduct

This project adheres to the [rOpenSci Code of
Conduct](https://ropensci.org/code-of-conduct/). By participating, you
are expected to uphold these guidelines.
