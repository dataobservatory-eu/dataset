
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
[![devel-version](https://img.shields.io/badge/devel%20version-0.3.93-blue.svg)](https://github.com/dataobservatory-eu/dataset)
[![dataobservatory](https://img.shields.io/badge/ecosystem-dataobservatory.eu-3EA135.svg)](https://dataobservatory.eu/)
[![Codecov test
coverage](https://codecov.io/gh/dataobservatory-eu/dataset/graph/badge.svg)](https://app.codecov.io/gh/dataobservatory-eu/dataset/)

<!-- badges: end -->

# Overview

The `dataset` package helps you create **semantically rich**,
**machine-readable**, and **interoperable datasets** in R. It introduces
S3 classes that enhance data frames, vectors, and bibliographic entries
with formal metadata structures inspired by:

- SDMX (Statistical Data and Metadata eXchange)

- Dublin Core / DataCite metadata standards for scientific and open data
  repositories

- Open Science publishing practices

The goal is to reduce metadata loss, improve interoperability, and
simplify the transition from tidy datasets to web-ready formats like
RDF.

## Installation

You can install the latest released version of **`dataset`** from
[CRAN](https://cran.r-project.org/package=dataset) with:

``` r
install.packages("dataset")
```

To install the development version from GitHub with `pak` or `remotes`:

``` r
# install.packages("pak")
pak::pak("dataobservatory-eu/dataset")

# install.packages("remotes")
remotes::install_github("dataobservatory-eu/dataset")
```

## Minimal Example

``` r
library(dataset)  
df <- dataset_df( 
  country = defined(c("AD", "LI"), label = "Country"),   
  gdp = defined(c(3897, 7365),
                label = "GDP", unit = "million euros"),
  dataset_bibentry = dublincore(
    title = "GDP Dataset", 
    creator = person("Jane", "Doe", role="aut"), 
    publisher = "Small Repository"
  )
) 
print(df)
#> Doe (2025): GDP Dataset [dataset]
#>   rowid     country   gdp       
#>   <defined> <defined> <defined>
#> 1 eg:1      AD        3897     
#> 2 eg:2      LI        7365
```

Export as RDF triples:

``` r
dataset_to_triples(df, format = "nt")
#> [1] "<http://example.com/dataset#eg:1> <http://example.com/prop/country> \"AD\"^^<xs:string> ."
#> [2] "<http://example.com/dataset#eg:2> <http://example.com/prop/country> \"LI\"^^<xs:string> ."
#> [3] "<http://example.com/dataset#eg:1> <http://example.com/prop/gdp> \"3897\"^^<xs:decimal> ." 
#> [4] "<http://example.com/dataset#eg:2> <http://example.com/prop/gdp> \"7365\"^^<xs:decimal> ."
```

Retain automatically recorded provenance:

``` r
provenance(df)
#> [1] "<http://example.com/dataset_prov.nt> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://www.w3.org/ns/prov#Bundle> ."                  
#> [2] "<http://example.com/dataset#> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://www.w3.org/ns/prov#Entity> ."                         
#> [3] "<http://example.com/dataset#> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://purl.org/linked-data/cube#DataSet> ."                 
#> [4] "_:doejane <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://www.w3.org/ns/prov#Agent> ."                                              
#> [5] "<https://doi.org/10.32614/CRAN.package.dataset> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://www.w3.org/ns/prov#SoftwareAgent> ."
#> [6] "<http://example.com/creation> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://www.w3.org/ns/prov#Activity> ."                       
#> [7] "<http://example.com/creation> <http://www.w3.org/ns/prov#generatedAtTime> \"2025-08-06T11:45:24Z\"^^<xs:dateTime> ."
```

## Contributing

We welcome contributions and discussion!

- Please see our
  [CONTRIBUTING.md](https://github.com/dataobservatory-eu/dataset/blob/main/CONTRIBUTING.md)
  guide.
- Ideas, bug reports, and feedback are welcome via [GitHub
  issues](https://github.com/dataobservatory-eu/dataset/issues).

## Code of Conduct

This project follows the [rOpenSci Code of
Conduct](https://ropensci.org/code-of-conduct/). By participating, you
are expected to uphold these guidelines.
