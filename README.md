
<!-- README.md is generated from README.Rmd. Please edit that file -->

# The dataset R Package

<!-- badges: start -->

[![lifecycle](https://lifecycle.r-lib.org/articles/figures/lifecycle-experimental.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.6833823.svg)](https://10.5281/zenodo.6833823)
[![devel-version](https://img.shields.io/badge/devel%20version-0.1.2-blue.svg)](https://github.com/antaldaniel/dataset)
[![dataobservatory](https://img.shields.io/badge/ecosystem-dataobservatory.eu-3EA135.svg)](https://dataobservatory.eu/)
[![Follow
rOpenGov](https://img.shields.io/twitter/follow/ropengov.svg?style=social)](https://twitter.com/intent/follow?screen_name=ropengov)
[![Follow
author](https://img.shields.io/twitter/follow/digitalmusicobs.svg?style=social)](https://twitter.com/intent/follow?screen_name=digitalmusicobs)
<!-- badges: end -->

The primary aim of dataset is to build well-documented data.frames,
tibbles or data.tables that follow the W3C [Data Cube
Vocabulary](https://www.w3.org/TR/vocab-data-cube/) based on the
statistical SDMX data cube model[^1]. Such standard R objects
(data.fame, data.table, tibble, or well-structured lists like json)
become highly interoperable and can be placed into relational databases,
semantic web applications, archives, repositories. They follow the
[FAIR](https://www.go-fair.org/fair-principles/) principles: they are
findable, accessible, interoperable and reusable.

## Installation

You can install the development version of dataset from Github:

``` r
remotes::install_package(dataobservatory-eu/dataset)
```

Our
[datasets](https://dataset.dataobservatory.eu/reference/dataset.html):

-   [x] Contain Dublin Core or DataCite (or both) metadata that makes
    the findable and easier accessible via online libraries. See
    vignette article [Datasets With FAIR
    Metadata](https://dataset.dataobservatory.eu/articles/metadata.html).

-   [x] Their dimensions can be easily and unambigously reduced to
    triples for RDF applications; they can be easily serialized to, or
    synchronized with semantic web applications. See vignette article
    [From dataset To
    RDF](https://dataset.dataobservatory.eu/articles/metadata.html);

-   [x] Contain processing metadata that greatly enhance the
    reproducibility of the results, and the reviewability of the
    contents of the dataset, including metadata defined by the [DDI
    Alliance](https://ddialliance.org/), which is particularly helpful
    for not yet processed data;

-   [x] Follow the datacube model of the [Statistical Data and Metadata
    eXchange](https://sdmx.org/), therefore allowing easy refreshing
    with new data from the source of the analytical work, and
    particularly useful for datasets containing results of statistical
    operations in R;

-   [x] Relatively lightweight in dependencies and easily works with
    data.frame, [tibble](https://tibble.tidyverse.org/) or
    [data.table](https://rstudio.github.io/DT/) R objects.

This package is in an early development phase. The current dataset S3
class is inherited from the base R data.frame. Later versions may change
to the modern [tibble](https://tibble.tidyverse.org/), which carries a
larger dependency footprint but easier to work with. Easy
interoperability with the [DT](https://rstudio.github.io/DT/)
package–which provides an Rinterface to the
[DataTables](https://datatables.net/) JavaScript library— remains a top
development priority.

## Getting started

``` r
library(dataset)
my_iris_dataset <- dataset(
  x = iris, 
  dimensions = NULL, 
  measures = c("Sepal.Length", "Sepal.Width",  "Petal.Length", "Petal.Width" ), 
  attributes = "Species", 
  title = "Iris Dataset"
)

my_iris_dataset <- dublincore_add(
  x = my_iris_dataset,
  creator = person("Edgar", "Anderson", role = "aut"),
  publisher = "American Iris Society",
  source = "https://doi.org/10.1111/j.1469-1809.1936.tb02137.x",
  date = 1935,
  language = "en"
)

dublincore(my_iris_dataset)
#> $names
#> [1] "Sepal.Length" "Sepal.Width"  "Petal.Length" "Petal.Width"  "Species"     
#> 
#> $dimensions
#> $dimensions$names
#> character(0)
#> 
#> $dimensions$class
#> named list()
#> 
#> $dimensions$isDefinedBy
#> named logical(0)
#> 
#> $dimensions$codelist
#> named list()
#> 
#> 
#> $measures
#> $measures$names
#> [1] "Sepal.Length" "Sepal.Width"  "Petal.Length" "Petal.Width" 
#> 
#> $measures$class
#> Sepal.Length  Sepal.Width Petal.Length  Petal.Width 
#>    "numeric"    "numeric"    "numeric"    "numeric" 
#> 
#> $measures$isDefinedBy
#> [1] "http://purl.org/linked-data/cube"
#> 
#> $measures$codelist
#> $measures$codelist$Sepal.Length
#> NULL
#> 
#> $measures$codelist$Sepal.Width
#> NULL
#> 
#> $measures$codelist$Petal.Length
#> NULL
#> 
#> $measures$codelist$Petal.Width
#> NULL
#> 
#> 
#> 
#> $attributes
#> $attributes$names
#> [1] "Species"
#> 
#> $attributes$class
#>  Species 
#> "factor" 
#> 
#> $attributes$isDefinedBy
#>                            Species 
#> "http://purl.org/linked-data/cube" 
#> 
#> $attributes$codelist
#> $attributes$codelist$Species
#> NULL
#> 
#> 
#> 
#> $issued
#> [1] 1935
#> 
#> $creator
#> [1] "Edgar Anderson [aut]"
#> 
#> $source
#> [1] "https://doi.org/10.1111/j.1469-1809.1936.tb02137.x"
#> 
#> $publisher
#> [1] "American Iris Society"
#> 
#> $language
#> [1] "eng"
```

## Motivation

Carl Boettiger: [A tidyverse lover’s intro to
RDF](https://cran.r-project.org/web/packages/rdflib/vignettes/rdf_intro.html)

## Code of Conduct

Please note that the StatCodelists project is released with a
[Contributor Code of
Conduct](https://contributor-covenant.org/version/2/1/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.

[^1]: RDF Data Cube Vocabulary, W3C Recommendation 16 January 2014
    <https://www.w3.org/TR/vocab-data-cube/>, Introduction to SDMX data
    modeling
    <https://www.unescap.org/sites/default/files/Session_4_SDMX_Data_Modeling_%20Intro_UNSD_WS_National_SDG_10-13Sep2019.pdf>
