
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

The goal of dataset is to create datasets from standared R objects
(data.fame, data.table, tibble, or well-structured lists like json) that
are highly interoperable and can be placed into relational databases,
semantic web applications, archives, repositories. They follow the
[FAIR](https://www.go-fair.org/fair-principles/) principles: they are
findable, accessible, interoperable and reusable.

## Installation

You can install the development version of dataset from Github:

``` r
remotes::install_package(dataobservatory-eu/dataset)
```

## FAir: Findable & Accessible Datasets

If you work with R, you are almost certainly familiar with the iris
dataset. The *?iris* will provide you with some information about this
often used dataset in tutorials. But how you make sure that you do not
forget its important properties?

The function datacite [DataCite](https://datacite.org/) add at least the
mandatory properties of the [DataCite Metadata Schema
4.3](https://schema.datacite.org/), a list of core metadata properties
chosen for an accurate and consistent identification of a resource for
citation and retrieval purposes. DataCite is largely interoperable to
the other similar international standard, the [Dublin
Core](https://www.dublincore.org/). We will later add similar
`dublincore` function, however, the practical differences are so small
that adjustments, if needed, can be easily made by hand.

``` r
library(dataset)
iris_dataset <- datacite_add(
  x = iris,
  Title = "Iris Dataset",
  Creator = person("Anderson", "Edgar", role = "aut"),
  Publisher=" American Iris Society",
  PublicationYear = 1935,
  Description = "This famous (Fisher's or Anderson's) iris data set gives the measurements in centimeters of the variables sepal length and width and petal length and width, respectively, for 50 flowers from each of 3 species of iris. The species are Iris setosa, versicolor, and virginica.",
  Language = "en")
```

``` r
datacite(iris_dataset)
#> $names
#> [1] "Sepal.Length" "Sepal.Width"  "Petal.Length" "Petal.Width"  "Species"     
#> 
#> $title
#> [1] "Iris Dataset"
#> 
#> $creator
#> [1] "Anderson Edgar [aut]"
#> 
#> $publisher
#> [1] " American Iris Society"
#> 
#> $issued
#> [1] 1935
#> 
#> $PublicationYear
#> [1] 1935
#> 
#> $ResourceType
#> [1] "Dataset"
#> 
#> $description
#> [1] "This famous (Fisher's or Anderson's) iris data set gives the measurements in centimeters of the variables sepal length and width and petal length and width, respectively, for 50 flowers from each of 3 species of iris. The species are Iris setosa, versicolor, and virginica."
#> 
#> $language
#> [1] "eng"
#> 
#> $Size
#> [1] "10.34 kB [10.09 KiB]"
```

The `df` parameter can be any well-structured R object that meets the
definition of a dataset: a data.frame, or an inherited class of it
(data.table, [tibble](https://tibble.tidyverse.org/)); or a
well-structured list (for example, a
[json](https://arxiv.org/abs/1403.2805) object.)

``` r
iris_bibentry <- bibentry_dataset(iris_dataset)
toBibtex(iris_bibentry)
#> @Misc{,
#>   title = {Iris Dataset},
#>   author = {Anderson Edgar},
#>   publisher = { American Iris Society},
#>   size = {10.34 kB [10.09 KiB]},
#>   year = {1935},
#> }
```

``` r
print(iris_bibentry, sytle="html")
```

Edgar A (1935). “Iris Dataset.”

## faIR: Interoperable & Reusable Datasets

The interoperability and reusability of datasets is greatly enhanced if
they follow a standardized and practical format. Our datasets follow the
tidy data principles[^1] and are interoperable with the W3C<sup>\[DF
Data Cube Vocabulary\]</sup>\[RDF Data Cube Vocabulary, W3C
Recommendation 16 January 2014
<https://www.w3.org/TR/vocab-data-cube/#metadata>\] (semantic web) and
SDMX (statistical) dataset definitions.

Both W3C and SDMX uses are more complex object, the Datacube in its
description. The dataset is a redused datacube. To adhere to tidy data
principles and easy use in reproducible resaerch workflows, we further
reduced our subjective definition of the dataset.

-   The `dataset` constructor first subsets the dataset for the `obs_id`
    observation identifier, and if it is missing, it creates one.
-   Then it selects the `dimensions`, such as geographic concept or time
    concept. The iris dataset does not have these variables, so we do
    not select anything.
-   Next we select the `measurements`. In case only one `measurement` is
    present, we have a long-form dataset that can be easily serialized
    into an `RDF` object, for example.
-   Next we select any `attributes` that are unlikely to be used for
    statistical aggregation (unlike the dimensions) and which are not
    measured values.
-   We can pass on further optional dataset attributes. These attributes
    do not correspond with a single observation, rather the entire
    dataset.

``` r
petal_length <- dataset(subset(iris, select = c("Petal.Length", "Species")), 
        dimensions=NULL, 
        measures = "Petal.Length", 
        attributes = "Species")

petal_width <- dataset(subset(iris, select = c("Petal.Width", "Species")), 
        dimensions=NULL, 
        measures = "Petal.Width", 
        attribute = "Species")

require(dplyr)
#> Loading required package: dplyr
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union
petal_length %>%
  left_join (petal_width, by = c("Species")) %>%
  sample_n(10)
#>    Petal.Length    Species Petal.Width
#> 1           1.5     setosa         0.2
#> 2           4.3 versicolor         1.5
#> 3           4.5 versicolor         1.5
#> 4           1.0     setosa         0.2
#> 5           5.6  virginica         1.8
#> 6           4.5 versicolor         1.4
#> 7           1.4     setosa         0.2
#> 8           4.8  virginica         2.1
#> 9           6.7  virginica         2.0
#> 10          4.1 versicolor         1.1
```

The obvious motivation of this format is that the datasets can be easily
integrated, joined, combined, because they are tidy.

## Code of Conduct

Please note that the StatCodelists project is released with a
[Contributor Code of
Conduct](https://contributor-covenant.org/version/2/1/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.

[^1]: Wickham, H.(2014). Tidy Data. Journal of Statistical Software,
    59(10), 1–23. <https://doi.org/10.18637/jss.v059.i10>
