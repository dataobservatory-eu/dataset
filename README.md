
<!-- README.md is generated from README.Rmd. Please edit that file -->

# The dataset R Package

<!-- badges: start -->

[![lifecycle](https://lifecycle.r-lib.org/articles/figures/lifecycle-experimental.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.6703765.svg)](https://doi.org/10.5281/zenodo.6703765)
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
devtools::install_package(dataobservatory-eu/dataset)
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
iris_dataset <- datacite(
  df = iris,
  Title = "Iris Dataset",
  Creator = person("Anderson", "Edgar", role = "aut"),
  Publisher=" American Iris Society",
  PublicationYear = 1935,
  Description = "This famous (Fisher's or Anderson's) iris data set gives the measurements in centimeters of the variables sepal length and width and petal length and width, respectively, for 50 flowers from each of 3 species of iris. The species are Iris setosa, versicolor, and virginica.",
  Language = "en")
```

The `df` parameter can be any well-structured R object that meets the
definition of a dataset: a data.frame, or an inherited class of it
(data.table, [tibble](https://tibble.tidyverse.org/)); or a
well-structured list (for example, a
[json](https://arxiv.org/abs/1403.2805) object.)

``` r
iris_bibentry <- dataset_bibentry(iris_dataset)
toBibtex(iris_bibentry)
#> @Misc{,
#>   title = {Iris Dataset},
#>   year = {1935},
#>   author = {Anderson Edgar},
#>   size = {9448},
#> }
```

``` r
print(iris_bibentry, sytle="html")
```

Edgar A (1935). “Iris Dataset.”

## faIR: Interoperable & Reusable Datasets

The interoperability and reusability of datasets is greatly enhanced if
they follow a standardized and practical format. Our datasets follow the
tidy data principles[^1] and are interoperable with the W3C <sup>\[DF
Data Cube Vocabulary\]</sup>\[RDF Data Cube Vocabulary, W3C
Recommendation 16 January 2014
<https://www.w3.org/TR/vocab-data-cube/#metadata>\] (semantic web) and
SDMX (statistical) dataset definitions.

Both W3C and SDMX uses are more complex object, the Datacube in its
description. The dataset is a redused datacube. To adhere to tidy data
principles and easy use in reproducible resaerch workflows, we further
reduced our subjective definition of the dataset.

-   `obs_id`: The unique identifier of the observations. If they are not
    present, the row.names() will be used—R row names may be lost when
    exporting to non-R files and it is better to make them explicit. In
    a tidy dataset there are exactly one observation identifier, because
    each row represents an observation.
-   `dimensions`: Variables that can be used for aggregation, for
    examaple, around a geographical concept like countries. The most
    important dimensions are a geographical concept (where was the
    measurement made) and a time concept (when was the measurement made,
    or what is the reference time period.)
-   `measurements`: Actual measurements of the observations.
-   `attributes`: Non-measured attributes, categories or constants.
-   `unit`: The unit of the measurement(s). In the tidy data defintion,
    each table has only one unit, measurements in different units go
    into different tables.
-   `dataset_id`: The identifier of the dataset. When joining datasets,
    it may be desirable to keep the unit observation ids. The
    `dataset_id` and the `obs_id` will be used to create unique resource
    identifiers (URIs).

``` r
iris_ds <- dataset ( x = iris,
                     Title = "Iris Dataset",
                     dataset_id = "iris_dataset", 
                     obs_id = NULL,
                     dimensions = NULL,
                     measurements = c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width"),
                     attributes = c("Species"),
                     unit = list(code="MM", label = "milimeters")
)
attributes(iris_ds)
#> $names
#> [1] "obs_id"       "Sepal.Length" "Sepal.Width"  "Petal.Length" "Petal.Width" 
#> [6] "Species"     
#> 
#> $class
#> [1] "data.frame" "dataset"   
#> 
#> $row.names
#>   [1]   1   2   3   4   5   6   7   8   9  10  11  12  13  14  15  16  17  18
#>  [19]  19  20  21  22  23  24  25  26  27  28  29  30  31  32  33  34  35  36
#>  [37]  37  38  39  40  41  42  43  44  45  46  47  48  49  50  51  52  53  54
#>  [55]  55  56  57  58  59  60  61  62  63  64  65  66  67  68  69  70  71  72
#>  [73]  73  74  75  76  77  78  79  80  81  82  83  84  85  86  87  88  89  90
#>  [91]  91  92  93  94  95  96  97  98  99 100 101 102 103 104 105 106 107 108
#> [109] 109 110 111 112 113 114 115 116 117 118 119 120 121 122 123 124 125 126
#> [127] 127 128 129 130 131 132 133 134 135 136 137 138 139 140 141 142 143 144
#> [145] 145 146 147 148 149 150
#> 
#> $Modified
#> [1] "2022-06-23 17:54:32 CEST"
#> 
#> $dataset_id
#> [1] "iris_dataset"
#> 
#> $obs_id
#> [1] "obs_id"
#> 
#> $unit
#> $unit$code
#> [1] "MM"
#> 
#> $unit$label
#> [1] "milimeters"
#> 
#> 
#> $dimensions
#> [1] ""
#> 
#> $measurements
#> [1] "Sepal.Length" "Sepal.Width"  "Petal.Length" "Petal.Width" 
#> 
#> $attributes
#> [1] "Species"
```

If your application needs URIs, the first 5 elements of the iris dataset
can be referenced as `dataset_id#obs_id` will be used to create unique
resource identifiers (URIs). For the first five observations of the
`iris` dataset:
iris_dataset#1’,‘iris_dataset#2’,‘iris_dataset#3’,‘iris_dataset#4’,’iris_dataset#5

## Code of Conduct

Please note that the StatCodelists project is released with a
[Contributor Code of
Conduct](https://contributor-covenant.org/version/2/1/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.

[^1]: Wickham, H.(2014). Tidy Data. Journal of Statistical Software,
    59(10), 1–23. <https://doi.org/10.18637/jss.v059.i10>
