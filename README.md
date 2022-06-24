
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
petal_length <- dataset(iris, 
        obs_id = NULL, 
        dimensions=NULL, 
        measurements = "Petal.Length", 
        attributes = "Species", 
        Title = "Iris Subset", unit = "mm", 
        Publisher = "Reprex")

petal_width <- dataset(iris, 
        obs_id = NULL, 
        dimensions=NULL, 
        measurements = "Petal.Width", 
        attributes = "Species", 
        Title = "Iris Subset", unit = "mm", 
        Publisher = "Reprex")

require(dplyr)
#> Loading required package: dplyr
#> Warning: package 'dplyr' was built under R version 4.1.3
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union
petal_length %>% left_join (petal_width, by = c("obs_id", "Species"))
#>     obs_id Petal.Length    Species Petal.Width
#> 1        1          1.4     setosa         0.2
#> 2        2          1.4     setosa         0.2
#> 3        3          1.3     setosa         0.2
#> 4        4          1.5     setosa         0.2
#> 5        5          1.4     setosa         0.2
#> 6        6          1.7     setosa         0.4
#> 7        7          1.4     setosa         0.3
#> 8        8          1.5     setosa         0.2
#> 9        9          1.4     setosa         0.2
#> 10      10          1.5     setosa         0.1
#> 11      11          1.5     setosa         0.2
#> 12      12          1.6     setosa         0.2
#> 13      13          1.4     setosa         0.1
#> 14      14          1.1     setosa         0.1
#> 15      15          1.2     setosa         0.2
#> 16      16          1.5     setosa         0.4
#> 17      17          1.3     setosa         0.4
#> 18      18          1.4     setosa         0.3
#> 19      19          1.7     setosa         0.3
#> 20      20          1.5     setosa         0.3
#> 21      21          1.7     setosa         0.2
#> 22      22          1.5     setosa         0.4
#> 23      23          1.0     setosa         0.2
#> 24      24          1.7     setosa         0.5
#> 25      25          1.9     setosa         0.2
#> 26      26          1.6     setosa         0.2
#> 27      27          1.6     setosa         0.4
#> 28      28          1.5     setosa         0.2
#> 29      29          1.4     setosa         0.2
#> 30      30          1.6     setosa         0.2
#> 31      31          1.6     setosa         0.2
#> 32      32          1.5     setosa         0.4
#> 33      33          1.5     setosa         0.1
#> 34      34          1.4     setosa         0.2
#> 35      35          1.5     setosa         0.2
#> 36      36          1.2     setosa         0.2
#> 37      37          1.3     setosa         0.2
#> 38      38          1.4     setosa         0.1
#> 39      39          1.3     setosa         0.2
#> 40      40          1.5     setosa         0.2
#> 41      41          1.3     setosa         0.3
#> 42      42          1.3     setosa         0.3
#> 43      43          1.3     setosa         0.2
#> 44      44          1.6     setosa         0.6
#> 45      45          1.9     setosa         0.4
#> 46      46          1.4     setosa         0.3
#> 47      47          1.6     setosa         0.2
#> 48      48          1.4     setosa         0.2
#> 49      49          1.5     setosa         0.2
#> 50      50          1.4     setosa         0.2
#> 51      51          4.7 versicolor         1.4
#> 52      52          4.5 versicolor         1.5
#> 53      53          4.9 versicolor         1.5
#> 54      54          4.0 versicolor         1.3
#> 55      55          4.6 versicolor         1.5
#> 56      56          4.5 versicolor         1.3
#> 57      57          4.7 versicolor         1.6
#> 58      58          3.3 versicolor         1.0
#> 59      59          4.6 versicolor         1.3
#> 60      60          3.9 versicolor         1.4
#> 61      61          3.5 versicolor         1.0
#> 62      62          4.2 versicolor         1.5
#> 63      63          4.0 versicolor         1.0
#> 64      64          4.7 versicolor         1.4
#> 65      65          3.6 versicolor         1.3
#> 66      66          4.4 versicolor         1.4
#> 67      67          4.5 versicolor         1.5
#> 68      68          4.1 versicolor         1.0
#> 69      69          4.5 versicolor         1.5
#> 70      70          3.9 versicolor         1.1
#> 71      71          4.8 versicolor         1.8
#> 72      72          4.0 versicolor         1.3
#> 73      73          4.9 versicolor         1.5
#> 74      74          4.7 versicolor         1.2
#> 75      75          4.3 versicolor         1.3
#> 76      76          4.4 versicolor         1.4
#> 77      77          4.8 versicolor         1.4
#> 78      78          5.0 versicolor         1.7
#> 79      79          4.5 versicolor         1.5
#> 80      80          3.5 versicolor         1.0
#> 81      81          3.8 versicolor         1.1
#> 82      82          3.7 versicolor         1.0
#> 83      83          3.9 versicolor         1.2
#> 84      84          5.1 versicolor         1.6
#> 85      85          4.5 versicolor         1.5
#> 86      86          4.5 versicolor         1.6
#> 87      87          4.7 versicolor         1.5
#> 88      88          4.4 versicolor         1.3
#> 89      89          4.1 versicolor         1.3
#> 90      90          4.0 versicolor         1.3
#> 91      91          4.4 versicolor         1.2
#> 92      92          4.6 versicolor         1.4
#> 93      93          4.0 versicolor         1.2
#> 94      94          3.3 versicolor         1.0
#> 95      95          4.2 versicolor         1.3
#> 96      96          4.2 versicolor         1.2
#> 97      97          4.2 versicolor         1.3
#> 98      98          4.3 versicolor         1.3
#> 99      99          3.0 versicolor         1.1
#> 100    100          4.1 versicolor         1.3
#> 101    101          6.0  virginica         2.5
#> 102    102          5.1  virginica         1.9
#> 103    103          5.9  virginica         2.1
#> 104    104          5.6  virginica         1.8
#> 105    105          5.8  virginica         2.2
#> 106    106          6.6  virginica         2.1
#> 107    107          4.5  virginica         1.7
#> 108    108          6.3  virginica         1.8
#> 109    109          5.8  virginica         1.8
#> 110    110          6.1  virginica         2.5
#> 111    111          5.1  virginica         2.0
#> 112    112          5.3  virginica         1.9
#> 113    113          5.5  virginica         2.1
#> 114    114          5.0  virginica         2.0
#> 115    115          5.1  virginica         2.4
#> 116    116          5.3  virginica         2.3
#> 117    117          5.5  virginica         1.8
#> 118    118          6.7  virginica         2.2
#> 119    119          6.9  virginica         2.3
#> 120    120          5.0  virginica         1.5
#> 121    121          5.7  virginica         2.3
#> 122    122          4.9  virginica         2.0
#> 123    123          6.7  virginica         2.0
#> 124    124          4.9  virginica         1.8
#> 125    125          5.7  virginica         2.1
#> 126    126          6.0  virginica         1.8
#> 127    127          4.8  virginica         1.8
#> 128    128          4.9  virginica         1.8
#> 129    129          5.6  virginica         2.1
#> 130    130          5.8  virginica         1.6
#> 131    131          6.1  virginica         1.9
#> 132    132          6.4  virginica         2.0
#> 133    133          5.6  virginica         2.2
#> 134    134          5.1  virginica         1.5
#> 135    135          5.6  virginica         1.4
#> 136    136          6.1  virginica         2.3
#> 137    137          5.6  virginica         2.4
#> 138    138          5.5  virginica         1.8
#> 139    139          4.8  virginica         1.8
#> 140    140          5.4  virginica         2.1
#> 141    141          5.6  virginica         2.4
#> 142    142          5.1  virginica         2.3
#> 143    143          5.1  virginica         1.9
#> 144    144          5.9  virginica         2.3
#> 145    145          5.7  virginica         2.5
#> 146    146          5.2  virginica         2.3
#> 147    147          5.0  virginica         1.9
#> 148    148          5.2  virginica         2.0
#> 149    149          5.4  virginica         2.3
#> 150    150          5.1  virginica         1.8
```

The obvious motivation of this format is that the datasets can be easily
integrated, joined, combined, because they are tidy.

In a the long form, they easily lend themselves for RDF format:

``` r
names(petal_width)[2] <- "measurement"
petal_width$petal_dimension <- "Petal width"
names(petal_length)[2] <- "measurement"
petal_length$petal_dimension <- "Petal length"

head(use_function(petal_width,  .f = "rbind", y = petal_length))
#>   obs_id measurement Species petal_dimension
#> 1      1         0.2  setosa     Petal width
#> 2      2         0.2  setosa     Petal width
#> 3      3         0.2  setosa     Petal width
#> 4      4         0.2  setosa     Petal width
#> 5      5         0.2  setosa     Petal width
#> 6      6         0.4  setosa     Petal width
```

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

## Reproducible Datasets

``` r
temp_file <- file.path(tempdir(), "iris.csv")
write.csv(iris, temp_file)
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
#> $Date
#> $Date$Date
#> [1] "2022-06-24 09:45:07 CEST"
#> 
#> $Date$dateType
#> [1] "Created"
#> 
#> $Date$dateInformation
#> [1] "dataset::dataset(dataset_id=iris_dataset, obs_id=obs_id, dimension=NULL, measurements=c(\"Sepal.Length\", \"Sepal.Width\", \"Petal.Length\", \"Petal.Width\"), attributes=Species, Title=Iris Dataset, Subject=NULL, Publisher=NULL, License=NULL)"
#> 
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
