
<!-- README.md is generated from README.Rmd. Please edit that file -->

# The dataset R Package <a href='https://dataset.dataobservatory.eu/'><img src='man/figures/logo.png' align="right" /></a>

<!-- badges: start -->

[![lifecycle](https://lifecycle.r-lib.org/articles/figures/lifecycle-experimental.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![Project Status:
WIP](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip)
[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/dataset)](https://cran.r-project.org/package=dataset)
[![CRAN_time_from_release](https://www.r-pkg.org/badges/ago/dataset)](https://cran.r-project.org/package=dataset)
[![Status at rOpenSci Software Peer
Review](https://badges.ropensci.org/553_status.svg)](https://github.com/ropensci/software-review/issues/553)
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.10574908.svg)](https://zenodo.org/record/6950435#.YukDAXZBzIU)
[![devel-version](https://img.shields.io/badge/devel%20version-0.3.1-blue.svg)](https://github.com/dataobservatory-eu/dataset)
[![dataobservatory](https://img.shields.io/badge/ecosystem-dataobservatory.eu-3EA135.svg)](https://dataobservatory.eu/)
[![Codecov test
coverage](https://codecov.io/gh/dataobservatory-eu/dataset/branch/master/graph/badge.svg)](https://app.codecov.io/gh/dataobservatory-eu/dataset?branch=master)
[![pkgcheck](https://github.com/dataobservatory-eu/dataset/workflows/pkgcheck/badge.svg)](https://github.com/dataobservatory-eu/dataset/actions?query=workflow%3Apkgcheck)
[![AppVeyor build
status](https://ci.appveyor.com/api/projects/status/github/dataobservatory-eu/dataset?branch=master&svg=true)](https://ci.appveyor.com/project/dataobservatory-eu/dataset)
[![R-CMD-check](https://github.com/dataobservatory-eu/dataset/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/dataobservatory-eu/dataset/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

The dataset package extension to the R statistical environment aims to
ensure that the most important R object that contains a dataset, i.e. a
[data.frame](https://www.rdocumentation.org/packages/base/versions/3.6.2/topics/data.frame)
or an inherited
[tibble](https://tibble.tidyverse.org/reference/tibble.html),
[tsibble](https://tsibble.tidyverts.org/) or
[data.table](https://rdatatable.gitlab.io/data.table/) contains
important metadata for the reuse and validation of the dataset contents.
We aim to offer a novel solution to support individuals or small groups
of data scientists working in various business, academic or policy
research functions who cannot count on the support of librarians,
knowledge engineers, and extensive documentation processes.

The dataset package extends the concept of tidy data and adds further,
standardized semantic information to the user’s dataset to increase the
(re-)use value of the data object.

- [x] More descriptive information about the dataset as a creation, its
  authors, contributors, reuse rights and other metadata to make it
  easier to find and use.
- [x] More standardized and linked metadata, such as standard variable
  definitions and code lists, enable the data owner to gather far more
  information from third parties or for third parties to understand and
  use the data correctly.
- [x] More information about the data provenance makes the quality
  assessment easier and reduces the need for time-consuming and
  unnecessary re-processing steps.
- [x] More structural information about the data makes it more
  accessible to reuse and join with new information, making it less
  error-prone for logical errors.

<!---
&#10;The primary aim of dataset is create well-referenced, well-described, interoperable datasets from data.frames, tibbles or data.tables that translate well into the W3C DataSet definition within the [Data Cube Vocabulary](https://www.w3.org/TR/vocab-data-cube/) in a reproducible manner. The data cube model in itself is is originated in the _Statistical Data and Metadata eXchange_, and it is almost fully harmonized with the Resource Description Framework (RDF), the standard model for data interchange on the web^[RDF Data Cube Vocabulary, W3C Recommendation 16 January 2014  <https://www.w3.org/TR/vocab-data-cube/>, Introduction to SDMX data modeling <https://www.unescap.org/sites/default/files/Session_4_SDMX_Data_Modeling_%20Intro_UNSD_WS_National_SDG_10-13Sep2019.pdf>].
&#10;--->

The current version of the `dataset` package is in an early,
experimental stage. You can follow the discussion of this package on
[rOpenSci](https://github.com/ropensci/software-review/issues/553).

``` r
library(dataset)
iris_ds <- dataset(
  x = iris,
  title = "Iris Dataset",
  author = person("Edgar", "Anderson", role = "aut"),
  publisher = "American Iris Society",
  source = "https://doi.org/10.1111/j.1469-1809.1936.tb02137.x",
  date = 1935,
  language = "en",
  description = "This famous (Fisher's or Anderson's) iris data set."
)
```

It is mandatory to add a `title`, `author` to a dataset, and if the
`date` is not specified, the current date will be added.

As the dataset at this point is just created, if it is not published
yet, the `identifer` receives the default `:tba` value, a `version` of
0.1.0 and the `:unas` (unassigned) `publisher` field.

The dataset behaves as expected, with all data.frame methods applicable.
If the dataset was originally a tibble or data.table object, it retained
all methods of these s3 classes because the dataset class only
implements further methods in the attributes of the original object.

``` r
summary(iris_ds)
#> Anderson E (2024). "Iris Dataset."
#> Further metadata: describe(iris_ds)
#>   Sepal.Length    Sepal.Width     Petal.Length    Petal.Width   
#>  Min.   :4.300   Min.   :2.000   Min.   :1.000   Min.   :0.100  
#>  1st Qu.:5.100   1st Qu.:2.800   1st Qu.:1.600   1st Qu.:0.300  
#>  Median :5.800   Median :3.000   Median :4.350   Median :1.300  
#>  Mean   :5.843   Mean   :3.057   Mean   :3.758   Mean   :1.199  
#>  3rd Qu.:6.400   3rd Qu.:3.300   3rd Qu.:5.100   3rd Qu.:1.800  
#>  Max.   :7.900   Max.   :4.400   Max.   :6.900   Max.   :2.500  
#>        Species  
#>  setosa    :50  
#>  versicolor:50  
#>  virginica :50  
#>                 
#>                 
#> 
```

A brief description of the extended metadata attributes:

``` r
describe(iris_ds)
#> Iris Dataset 
#> Dataset with 150 observations (rows) and 5 variables (columns).
#> Description: This famous (Fisher's or Anderson's) iris data set.
#> Creator: Edgar Anderson [aut]
#> Publisher: American Iris Society
```

``` r
paste0("Publisher:", publisher(iris_ds))
#> [1] "Publisher:American Iris Society"
paste0("Rights:", rights(iris_ds))
#> [1] "Rights::unas"
```

The descriptive metadata are added to a `utils::bibentry` object which
has many printing options (see `?bibentry`).

``` r
mybibentry <- dataset_bibentry(iris_ds)
print(mybibentry, "text")
#> Anderson E (2024). "Iris Dataset."
print(mybibentry, "Bibtex")
#> @Misc{,
#>   title = {Iris Dataset},
#>   author = {Edgar Anderson},
#>   publisher = {American Iris Society},
#>   year = {2024},
#>   resourcetype = {Dataset},
#>   identifier = {:tba},
#>   version = {0.1.0},
#>   description = {This famous (Fisher's or Anderson's) iris data set.},
#>   language = {en},
#>   format = {application/r-rds},
#>   rights = {:unas},
#> }
```

``` r
rights(iris_ds) <- "CC0"
rights(iris_ds)
#> [1] "CC0"
rights(iris_ds, overwrite = FALSE) <- "GNU-2"
#> The dataset has already a rights field: CC0
```

Some important metadata is protected from accidental overwriting (except
for the default `:unas` unassigned and `:tba` to-be-announced values.)

``` r
rights(iris_ds, overwrite = TRUE)  <- "GNU-2"
```

## Code of Conduct

Please note that the `dataset` package is released with a [Contributor
Code of
Conduct](https://contributor-covenant.org/version/2/1/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.

Furthermore, [rOpenSci Community Contributing
Guide](https://contributing.ropensci.org/) - *A guide to help people
find ways to contribute to rOpenSci* is also applicable, because
`dataset` is under software review for potential inclusion in
[rOpenSci](https://github.com/ropensci/software-review/issues/553).
