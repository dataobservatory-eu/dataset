
<!-- README.md is generated from README.Rmd. Please edit that file -->

# The dataset R Package <a href='https://dataset.dataobservatory.eu/'><img src='man/figures/logo.png' align="right" /></a>

<!-- badges: start -->

[![lifecycle](https://lifecycle.r-lib.org/articles/figures/lifecycle-experimental.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/dataset)](https://cran.r-project.org/package=dataset)
[![CRAN_time_from_release](https://www.r-pkg.org/badges/ago/dataset)](https://cran.r-project.org/package=dataset)
[![Status at rOpenSci Software Peer
Review](https://badges.ropensci.org/553_status.svg)](https://github.com/ropensci/software-review/issues/553)
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.7440192.svg)](https://zenodo.org/record/6950435#.YukDAXZBzIU)
[![devel-version](https://img.shields.io/badge/devel%20version-0.2.1-blue.svg)](https://github.com/dataobservatory-eu/dataset)
[![dataobservatory](https://img.shields.io/badge/ecosystem-dataobservatory.eu-3EA135.svg)](https://dataobservatory.eu/)
[![Follow
rOpenGov](https://img.shields.io/twitter/follow/ropengov.svg?style=social)](https://twitter.com/intent/follow?screen_name=ropengov)
[![Codecov test
coverage](https://codecov.io/gh/dataobservatory-eu/dataset/branch/master/graph/badge.svg)](https://app.codecov.io/gh/dataobservatory-eu/dataset?branch=master)
[![pkgcheck](https://github.com/dataobservatory-eu/dataset/workflows/pkgcheck/badge.svg)](https://github.com/dataobservatory-eu/dataset/actions?query=workflow%3Apkgcheck)
[![AppVeyor build
status](https://ci.appveyor.com/api/projects/status/github/dataobservatory-eu/dataset?branch=master&svg=true)](https://ci.appveyor.com/project/dataobservatory-eu/dataset)

<!-- badges: end -->

The primary aim of dataset is create well-referenced, well-described,
interoperable datasets from data.frames, tibbles or data.tables that
translate well into the W3C DataSet definition within the [Data Cube
Vocabulary](https://www.w3.org/TR/vocab-data-cube/) in a reproducible
manner. The data cube model in itself is is originated in the
*Statistical Data and Metadata eXchange*, and it is almost fully
harmonized with the Resource Description Framework (RDF), the standard
model for data interchange on the web[^1].

The development version of the `dataset` package is very significantly
differnet from the CRAN release. The documentation has not been
rewritten yet!

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
#> Anderson E (2023). "Iris Dataset."
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
#> Description: :unas
#> Creator: Edgar Anderson [aut]
#> Publisher: American Iris Society
#> Rights: :unas
```

``` r
# Remove lengthy attributes to see what is going on under the hoods:
iris_ds2 <- iris_ds
attr(iris_ds2, "DataStructure") <- NULL; attr(iris_ds2, "row.names") <- NULL
str(iris_ds2)
#> Classes 'dataset' and 'data.frame':  0 obs. of  5 variables:
#>  $ Sepal.Length: num  5.1 4.9 4.7 4.6 5 5.4 4.6 5 4.4 4.9 ...
#>  $ Sepal.Width : num  3.5 3 3.2 3.1 3.6 3.9 3.4 3.4 2.9 3.1 ...
#>  $ Petal.Length: num  1.4 1.4 1.3 1.5 1.4 1.7 1.4 1.5 1.4 1.5 ...
#>  $ Petal.Width : num  0.2 0.2 0.2 0.2 0.2 0.4 0.3 0.2 0.2 0.1 ...
#>  $ Species     : Factor w/ 3 levels "setosa","versicolor",..: 1 1 1 1 1 1 1 1 1 1 ...
#>  - attr(*, "DataBibentry")=Class 'bibentry'  hidden list of 1
#>   ..$ :List of 11
#>   .. ..$ title       : chr "Iris Dataset"
#>   .. ..$ author      :Class 'person'  hidden list of 1
#>   .. .. ..$ :List of 5
#>   .. .. .. ..$ given  : chr "Edgar"
#>   .. .. .. ..$ family : chr "Anderson"
#>   .. .. .. ..$ role   : chr "aut"
#>   .. .. .. ..$ email  : NULL
#>   .. .. .. ..$ comment: NULL
#>   .. ..$ publisher   : chr "American Iris Society"
#>   .. ..$ year        : chr "2023"
#>   .. ..$ resourcetype: chr "Dataset"
#>   .. ..$ identifier  : chr ":tba"
#>   .. ..$ version     : chr "0.1.0"
#>   .. ..$ description : chr ":unas"
#>   .. ..$ language    : chr "en"
#>   .. ..$ format      : chr "application/r-rds"
#>   .. ..$ rights      : chr ":unas"
#>   .. ..- attr(*, "bibtype")= chr "Misc"
#>  - attr(*, "Subject")=List of 6
#>   ..$ term              : chr ""
#>   ..$ subjectScheme     : chr ""
#>   ..$ schemeURI         : chr ""
#>   ..$ valueURI          : chr ""
#>   ..$ classificationCode: NULL
#>   ..$ prefix            : chr ""
#>   ..- attr(*, "class")= chr [1:2] "subject" "list"
```

``` r
paste0("Publisher:", publisher(iris_ds2))
#> [1] "Publisher:American Iris Society"
paste0("Rights:", rights(iris_ds2))
#> [1] "Rights::unas"
paste0("Rights:", rights(iris_ds2))
#> [1] "Rights::unas"
```

The descriptive metadata are added to a `utils::bibentry` object which
has many printing options (see `?bibentry`).

``` r
mybibentry <- dataset_bibentry(iris_ds)
print(mybibentry, "text")
#> Anderson E (2023). "Iris Dataset."
print(mybibentry, "Bibtex")
#> @Misc{,
#>   title = {Iris Dataset},
#>   author = {Edgar Anderson},
#>   publisher = {American Iris Society},
#>   year = {2023},
#>   resourcetype = {Dataset},
#>   identifier = {:tba},
#>   version = {0.1.0},
#>   description = {:unas},
#>   language = {en},
#>   format = {application/r-rds},
#>   rights = {:unas},
#> }
```

``` r
rights(iris_ds2) <- "CC0"
rights(iris_ds2)
#> [1] "CC0"
rights(iris_ds2, overwrite = FALSE) <- "GNU-2"
#> The dataset has already a rights field: CC0
```

Some important metadata is protected from accidental overwriting (except
for the default `:unas` unassigned and `:tba` to-be-announced values.)

``` r
rights(iris_ds2, overwrite = TRUE)  <- "GNU-2"
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

[^1]: RDF Data Cube Vocabulary, W3C Recommendation 16 January 2014
    <https://www.w3.org/TR/vocab-data-cube/>, Introduction to SDMX data
    modeling
    <https://www.unescap.org/sites/default/files/Session_4_SDMX_Data_Modeling_%20Intro_UNSD_WS_National_SDG_10-13Sep2019.pdf>
