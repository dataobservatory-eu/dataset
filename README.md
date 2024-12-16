
<!-- README.md is generated from README.Rmd. Please edit that file -->

# The dataset R Package <a href='https://dataset.dataobservatory.eu/'><img src='man/figures/logo.png' align="right" /></a>

<!-- badges: start -->

[![Codecov test
coverage](https://codecov.io/gh/antaldaniel/dataset/graph/badge.svg)](https://app.codecov.io/gh/antaldaniel/dataset)
[![lifecycle](https://lifecycle.r-lib.org/articles/figures/lifecycle-experimental.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![Project Status:
WIP](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip)
[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/dataset)](https://cran.r-project.org/package=dataset)
[![CRAN_time_from_release](https://www.r-pkg.org/badges/ago/dataset)](https://cran.r-project.org/package=dataset)
[![Status at rOpenSci Software Peer
Review](https://badges.ropensci.org/553_status.svg)](https://github.com/ropensci/software-review/issues/553)
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.14499632.svg)](https://zenodo.org/record/6950435#.YukDAXZBzIU)
[![devel-version](https://img.shields.io/badge/devel%20version-0.3.3.0007-blue.svg)](https://github.com/antaldaniel/dataset)
[![dataobservatory](https://img.shields.io/badge/ecosystem-dataobservatory.eu-3EA135.svg)](https://dataobservatory.eu/)
<!-- badges: end -->

The aim of the *dataset* package is to make tidy datasets easier to
release, exchange and reuse. It organizes and formats data frame R
objects into well-referenced, well-described, interoperable datasets
into release and reuse ready form.

1.  Offer a way to better utilise the `utils:bibentry` bibliographic
    entry objects by extending them with the fields of the Dublin Core
    and DataCite tenders, and making them detachable from the data. This
    extension aims to work with a
    [data.frame](https://www.rdocumentation.org/packages/base/versions/3.6.2/topics/data.frame)
    or an inherited
    [tibble](https://tibble.tidyverse.org/reference/tibble.html),
    [tsibble](https://tsibble.tidyverts.org/) or
    [data.table](https://rdatatable.gitlab.io/data.table/). See for more
    information the [Bibentry for FAIR
    datasets](https://dataset.dataobservatory.eu/articles/bibentry.html)
    vignette.
2.  Extending the `haven_labelled` class of the `tidyverse` for
    consistently labelled categorical variables with linked (standard)
    definitions and units of measures in our
    [defined](https://dataset.dataobservatory.eu/articles/defined.html)
    class.
3.  Offering a new data frame format, `dataset_df` that extends tibbles
    with semantically rich metadata, ready to be shared on open data
    exchange platforms and in data repositories. This s3 class is aimed
    at developers and we are working on several packages that provide
    interoperability with SDMX statistical data exchange platforms,
    Wikidata, or the EU Open Data portal. Read more in the [Create
    Datasets that are Easy to Share Exchange and
    Extend](https://dataset.dataobservatory.eu/articles/dataset_df.html)
    vignette.

<!---
&#10;The primary aim of dataset is create well-referenced, well-described, interoperable datasets from data.frames, tibbles or data.tables that translate well into the W3C DataSet definition within the [Data Cube Vocabulary](https://www.w3.org/TR/vocab-data-cube/) in a reproducible manner. The data cube model in itself is is originated in the _Statistical Data and Metadata eXchange_, and it is almost fully harmonized with the Resource Description Framework (RDF), the standard model for data interchange on the web^[RDF Data Cube Vocabulary, W3C Recommendation 16 January 2014  <https://www.w3.org/TR/vocab-data-cube/>, Introduction to SDMX data modeling <https://www.unescap.org/sites/default/files/Session_4_SDMX_Data_Modeling_%20Intro_UNSD_WS_National_SDG_10-13Sep2019.pdf>].
&#10;--->

Further development plans for peer-review are added in till 5 November
2024 here: [New
Requirement](https://dataset.dataobservatory.eu/articles/new-requirements.html)
setting.

You can install the development version of dataset with
`remotes::install_github()`:

``` r
remotes::install_github("dataobservatory-eu/dataset", build = FALSE)
```

The current version of the `dataset` package is in an early,
experimental stage. You can follow the discussion of this package on
[rOpenSci](https://github.com/ropensci/software-review/issues/553).

``` r
library(dataset)
iris_ds <- dataset_df(
  x = iris,
  reference = list(
    title = "Iris Dataset",
    author = person("Edgar", "Anderson", role = "aut"),
    publisher = "American Iris Society",
    source = "https://doi.org/10.1111/j.1469-1809.1936.tb02137.x",
    date = 1935,
    language = "en",
    description = "This famous (Fisher's or Anderson's) iris data set."
  )
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
#>    x.Sepal.Length        x.Sepal.Width       x.Petal.Length        x.Petal.Width          x.Species     
#>  Min.   :4.300000     Min.   :2.000000     Min.   :1.000        Min.   :0.1000000    setosa    :50      
#>  1st Qu.:5.100000     1st Qu.:2.800000     1st Qu.:1.600        1st Qu.:0.3000000    versicolor:50      
#>  Median :5.800000     Median :3.000000     Median :4.350        Median :1.3000000    virginica :50      
#>  Mean   :5.843333     Mean   :3.057333     Mean   :3.758        Mean   :1.1993333    NA                 
#>  3rd Qu.:6.400000     3rd Qu.:3.300000     3rd Qu.:5.100        3rd Qu.:1.8000000    NA                 
#>  Max.   :7.900000     Max.   :4.400000     Max.   :6.900        Max.   :2.5000000    NA
```

A brief description of the extended metadata attributes:

``` r
print(get_bibentry(iris_ds), "Bibtex")
#> @Misc{,
#>   title = {Iris Dataset},
#>   author = {Edgar Anderson},
#>   publisher = {American Iris Society},
#>   year = {2024},
#>   resourcetype = {Dataset},
#>   version = {0.1.0},
#>   description = {This famous (Fisher's or Anderson's) iris data set.},
#>   language = {en},
#>   format = {application/r-rds},
#>   rights = {:unas},
#>   date = {1935},
#> }
```

``` r
paste0("Publisher:", publisher(iris_ds))
#> [1] "Publisher:American Iris Society"
paste0("Rights:", rights(iris_ds))
#> [1] "Rights::unas"
```

The descriptive metadata are added to a `utils::bibentry` object which
has many printing options (see `?bibentry`). (The `utils` package is
installed by default with every R system, so working with utils is not
an extra dependency.)

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
