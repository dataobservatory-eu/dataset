
<!-- README.md is generated from README.Rmd. Please edit that file -->

# The dataset R Package <a href='https://dataset.dataobservatory.eu/'><img src='man/figures/logo.png' align="right" /></a>

<!-- badges: start -->

[![Codecov test
coverage](https://codecov.io/gh/dataobservatory-eu/dataset/graph/badge.svg)](https://app.codecov.io/gh/dataobservatory-eu/dataset)
[![rhub](https://github.com/dataobservatory-eu/dataset/actions/workflows/rhub.yaml/badge.svg)](https://github.com/dataobservatory-eu/dataset/actions/workflows/rhub.yaml)
[![lifecycle](https://lifecycle.r-lib.org/articles/figures/lifecycle-experimental.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![Project Status:
WIP](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip)
[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/dataset)](https://cran.r-project.org/package=dataset)
[![CRAN_time_from_release](https://www.r-pkg.org/badges/ago/dataset)](https://cran.r-project.org/package=dataset)
[![Status at rOpenSci Software Peer
Review](https://badges.ropensci.org/553_status.svg)](https://github.com/ropensci/software-review/issues/553)
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.14537352.svg)](https://zenodo.org/record/6950435#.YukDAXZBzIU)\]
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

## Semantically richer data frames

``` r
library(dataset)
iris_ds <- dataset_df(
  x = iris,
  dataset_bibentry = dublincore(
    title = "Iris Dataset",
    creator = person("Edgar", "Anderson", role = "aut"),
    publisher = "American Iris Society",
    datasource = "https://doi.org/10.1111/j.1469-1809.1936.tb02137.x",
    date = 1935,
    language = "en",
    description = "This famous (Fisher's or Anderson's) iris data set."
  )
)
```

It is mandatory to add a `title`, `author` to a dataset, and if the
`date` is not specified, the current date will be added.

As the `dataset_df` at this point is just created, if it is not
published yet, the `identifer` receives the default `:tba` value, a
`version` of 0.1.0 and the `:unas` (unassigned) `publisher` field.

The `dataset_df` behaves as expected from a data.frame-like object. See
more information about the enhanced semantic capabilities of these data
frames in the vignette article [Create Datasets that are Easy to Share
Exchange and
Extend](https://dataset.dataobservatory.eu/articles/dataset_df.html)

``` r
summary(iris_ds)
#> Anderson E (1935). "Iris Dataset."
#>    x.Sepal.Length        x.Sepal.Width       x.Petal.Length        x.Petal.Width          x.Species     
#>  Min.   :4.300000     Min.   :2.000000     Min.   :1.000        Min.   :0.1000000    setosa    :50      
#>  1st Qu.:5.100000     1st Qu.:2.800000     1st Qu.:1.600        1st Qu.:0.3000000    versicolor:50      
#>  Median :5.800000     Median :3.000000     Median :4.350        Median :1.3000000    virginica :50      
#>  Mean   :5.843333     Mean   :3.057333     Mean   :3.758        Mean   :1.1993333    NA                 
#>  3rd Qu.:6.400000     3rd Qu.:3.300000     3rd Qu.:5.100        3rd Qu.:1.8000000    NA                 
#>  Max.   :7.900000     Max.   :4.400000     Max.   :6.900        Max.   :2.5000000    NA
```

The dataset_df A brief description of the extended metadata attributes:

``` r
print(get_bibentry(iris_ds), "Bibtex")
#> @Misc{,
#>   title = {Iris Dataset},
#>   author = {Edgar Anderson},
#>   identifier = {:tba},
#>   publisher = {American Iris Society},
#>   year = {1935},
#>   language = {en},
#>   relation = {:unas},
#>   format = {:unas},
#>   rights = {:tba},
#>   description = {This famous (Fisher's or Anderson's) iris data set.},
#>   type = {DCMITYPE:Dataset},
#>   datasource = {https://doi.org/10.1111/j.1469-1809.1936.tb02137.x},
#>   coverage = {:unas},
#> }
```

``` r
paste0("Publisher:", publisher(iris_ds))
#> [1] "Publisher:American Iris Society"
paste0("Rights:", rights(iris_ds))
#> [1] "Rights::tba"
```

The descriptive metadata are added to a `utils::bibentry` object which
has many printing options (see `?bibentry`). (The `utils` package is
installed by default with every R system, so working with utils is not
an extra dependency.)

## Semantically richer data frame columns

It is important to see that we do not only increase the semantics of the
dataset as a whole, but also the semantics of each variable. R users
often have a problem with the reusability of their data frames because,
by default, a variable is only described by a programmatically usable
name label; for example, in the famous `iris` dataset, the length of the
sepal for each observation (row) is in the `iris$Sepal.Length` column.
If we would like to add rows to this dataset, it is essential to know if
the numbers in the `iris$Sepal.Length` are measured in millimetres
centimetres or inches.

When working with datasets that receive their components from different
linked open data sources, it is particularly important to have a more
precise semantic definition and description of each variable.

``` r
gdp_1 = defined(
    c(3897, 7365), 
    label = "Gross Domestic Product", 
    unit = "million dollars", 
    definition = "http://data.europa.eu/83i/aa/GDP")

# Summarise this semantically better defined vector:
summary(gdp_1)
#> Gross Domestic Product (million dollars)
#>    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#>    3897    4764    5631    5631    6498    7365

# See its attributes under the hood:
attributes(gdp_1)
#> $label
#> [1] "Gross Domestic Product"
#> 
#> $class
#> [1] "haven_labelled_defined" "haven_labelled"         "vctrs_vctr"            
#> [4] "double"                
#> 
#> $unit
#> [1] "million dollars"
#> 
#> $definition
#> [1] "http://data.europa.eu/83i/aa/GDP"
```

The *dataset* package contains a semantically enriched version of the
`iris` dataset (which is installed with every R system.)

``` r
data("iris_dataset")

# Print the dataset_df object:
print(iris_dataset)
#> Anderson E (1935). "Iris Dataset."
#>    rowid      Sepal.Length Petal.Length Sepal.Width Petal.Width Species   
#>    <hvn_lbl_> <hvn_lbl_>   <hvn_lbl_>   <hvn_lbl_>  <hvn_lbl_>  <hvn_lbl_>
#>  1 #1         5.1          1.4          3.5         0.2         1 [setosa]
#>  2 #2         4.9          1.4          3           0.2         1 [setosa]
#>  3 #3         4.7          1.3          3.2         0.2         1 [setosa]
#>  4 #4         4.6          1.5          3.1         0.2         1 [setosa]
#>  5 #5         5            1.4          3.6         0.2         1 [setosa]
#>  6 #6         5.4          1.7          3.9         0.4         1 [setosa]
#>  7 #7         4.6          1.4          3.4         0.3         1 [setosa]
#>  8 #8         5            1.5          3.4         0.2         1 [setosa]
#>  9 #9         4.4          1.4          2.9         0.2         1 [setosa]
#> 10 #10        4.9          1.5          3.1         0.1         1 [setosa]
#> # ℹ 140 more rows

# Summarise the Sepal.Length variable:
summary(iris_dataset$Sepal.Length)
#> Length of the sepal in cm (centimeter)
#>    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#>   4.300   5.100   5.800   5.843   6.400   7.900

# Check the attributes of this variable:
attributes(iris_dataset$Sepal.Length)
#> $label
#> [1] "Length of the sepal in cm"
#> 
#> $class
#> [1] "haven_labelled_defined" "haven_labelled"         "vctrs_vctr"            
#> [4] "double"                
#> 
#> $unit
#> [1] "centimeter"
#> 
#> $definition
#> [1] "https://www.wikidata.org/wiki/Property:P2043"
```

## Dataset Provenance

The constructor of the `dataset_df` objects also records the most
important processes that created or modified the dataset. This
experimental feature has not been fully developed in the current
*dataset* version. The aim is to provide a standard way of describing
the processes that help to understand what happened with your data using
the W3C [PROV-O](https://www.w3.org/TR/prov-o/) provenance ontology and
the [RDF 1.1 N-Triples](https://www.w3.org/TR/n-triples/) W3C standard
for describing these processes in a flat file.

``` r
provenance(iris_dataset)
#> [1] "<https://doi.org/10.5281/zenodo.10396807> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://purl.org/linked-data/cube#DataSet> ."
#> [2] "<https://orcid.org/0000-0001-7513-6760> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://www.w3.org/ns/prov#Agent> ."           
#> [3] "<https://doi.org/10.5281/zenodo.6703764.> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://www.w3.org/ns/prov#SoftwareAgent> ."
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
