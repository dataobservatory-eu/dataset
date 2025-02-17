
<!-- README.md is generated from README.Rmd. Please edit that file -->

# The dataset R Package <a href='https://dataset.dataobservatory.eu/'><img src='man/figures/logo.png' align="right" /></a>

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
[![devel-version](https://img.shields.io/badge/devel%20version-0.3.4006-blue.svg)](https://github.com/dataobservatory-eu/dataset)
[![dataobservatory](https://img.shields.io/badge/ecosystem-dataobservatory.eu-3EA135.svg)](https://dataobservatory.eu/)
[![Codecov test
coverage](https://codecov.io/gh/dataobservatory-eu/dataset/graph/badge.svg)](https://app.codecov.io/gh/dataobservatory-eu/dataset)
<!-- badges: end -->

The aim of the *dataset* package is to make tidy datasets easier to
release, exchange and reuse. It organizes and formats data frame R
objects into well-referenced, well-described, interoperable datasets
into release and reuse ready form.

1.  **Increase FAIR use of your datasets**: Offer a way to better
    utilise the `utils:bibentry` bibliographic entry objects to offer
    more comprehensive and standardised descriptive metadata utilising
    the
    [DCTERMS](https://www.dublincore.org/specifications/dublin-core/dcmi-terms/)
    and
    [DataCite](https://datacite-metadata-schema.readthedocs.io/en/4.6/)
    standards. This will lead to a higher level of findability and
    accessibility, and a better use of the rOpenSci package
    [RefManageR](https://docs.ropensci.org/RefManageR/). See for more
    information the [Bibentry for FAIR
    datasets](https://dataset.dataobservatory.eu/articles/bibentry.html)
    vignette.
2.  **Interoperability outside R**: Extending the `haven_labelled` class
    of the `tidyverse` for consistently labelled categorical variables
    with linked (standard) definitions and units of measures in our
    [defined](https://dataset.dataobservatory.eu/articles/defined.html)
    class; this enables to share metadata not only about the dataset as
    a whole, but about its key components (rows and columns), including
    precise definitions, units of measures. This results in a higher
    level of interoperability and reusability, within and outside of the
    R ecossytem.
3.  **Tidy data tidier, richer**: Offering a new data frame format,
    `dataset_df` that extends tibbles with semantically rich metadata,
    ready to be shared on open data exchange platforms and in data
    repositories. This s3 class is aimed at developers and we are
    working on several packages that provide interoperability with SDMX
    statistical data exchange platforms, Wikidata, or the EU Open Data
    portal. Read more in the [Create Datasets that are Easy to Share
    Exchange and
    Extend](https://dataset.dataobservatory.eu/articles/dataset_df.html)
    vignette.
4.  Adding provenance metadata to make your dataset easier to reuse by
    making its history known to future users. We have no vignette on
    this topic, but you find at the bottom of this `README` an example.
5.  **Releasing and exchanging datasets**: The [From R to
    RDF](https://dataset.dataobservatory.eu/articles/rdf.html) vignette
    shows how to leverage the capabilities of the *dataset* package with
    [rdflib](https://docs.ropensci.org/rdflib/index.html), an
    R-user-friendly wrapper on rOpenSci to work with the *redland*
    Python library for performing common tasks on RDF data, such as
    parsing and converting between formats including rdfxml, turtle,
    nquads, ntriples, creating RDF graphs, and performing SPARQL
    queries.

Putting it all together: the
[Motivation](https://dataset.dataobservatory.eu/articles/Motivation.html)
explains in a long case study why `tidyverse` and the *tidy data
principle* is no longer sufficient for a high level of interoperability
and reusability.

<!---
&#10;The primary aim of dataset is create well-referenced, well-described, interoperable datasets from data.frames, tibbles or data.tables that translate well into the W3C DataSet definition within the [Data Cube Vocabulary](https://www.w3.org/TR/vocab-data-cube/) in a reproducible manner. The data cube model in itself is is originated in the _Statistical Data and Metadata eXchange_, and it is almost fully harmonized with the Resource Description Framework (RDF), the standard model for data interchange on the web^[RDF Data Cube Vocabulary, W3C Recommendation 16 January 2014  <https://www.w3.org/TR/vocab-data-cube/>, Introduction to SDMX data modeling <https://www.unescap.org/sites/default/files/Session_4_SDMX_Data_Modeling_%20Intro_UNSD_WS_National_SDG_10-13Sep2019.pdf>].
&#10;--->

You can install the latest CRAN release with
`install.packages("dataset")`, and the latest development version of
dataset with `remotes::install_github()`:

``` r
install.packages("dataset")
remotes::install_github("dataobservatory-eu/dataset", build = FALSE)
```

The current version of the `dataset` package is in an early,
experimental stage. You can follow the discussion of this package on
[rOpenSci \#553](https://github.com/ropensci/software-review/issues/553)
about the original scope, that included the datacube data model, and the
[rOpenSci \#681](https://github.com/ropensci/software-review/issues/681)
on the new version that moves the data cube data model of SDMX into a
future downstream package. (See, again, the
[Motivation](https://dataset.dataobservatory.eu/articles/Motivation.html)
article.)

Interoperability and future (re)usability depends on the amount and
quality of the metadata that was generated, recorded, and released
together with the data. The `dataset` package aims to collect such
metadata and record them in the least possible intrusive way.

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
    dataset_date = 1935,
    language = "en",
    description = "This famous (Fisher's or Anderson's) iris data set."
  )
)
```

You can read more about the history of this dataset, and some
controversy around its association of problematic science in the
[Bibentry for FAIR
datasets](https://dataset.dataobservatory.eu/articles/bibentry.html)
vignette.

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
#>     rowid          
#>  Length:150        
#>  Class :character  
#>  Mode  :character  
#>                    
#>                    
#>                    
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
#>   year = {1935},
#>   identifier = {:tba},
#>   publisher = {American Iris Society},
#>   contributor = {:unas},
#>   date = {1935},
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
the numbers in the `iris$Sepal.Length` are measured in millimetres,
centimetres or inches.

When working with datasets that receive their components from different
linked open data sources, it is particularly important to have a more
precise semantic definition and description of each variable.

``` r
gdp_1 <- defined(
  c(3897, 7365),
  label = "Gross Domestic Product",
  unit = "million dollars",
  definition = "http://data.europa.eu/83i/aa/GDP"
)

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
#> Anderson E (????). "Iris Dataset."
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
#> [1] "<http://example.com/dataset_prov.nt> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://www.w3.org/ns/prov#Bundle> ."                  
#> [2] "<http://example.com/dataset#> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://www.w3.org/ns/prov#Entity> ."                         
#> [3] "<http://example.com/dataset#> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://purl.org/linked-data/cube#DataSet> ."                 
#> [4] "<http://viaf.org/viaf/6440526> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://www.w3.org/ns/prov#Agent> ."                         
#> [5] "<https://doi.org/10.32614/CRAN.package.dataset> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://www.w3.org/ns/prov#SoftwareAgent> ."
#> [6] "<http://example.com/creation> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://www.w3.org/ns/prov#Activity> ."                       
#> [7] "<http://example.com/creation> <http://www.w3.org/ns/prov#generatedAtTime> \"2025-02-16T08:57:35Z\"^^<xs:dateTime> ."
```

The [From R to
RDF](https://dataset.dataobservatory.eu/articles/rdf.html) vignette
shows how to leverage the capabilities of the *dataset* package with
[rdflib](https://docs.ropensci.org/rdflib/index.html) to share the
history and other metadata of your dataset globally, or import data
updates from standardised statistical data exchanges.

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
