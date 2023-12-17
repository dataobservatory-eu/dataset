# dataset 0.2.8

New vignettes on
- [x] [Richer Semantics for the Dataset's Variables](https://dataset.dataobservatory.eu/articles/columns.html).
- [x] [Richer Semantics for the Dataset's Observations](https://dataset.dataobservatory.eu/articles/rows.html)
- [x] [Richer Description of the Datasets's Structure for Meaningful Reuse Opportunities](https://dataset.dataobservatory.eu/articles/DSD.html)

# dataset 0.2.7 

* Released on CRAN

The devel branch contains new code that is not is validated, but as a whole the package is not working consistently.

# dataset 0.2.6
* All tests are passing, all examples are running.

# dataset 0.2.5 
* `datacite()` has a new interface and an  `as_datacite()` retrieval version. See the `Working with DataCite Metadata` vignette.
* `dublincore()` has a new interface and an  `as_dublincore()` version. See the `Working with Dublin Core Metadata` vignette.

# dataset 0.2.4
All tests are passing but documentation is not rewritten yet.

# dataset 0.2.3 
new subject class for recording subjects

# dataset 0.2.2 
New s3 classes for DataCite and Dublin Core bibliographic entries.

# dataset 0.2.1

A minor correction to avoid vignettes downloading data from the Eurostat data warehouse on CRAN. Small readability improvements in the vignette articles.

# dataset 0.2.0

* New methods for the `dataset()` s3 class: `print.dataset()`, `summary.dataset()`, `subset.dataset`, `[.dataset`, `as.data.frame()`.
* New vignette on how to use the [dataspice](https://github.com/ropensci/dataspice) package programmatically for publishing dataset documentation.
* Released on CRAN.

# dataset 0.1.9

* Incorporating minor changes from the [rOpenSci](https://github.com/ropensci/software-review/issues/553) and CRAN peer-reviews.

# dataset 0.1.7
[![Status at rOpenSci Software Peer Review](https://badges.ropensci.org/553_status.svg)](https://github.com/ropensci/software-review/issues/553)
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.6992467.svg)](https://doi.org/10.5281/zenodo.6992467)

* After reviewing CRAN submission comments, and correcting documentation issues, submitted to [rOpenSci](https://github.com/ropensci/software-review/issues/553) for review before re-submitting to CRAN.

# dataset 0.1.6.0001
* Add `dataset_local_id()` and `dataset_uri()` to the dataset functions.

# dataset 0.1.6.
* A release candidate on CRAN after small documentation improvements.

# dataset 0.1.4.

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.6950435.svg)](https://doi.org/10.5281/zenodo.6950435) Development version available on Zenodo.

* `dataset_export()` is implemented with filetype = 'csv'.
* Replacement functions are added to simple properties `identifier()`, `publisher()`, `publication_year()`, `language()`, `description()`,  `dataset_source()` [to avoid confusion with the base R source() function], `geolocation()`, `rights()`, `version()`.
* Functions to work with structured referential metadata: `dataset_title()`, `dataset_title_create()`, `subject()`, `subject_create()`.
* The Type property is handled by `resource_type()`.

# dataset 0.1.3.

* Vignette articles started to develop and consult the development plan of the project. See  [From dataset To RDF](https://dataset.dataobservatory.eu/articles/RDF.html), [Export and Publish A dataset Object](https://dataset.dataobservatory.eu/articles/publish.html), [Datasets with FAIR metadata](https://dataset.dataobservatory.eu/articles/metadata.html), all [comments](https://github.com/dataobservatory-eu/dataset/issues/) are welcome.
* New functions: `download_dataset()`, `datacite()`, and the `dataset()` constructor.

# dataset 0.1.2.

* The definition of the `dataset()` class, an improved data.frame (tibble, DT) R object with standardized structure and metadata.
* Adding and reading [DublinCore](https://www.dublincore.org/specifications/dublin-core/dcmi-terms/) metadata and [DataCite](https://support.datacite.org/docs/datacite-metadata-schema-44/) mandatory and recommended [FAIR metadata](https://www.go-fair.org/fair-principles/) metadata.

# dataset 0.1.0.

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.6703765.svg)](https://doi.org/10.5281/zenodo.6703765) First development version release.

* Added the [Motivation of the dataset package](https://dataset.dataobservatory.eu/articles/motivation.html) vignette article.
