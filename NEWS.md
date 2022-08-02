# dataset 0.1.5.
* A release candidate on CRAN after small documentation improvements.

# dataset 0.1.4.

* [![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.6950435.svg)](https://doi.org/10.5281/zenodo.6950435) Development version available on Zenodo.
* `dataset_export()` is implemented with filetype = 'csv'.
* Replacement functions are added to simple properties `identifier()`, `publisher()`, `publication_year()`, `language()`, `description()`,  `dataset_source()` [to avoid confusion with the base R source() function], `geolocation()`, `rights()`, `version()`.
* Functions to work with structured referencial metadata: `dataset_title()`, `dataset_title_create()`, `subject()`, `subject_create()`.
* The Type property is handled by `resource_type()`.

# dataset 0.1.3.

* Vignette articles started to develop and consult the development plan of the project. See  [From dataset To RDF](https://dataset.dataobservatory.eu/articles/RDF.html), [Export and Publish A dataset Object](https://dataset.dataobservatory.eu/articles/publish.html), [Datasets with FAIR metadata](https://dataset.dataobservatory.eu/articles/metadata.html), all [comments](https://github.com/dataobservatory-eu/dataset/issues/) are welcome.
* New functions: `download_dataset()`, `datacite()`, and the `dataset()` constructor.

# dataset 0.1.2.

* The definition of the [dataset](https://dataset.dataobservatory.eu/articles/datacube.html) class, an improved data.frame (tibble, DT) R object with standardized structure and metadata.
* Adding and reading [DublinCore](https://www.dublincore.org/specifications/dublin-core/dcmi-terms/) metadata and [DataCite](https://support.datacite.org/docs/datacite-metadata-schema-44/) mandatory and recommended [FAIR metadata](https://www.go-fair.org/fair-principles/) metadata.

# dataset 0.1.0.

* [![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.6703765.svg)](https://doi.org/10.5281/zenodo.6703765) First development version release.
* Added the [Motivation of the dataset package](https://dataset.dataobservatory.eu/articles/motivation.html) vignette article.
