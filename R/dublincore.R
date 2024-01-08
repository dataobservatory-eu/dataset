#' @title Add or get Dublin Core metadata
#' @description Add metadata conforming the
#' \href{https://www.dublincore.org/specifications/dublin-core/dcmi-terms/terms/format/}{DCMI Metadata Terms}.
#' to datasets, i.e. structured R data.frame or list objects, for an accurate and consistent identification
#' of a resource for citation and retrieval purposes.
#' @details The Dublin Core, also known as the Dublin Core Metadata Element Set
#' (DCMES), is a set of fifteen main metadata items for describing digital
#' or physical resources, such as datasets or their printed versions.
#' Dublin Core has been formally standardized internationally as ISO 15836,
#' as IETF RFC 5013 by the Internet Engineering Task Force (IETF),
#' as well as in the U.S. as ANSI/NISO Z39.85.
#' @param x An R object of type data.frame, or inherited data.table, tibble; alternatively a well
#' structured R list.
#' @details The \code{ResourceType} property will be by definition "Dataset".
#' The \code{Size} attribute (e.g. bytes, pages, inches, etc.) will automatically added to the dataset.
#' @param title \href{https://www.dublincore.org/specifications/dublin-core/dcmi-terms/elements11/title/}{dct:title}, a name given to the resource.
#' \code{\link{datacite}} allows the use of alternate titles, too. See \code{\link{dataset_title}}.
#' @param creator An entity primarily responsible for making the resource.
#' \href{https://www.dublincore.org/specifications/dublin-core/dcmi-terms/elements11/creator/}{dct:creator}
#' Corresponds to \code{Creator} in \code{\link{datacite}}. See \code{\link{creator}}.
#' @param identifier An unambiguous reference to the resource within a given context.
#' Recommended practice is to identify the resource by means of a string conforming to an
#' identification system. Examples include International Standard Book Number (ISBN),
#' Digital Object Identifier (DOI), and Uniform Resource Name (URN).
#' Select and identifier scheme from
#' \href{https://www.ukoln.ac.uk/metadata/dcmi-ieee/identifiers/index.html}{registered URI schemes maintained by IANA}.
#' More details: \href{https://www.ukoln.ac.uk/metadata/dcmi-ieee/identifiers/}{Guidelines for using resource identifiers in Dublin Core metadata and IEEE LOM}.
#' Similar to \code{Identifier} in \code{\link{datacite}}. See \code{\link{identifier}}.
#' @param publisher Corresponds to \href{https://www.dublincore.org/specifications/dublin-core/dcmi-terms/#publisher}{dct:publisher}
#' and Publisher in DataCite.
#' The name of the entity that holds, archives, publishes prints, distributes, releases,
#' issues, or produces the resource. This property will be used to formulate the citation,
#' so consider the prominence of the role. For software, use \code{Publisher} for the
#' code repository. If there is an entity other than a code repository, that
#' "holds, archives, publishes, prints, distributes, releases, issues, or produces" the
#' code, use the property Contributor/contributorType/hostingInstitution for the code
#' repository. See \code{\link{publisher}}.
#' @param subject In \href{https://www.dublincore.org/specifications/dublin-core/dcmi-terms/elements11/subject/}{dct:subject}. In
#' \code{\link{datacite}} it is a recommended property for discovery. In DataCite, a more complex
#' referencing is used. See \code{\link{subject}} and create structured Subject objects with
#' \code{\link{subject_create}}.
#' @param date Corresponds to a point or period of time associated with an event in the
#' lifecycle of the resource. \href{https://www.dublincore.org/specifications/dublin-core/dcmi-terms/elements11/date/}{dct:date}.
#' \code{Date} is also recommended for
#' discovery in \code{\link{datacite}}, but it requires a different formatting.
#' @param language The primary language of the resource. Allowed values are taken from
#' IETF BCP 47, ISO 639-1 language code. See \code{\link{language}}. Corresponds to Language in Datacite.
#' @param format The file format, physical medium, or dimensions of the resource.
#' \href{https://www.dublincore.org/specifications/dublin-core/dcmi-terms/elements11/format/}{dct:format}
#' Examples of dimensions include size and duration. Recommended best practice is to use a controlled
#' vocabulary such as the list of \href{https://www.iana.org/assignments/media-types/media-types.xhtml}{Internet Media Types, formerly known as MIME}. It is similar to \code{Format} in
#' \code{\link{datacite}}.
#' @param rights Corresponds to \href{https://www.dublincore.org/specifications/dublin-core/dcmi-terms/elements11/rights}{dct:rights} and
#' \code{\link{datacite}} Rights. Information about rights held in and over the resource.
#' Typically, rights information includes a statement about various property rights associated with the resource,
#' including intellectual property rights. See \code{\link{rights}}.
#' @param description An account of the resource. It may include but is not limited to:
#' an abstract, a table of contents, a graphical representation, or a free-text account of the resource.
#' \href{https://www.dublincore.org/specifications/dublin-core/dcmi-terms/elements11/description/}{dct:description}. In
#' \code{\link{datacite}} it is recommended for discovery. See \code{\link{description}}.
#' @param relation A related resource. Recommended best practice is to identify the related
#' resource by means of a string conforming to a formal identification system.
#' See: \href{https://www.dublincore.org/specifications/dublin-core/dcmi-terms/elements11/relation/}{dct:relation}.
#' Similar to \code{RelatedItem} in \code{\link{datacite}}, which is recommended for discovery.
#' @param type The nature or genre of the resource. Recommended best practice is to use a controlled vocabulary such as the DCMI Type Vocabulary
#' \href{https://www.dublincore.org/specifications/dublin-core/dcmi-type-vocabulary/}{DCMITYPE}.
#' For a dataset, the correct term is \code{Dataset}.
#' To describe the file format, physical medium, or dimensions of the resource, use the
#' Format element.
#' @param subject Defaults to \code{NULL}. See \code{\link{subject}} to add subject descriptions
#' to your dataset.
#' @param datasource The source of the dataset,
#' \href{https://www.dublincore.org/specifications/dublin-core/dcmi-terms/elements11/source/}{DCMI: Source},
#' which corresponds to a \code{relatedItem} in the DataCite vocabulary. We use
#' \code{datasource} instead of \code{source} to avoid naming conflicts with the
#' @param format The file format, physical medium, or dimensions of the dataset. See
#' \href{https://www.dublincore.org/specifications/dublin-core/dcmi-terms/elements11/source/}{DCMI: Format}.
#' @param coverage The spatial or temporal topic of the resource, spatial
#' applicability of the dataset, or jurisdiction under which the dataset
#' is relevant. See
#' \href{https://www.dublincore.org/specifications/dublin-core/dcmi-terms/elements11/coverage/}{DCMI: Coverage}.
#' @param contributor An entity responsible for making contributions to the dataset. See
#' \href{https://www.dublincore.org/specifications/dublin-core/dcmi-terms/elements11/contributor/}{DCMI: Contributor}.
#' @param language A language of the dataset. See
#' \href{https://www.dublincore.org/specifications/dublin-core/dcmi-terms/elements11/language/}{DCMI: Language}.
#' @importFrom utils person bibentry
#' @source \href{https://www.dublincore.org/specifications/dublin-core/dcmi-terms/terms/format/}{	DCMI Metadata Terms}.
#' @family metadata functions
#' @return The Dublin Core Metadata elements of the dataset in a \code{utils::\link[utils]{bibentry}} object.
#' @export
#' @examples
#' dublincore(
#'   title = "Iris Dataset",
#'   creator = person("Edgar", "Anderson", role = "aut"),
#'   publisher = "American Iris Society",
#'   datasource = "https://doi.org/10.1111/j.1469-1809.1936.tb02137.x",
#'   date = 1935,
#'   language = "en",
#'   description = "This famous (Fisher's or Anderson's) iris data set gives the
#'   measurements in centimeters of the variables sepal length and width and petal length
#'   and width, respectively, for 50 flowers from each of 3 species of iris.
#'   The species are Iris setosa, versicolor, and virginica."
#' )

dublincore <- function(
    title,
    creator,
    identifier = NULL,
    publisher = NULL,
    subject = NULL,
    type = "DCMITYPE:Dataset",
    contributor = NULL,
    date = NULL,
    language = NULL,
    relation = NULL,
    format = "application/r-rds",
    rights = NULL,
    datasource = NULL,
    description = NULL,
    coverage = NULL) {

  date <- ifelse (is.null(date), ":tba", as.character(date))
  identifier <- ifelse (is.null(identifier), ":tba", as.character(identifier))
  format <- ifelse (is.null(format), ":tba", as.character(format))
  relation <- ifelse (is.null(relation), ":unas", relation)
  format <- ifelse (is.null(relation), ":unas", relation)
  rights <- ifelse (is.null(rights), ":tba", as.character(rights))
  coverage <- ifelse (is.null(coverage), ":unas", as.character(coverage))
  datasource <- ifelse (is.null(datasource), ":unas", as.character(datasource))

  new_dublincore(title = title,
                 creator = creator,
                 identifier = identifier,
                 publisher = publisher,
                 subject = subject,
                 type = type,
                 contributor = contributor,
                 date = date,
                 language = language,
                 relation = relation,
                 format = format,
                 rights = rights,
                 datasource = datasource,
                 description = description,
                 coverage = coverage)
}

#' @rdname dublincore
#' @param ... Optional parameters to add to a \code{dublincore} object.
#' \code{author=person("Jane", "Doe")} adds an author to the citation
#' object if \code{type="dataset"}.
#' @export
as_dublincore <- function(x, type = "bibentry", ...) {

  citation_author <- person(NULL, NULL)

  is_person <- function(p) ifelse (inherits(p, "person"), TRUE, FALSE)

  arguments <- list(...)

  if (!is.null(arguments$author)) {
    if ( is_person(arguments$author))  {
      citation_author <- arguments$author
    } else {
      stop("as_dublincore(x, ..., author = ): author must be created with utils::person().")
    }
  }

  if (! type %in% c("bibentry", "list", "dataset")) {
    warning_message <- "as_dublincore(ds, type=...) type cannot be "
    warning(warning_message, type, ". Reverting to 'bibentry'.")
    type <- 'bibentry'
  }

  ds_bibentry <- dataset_bibentry(x)
  dataset_title   <- ds_bibentry$title
  dataset_creator <- ds_bibentry$author

  if (! is_person(dataset_creator)) {
    stop('attr(x, "DataBibentry")$author is not a person object.')
  }

  if (!is.null(ds_bibentry$year)) {
    if(!is.null(ds_bibentry$year)) {
      dataset_date <- ":tba"
    } else {
      dataset_date <- as.character(ds_bibentry$year)
    }
  }

  dataset_relation <- ifelse (is.null(ds_bibentry$relation), ":unas", as.character(ds_bibentry$relation))
  dataset_identifier <- ifelse (is.null(ds_bibentry$identifier), ":tba", as.character(ds_bibentry$identifier))
  dataset_version <- ifelse (is.null(ds_bibentry$version), ":unas", as.character(ds_bibentry$version))
  dataset_description <- ifelse (is.null(ds_bibentry$description), ":unas", as.character(ds_bibentry$description))
  dataset_language <- ifelse (is.null(ds_bibentry$language), ":unas", as.character(ds_bibentry$language))
  dataset_format <- ifelse (is.null(ds_bibentry$format), ":tba", as.character(ds_bibentry$format))
  dataset_rights <- ifelse (is.null(ds_bibentry$rights), ":tba", as.character(ds_bibentry$rights))
  dataset_coverage  <- ifelse (is.null(ds_bibentry$coverage), ":unas", as.character(ds_bibentry$coverage))
  datasource <- ifelse (is.null(ds_bibentry$datasource), ":unas", as.character(ds_bibentry$datasource))
  dataset_contributor <- ifelse (is.null(ds_bibentry$contributor), "", as.character(ds_bibentry$contributor))
  dataset_subject <- ifelse (is.null(ds_bibentry$subject), "", as.character(ds_bibentry$subject))
  dataset_publisher <- ifelse (is.null(ds_bibentry$publisher), "", as.character(ds_bibentry$publisher))

  if (type == "bibentry") {
    new_dublincore(title = dataset_title,
                   creator = dataset_creator,
                   identifier = dataset_identifier,
                   publisher = dataset_publisher,
                   subject = dataset_subject,
                   type = "DCMITYPE:Dataset",
                   contributor = dataset_contributor,
                   date = dataset_date,
                   language = dataset_language,
                   relation = dataset_relation,
                   format = dataset_format,
                   rights = dataset_rights,
                   datasource = datasource,
                   description = dataset_description,
                   coverage = dataset_coverage)
  } else if (type== "list") {
    if (dataset_contributor == "") dataset_contributor <- NULL
    if (dataset_subject == "") dataset_subject <- NULL

    list(title=dataset_title,
         creator=dataset_creator,
         identifier = dataset_identifier,
         publisher = dataset_publisher,
         subject = dataset_subject,
         type = "DCMITYPE:Dataset",
         contributor = dataset_contributor,
         date = date,
         language = dataset_language,
         relation = dataset_relation,
         format = dataset_format,
         rights = dataset_rights,
         datasource = datasource,
         description = dataset_description,
         coverage = dataset_coverage)
  } else if ( type  == "dataset") {

    properties <- c(length(dataset_title),
                    length(as.character(dataset_creator)),
                    length(dataset_identifier),
                    length(dataset_publisher),
                    length(dataset_subject),
                    length("DCMITYPE:Dataset"),
                    length(dataset_contributor),
                    length(dataset_date),
                    length(dataset_language),
                    length(dataset_relation),
                    length(dataset_format),
                    length(dataset_rights),
                    length(datasource),
                    length(dataset_description),
                    length(dataset_coverage)
                    )
    assertthat::assert_that(
      all(properties)==1, msg= "In as_dublincore() not all properties have a length 1 to export into datataset (data.frame)."
    )

    dataset (
      data.frame(title = dataset_title,
                 creator = as.character(dataset_creator),
                 identifier = dataset_identifier,
                 publisher = dataset_publisher,
                 subject = dataset_subject,
                 type = "DCMITYPE:Dataset",
                 contributor = dataset_contributor,
                 date = dataset_date,
                 language = dataset_language,
                 relation = dataset_relation,
                 format = dataset_format,
                 rights = dataset_rights,
                 datasource = datasource,
                 description = dataset_description,
                 coverage = dataset_coverage),
      title = paste0("The Dublin Core Metadata of `", ds_bibentry$title, "'"),
      author = citation_author,
      year = substr(as.character(Sys.Date()),1,4)
    )
  }
}

#' @keywords internal
new_dublincore <- function (title,
                            creator,
                            identifier = NULL,
                            publisher = NULL,
                            subject = NULL,
                            type = "DCMITYPE:Dataset",
                            contributor = NULL,
                            date = NULL,
                            language = NULL,
                            relation = NULL,
                            format = NULL,
                            rights = NULL,
                            datasource = NULL,
                            description = NULL,
                            coverage = NULL) {

  dublincore_object <- bibentry(bibtype = "Misc",
                                title = title,
                                author = creator,
                                identifier = identifier,
                                publisher = publisher,
                                contributor = contributor,
                                year = as.character(substr(date, 1,4)),
                                language = language,
                                relation = relation,
                                format = format,
                                rights = rights,
                                description = description,
                                type = type,
                                datasource = datasource,
                                coverage = coverage)

  class(dublincore_object) <- c("dublincore", class(dublincore_object))
  dublincore_object
}


#' @rdname dublincore
is.dublincore <- function(x) {
  UseMethod("is.dublincore", x)
}

#' @rdname dublincore
#' @param x An object that is tested if it has a class "dublincore".
#' @exportS3Method
is.dublincore.dublincore <- function(x) inherits(x, "dublincore")
