#' @title Add DublinCore metadata to an object
#'
#' @description Add metadata conforming the \href{https://schema.datacite.org/}{DataCite Metadata Schema}
#' to datasets, i.e. structured R data.frame or list objects, for an accurate and consistent identification
#' of a resource for citation and retrieval purposes.
#'
#' @details DataCite is a leading global non-profit organisation that provides persistent identifiers
#' (DOIs) for research data and other research outputs. Organizations within the research
#' community join DataCite as members to be able to assign DOIs to all their research
#' outputs. This way, their outputs become discoverable and associated metadata is made
#' available to the community.
#' DataCite then develops additional services to improve the DOI management experience,
#' making it easier for our members to connect and share their DOIs with the broader
#' research ecosystem and to assess the use of their DOIs within that ecosystem.
#' DataCite is an active participant in the research community and promotes data sharing
#' and citation through community-building efforts and outreach activities.
#' @param x An R object of type data.frame, or inherited data.table, tibble; alternatively a well
#' structured R list.
#' @details The \code{ResourceType} property will be by definition "Dataset".
#' The \code{Size} attribute (e.g. bytes, pages, inches, etc.) will automatically added to the dataset.
#' @param title \href{http://purl.org/dc/elements/1.1/title}{dct:title}, a name given to the resource.
#' \code{\link{datacite}} allows the use of alternate titles, too.
#' @param creator An entity primarily responsible for making the resource. \href{http://purl.org/dc/elements/1.1/creator}{dct:creator}
#' Corresponds to \code{Creator} in \code{\link{datacite}}.
#' @param pulisher Corresponds to dct:publisher and Publisher in DataCite.
#' The name of the entity that holds, archives, publishes prints, distributes, releases,
#' issues, or produces the resource. This property will be used to formulate the citation,
#' so consider the prominence of the role. For software, use Publisher for the
#' code repository. If there is an entity other than a code repository, that
#' "holds, archives, publishes, prints, distributes, releases, issues, or produces" the
#' code, use the property Contributor/contributorType/ hostingInstitution for the code
#' repository.
#' @param subject In \href{http://purl.org/dc/elements/1.1/subject}{dct:subject}. In
#' \code{\link{datacite}} it is a recommended property for discovery.
#' @param date Corresponds to a point or period of time associated with an event in the
#' lifecycle of the resource. \href{http://purl.org/dc/elements/1.1/date}{dct:date}.
#' \code{Date} is also recommended for
#' discovery in \code{\link{datacite}}.
#' @param source A related resource from which the described resource is derived. See \href{http://purl.org/dc/elements/1.1/source}{dct:source}.
#' @param language The primary language of the resource. Allowed values are taken from
#' IETF BCP 47, ISO 639-1 language code. See \code{\link{language_add}}. Corresponds to Language in Datacite.
#' @param format The file format, physical medium, or dimensions of the resource.
#' \href{	http://purl.org/dc/elements/1.1/format}{dct:format}
#' Examples of dimensions include size and duration. Recommended best practice is to use a controlled
#' vocabulary such as the list of Internet Media Types [MIME]. It is similar to \code{Format} in
#' \code{\link{datacite}}.
#' @param rights  Corresponds to \href{http://purl.org/dc/elements/1.1/rights}{dct:rights} and
#' \code{\link{datacite}} Rights. Information about rights held in and over the resource.
#' Typically, rights information includes a statement about various property rights associated with the resource, including intellectual property rights.
#' @param description An account of the resource. It may include but is not limited to:
#' an abstract, a table of contents, a graphical representation, or a free-text account of the resource.
#' \href{http://purl.org/dc/elements/1.1/description}{dct:description}. In
#' \code{\link{datacite}} it is recommended for discovery.
#' @param relation A related resource. Recommended best practice is to identify the related
#' resource by means of a string conforming to a formal identification system. See: \href{http://purl.org/dc/elements/1.1/relation}{dct:relation}.
#' Similar to \code{RelatedItem} in \code{\link{datacite}}, which is recommended for discovery.
#' @param type The nature or genre of the resource. Recommended best practice is to use a controlled vocabulary such as the DCMI Type Vocabulary
#' \href{https://www.dublincore.org/specifications/dublin-core/dcmi-type-vocabulary/}{[DCMITYPE]}.
#' For a dataset, the correct term is \code{Dataset}.
#' To describe the file format, physical medium, or dimensions of the resource, use the
#' Format element.
#' @importFrom utils person
#' @source \href{https://support.datacite.org/docs/schema-mandatory-properties-v43}{DataCite 4.3 Mandatory Properties} and
#' \href{https://support.datacite.org/docs/schema-optional-properties-v43}{DataCite 4.3 Optional Properties}
#' @family metadata functions
#' @export
#' @examples
#' dct_iris <- dublincore_add(
#'                    x = iris,
#'                    title = "Iris Dataset",
#'                    creator = person("Anderson", "Edgar", role = "aut"),
#'                    publisher = "American Iris Society",
#'                    source = "https://doi.org/10.1111/j.1469-1809.1936.tb02137.x",
#'                    date = 1935,
#'                    language = "en"
#'                    )
#'
#' dublincore(dct_iris)

dublincore <- function(x) {

  attributes_dataset <- attributes(x)
  attributes_dataset$row.names <- NULL
  attributes_dataset$class <- NULL
  attributes_dataset$Size <- NULL

  attributes_dataset
}

#' @rdname dublincore
#' @export
dublincore_add <- function(x,
                           title = NULL, creator=NULL,
                           publisher=NULL,
                           subject = NULL,
                           date = NULL,
                           source = NULL,
                           language = NULL,
                           format = NULL,
                           rights = NULL,
                           description = NULL,
                           type = NULL,
                           overwrite = TRUE) {


  if (is.null(attr(x, "title"))) {
    attr(x, "title") <- title
  } else if ( overwrite ) {
    attr(x, "title") <- title
  } else {
    message ("The dataset has already a title: ",  attr(x, "title") )
  }

  if (is.null(attr(x, "creator"))) {
    attr(x, "creator") <- creator
  } else if ( overwrite ) {
    attr(x, "creator") <- creator
  } else {
    message ("The dataset has already a creator: ",  attr(x, "creator") )
  }

  attr(x, "creator") <- creator
  attr(x, "source") <- source

  if (is.null(attr(x, "publisher"))) {
    attr(x, "publisher") <- publisher
  } else if ( overwrite ) {
    attr(x, "publisher") <- publisher
  } else {
    message ("The dataset has already a publisher: ",  attr(x, "publisher") )
  }

  attr(x, "issued") <- date
  attr(x, "type") <- type
  attr(x, "description") <- description

  if (!is.null(language)) x <- language_add (x, language)

  x <- size_add(x)
  x
}


