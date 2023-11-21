#' @title Add DublinCore metadata to a bibentry object
#'
#' @description Add metadata conforming the
#' \href{https://www.dublincore.org/specifications/dublin-core/dcmi-terms/terms/format/}{DCMI Metadata Terms}.
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
#' @param title \href{https://purl.org/dc/elements/1.1/title}{dct:title}, a name given to the resource.
#' \code{\link{datacite}} allows the use of alternate titles, too. See \code{\link{dataset_title}}.
#' @param creator An entity primarily responsible for making the resource. \href{https://purl.org/dc/elements/1.1/creator}{dct:creator}
#' Corresponds to \code{Creator} in \code{\link{datacite}}. See \code{\link{creator}}.
#' @param identifier An unambiguous reference to the resource within a given context.
#' Recommended practice is to identify the resource by means of a string conforming to an
#' identification system. Examples include International Standard Book Number (ISBN),
#' Digital Object Identifier (DOI), and Uniform Resource Name (URN).
#' Select and identifier scheme from
#' \href{https://www.ukoln.ac.uk/metadata/dcmi-ieee/identifiers/index.html}{registered URI schemes maintained by IANA}.
#' More details: \href{https://www.ukoln.ac.uk/metadata/dcmi-ieee/identifiers/}{Guidelines for using resource identifiers in Dublin Core metadata and IEEE LOM}.
#' Similar to \code{Identifier} in \code{\link{datacite}}. See \code{\link{identifier}}.
#' @param oublisher Corresponds to \href{https://www.dublincore.org/specifications/dublin-core/dcmi-terms/#publisher}{dct:publisher}
#' and Publisher in DataCite.
#' The name of the entity that holds, archives, publishes prints, distributes, releases,
#' issues, or produces the resource. This property will be used to formulate the citation,
#' so consider the prominence of the role. For software, use \code{Publisher} for the
#' code repository. If there is an entity other than a code repository, that
#' "holds, archives, publishes, prints, distributes, releases, issues, or produces" the
#' code, use the property Contributor/contributorType/hostingInstitution for the code
#' repository. See \code{\link{publisher}}.
#' @param subject In \href{https://purl.org/dc/elements/1.1/subject}{dct:subject}. In
#' \code{\link{datacite}} it is a recommended property for discovery. In DataCite, a more complex
#' referencing is used. See \code{\link{subject}} and create structured Subject objects with
#' \code{\link{subject_create}}.
#' @param date Corresponds to a point or period of time associated with an event in the
#' lifecycle of the resource. \href{https://purl.org/dc/elements/1.1/date}{dct:date}.
#' \code{Date} is also recommended for
#' discovery in \code{\link{datacite}}, but it requires a different formatting.
#' @param source A related resource from which the described resource is derived.
#' See \href{https://purl.org/dc/elements/1.1/source}{dct:source} and
#' \code{\link{dataset_source}}.
#' @param Language The primary language of the resource. Allowed values are taken from
#' IETF BCP 47, ISO 639-1 language code. See \code{\link{language}}. Corresponds to Language in Datacite.
#' @param Format The file format, physical medium, or dimensions of the resource.
#' \href{	https://purl.org/dc/elements/1.1/format}{dct:format}
#' Examples of dimensions include size and duration. Recommended best practice is to use a controlled
#' vocabulary such as the list of \href{https://www.iana.org/assignments/media-types/media-types.xhtml}{Internet Media Types, formerly known as MIME}. It is similar to \code{Format} in
#' \code{\link{datacite}}.
#' @param rights Corresponds to \href{https://purl.org/dc/elements/1.1/rights}{dct:rights} and
#' \code{\link{datacite}} Rights. Information about rights held in and over the resource.
#' Typically, rights information includes a statement about various property rights associated with the resource,
#' including intellectual property rights. See \code{\link{rights}}.
#' @param description An account of the resource. It may include but is not limited to:
#' an abstract, a table of contents, a graphical representation, or a free-text account of the resource.
#' \href{https://purl.org/dc/elements/1.1/description}{dct:description}. In
#' \code{\link{datacite}} it is recommended for discovery. See \code{\link{description}}.
#' @param Relation A related resource. Recommended best practice is to identify the related
#' resource by means of a string conforming to a formal identification system. See: \href{https://purl.org/dc/elements/1.1/relation}{dct:relation}.
#' Similar to \code{RelatedItem} in \code{\link{datacite}}, which is recommended for discovery.
#' @param type The nature or genre of the resource. Recommended best practice is to use a controlled vocabulary such as the DCMI Type Vocabulary
#' \href{https://www.dublincore.org/specifications/dublin-core/dcmi-type-vocabulary/}{DCMITYPE}.
#' For a dataset, the correct term is \code{Dataset}.
#' To describe the file format, physical medium, or dimensions of the resource, use the
#' Format element.
#' @param subject Defaults to \code{NULL}. See \code{\link{subject}} to add subject descriptions
#' to your dataset.
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
#'   source = "https://doi.org/10.1111/j.1469-1809.1936.tb02137.x",
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
    source = NULL,
    description = NULL,
    coverage = NULL) {

  date <- ifelse (is.null(date), ":tba", as.character(date))
  format <- ifelse (is.null(format), ":tba", as.character(format))
  relation <- ifelse (is.null(relation), ":unas", relation)
  format <- ifelse (is.null(relation), ":unas", relation)
  rights <- ifelse (is.null(rights), ":tba", as.character(rights))
  coverage <- ifelse (is.null(coverage), ":unas", as.character(coverage))
  source <- ifelse (is.null(source), ":unas", as.character(source))

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
                 source = source,
                 description = description,
                 coverage = coverage)
}


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
                            source = NULL,
                            description = NULL,
                            coverage = NULL) {

  dublincore_object <- as_bibentry(title = title,
                                   author = creator,
                                   identifier = identifier,
                                   publisher = publisher,
                                   contributor = contributor,
                                   date = date,
                                   language = language,
                                   relation = relation,
                                   format = format,
                                   rights = rights,
                                   description = description,
                                   type = type,
                                   source = source,
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
