#' @title Add DataCite metadata to an object
#'
#' @description Add metadata conforming the \href{https://schema.datacite.org/}{DataCite Metadata Schema}
#' to datasets, i.e. structured R data.frame or list objects, for an accurate and consistent identification
#' of a resource for citation and retrieval purposes.
#'
#' @details DataCite is a leading global non-profit organisation that provides persistent identifiers
#' (DOIs) for research data and other research outputs. Organizations within the research
#' community join DataCite as members to be able to assign DOIs to all their research
#' outputs. This way, their outputs become discoverable and associated metadata is made
#' available to the community.\cr
#' DataCite then develops additional services to improve the DOI management experience,
#' making it easier for our members to connect and share their DOIs with the broader
#' research ecosystem and to assess the use of their DOIs within that ecosystem.
#' DataCite is an active participant in the research community and promotes data sharing
#' and citation through community-building efforts and outreach activities.
#' @param x An R object of type data.frame, or inherited data.table, tibble; alternatively a well
#' structured R list.
#' @details The \code{ResourceType} property will be by definition "Dataset".
#' The \code{Size} attribute (e.g. bytes, pages, inches, etc.) will automatically added to the dataset.
#' @param Title The name(s) or title(s) by which a resource is known. May be the title of a dataset
#' or the name of a piece of software. Similar to \href{http://purl.org/dc/elements/1.1/title}{dct:title}.\cr
#' See \code{\link{dataset_title}} for adding further titles.
#' @param titleType For a single \code{Title} defaults to \code{NULL}. Otherwise you can add
#' a Subtitle, an Alternative Title and an Other Title. See \code{\link{dataset_title}}.
#' @param Creator The main researchers involved in producing the data, or the authors of the publication, in
#' priority order. To supply multiple creators, repeat this property.
#' @param Identifier The Identifier is a unique string that identifies a resource. For software, determine
#' whether the identifier is for a specific version of a piece of software, (per the
#' \href{https://force11.org/info/software-citation-principles-published-2016/}{Force11 Software Citation Principles},
#' or for all versions. Similar to \code{dct:title} in \code{\link{dublincore}}.
#' @param Publisher The name of the entity that holds, archives, publishes prints, distributes,
#' releases, issues, or produces the resource. This property will be used to formulate the
#' citation, so consider the prominence of the role. For software, use Publisher for the
#' code repository. If there is an entity other than a code repository, that "holds, archives,
#' publishes, prints, distributes, releases, issues, or produces" the code, use the property
#' Contributor/contributorType/ hostingInstitution for the code repository. Corresponds to dct:Publisher in
#' \code{\link{dublincore}}.
#' @param PublicationYear The year when the data was or will be made publicly available in \code{YYYY} format.
#' @param Subject Recommended for discovery. Subject, keyword, classification code, or key
#' phrase describing the resource. Similar to \href{http://purl.org/dc/elements/1.1/subject}{dct:subject}. \cr
#' Use \code{\link{subject_add}} to properly add a key phrase from a controlled vocabulary.
#' @param Contributor Recommended for discovery. The institution or person responsible for collecting, managing, distributing, or otherwise contributing to the development of the resource.
#' @param Date Recommended for discovery in DataCite. Similar to \href{http://purl.org/dc/elements/1.1/date}{dct:date} in
#' \code{\link{dublincore}}.
#' @param Publisher The name of the entity that holds, archives, publishes prints,
#' distributes, releases, issues, or produces the resource. This property will be used to
#' formulate the citation, so consider the prominence of the role.
#' For software, use Publisher for the code repository. Mandatory in DataCite, and similar to
#' dct:publisher
#' @param Language The primary language of the resource. Allowed values are taken from
#' IETF BCP 47, ISO 639-1 language code. See \code{\link{language_add}}.
#' @param AlternateIdentifier An identifier or identifiers other than the primary Identifier applied to the resource being registered. This may be any alphanumeric string which is unique within its domain of issue. May be used for local identifiers. AlternateIdentifier should be used for another identifier of the same instance (same location, same file).
#' @param RelatedIdentifier Recommended for discovery. Similar to \href{http://purl.org/dc/elements/1.1/relation}{dct:relation}.
#' @param Format Technical format of the resource. Similar to \href{http://purl.org/dc/elements/1.1/format}{dct:format}.
#' @param Version Free text. Suggested practice: track major_version.minor_version. See \code{\link{version_add}}.
#' @param Rights  Any rights information for this resource. The property may be repeated to record complex rights characteristics.
#' Free text.
#' @param Description Recommended for discovery. All additional information that does not
#' fit in any of the other categories. May be used for technical information. A free text.
#' Similar to \href{http://purl.org/dc/elements/1.1/description}{dct:description}.
#' @param Geolocation Recommended for discovery. Spatial region or named place where the data was gathered or about which the data is focused.
#' @param FundingReference Information about financial support (funding) for the resource
#' being registered.
#' @param overwrite If pre-existing metadata properties should be overwritten,
#' defaults to \code{TRUE}.
#' @return An R object with at least the mandatory DataCite attributes.
#' @importFrom utils person
#' @source \href{https://support.datacite.org/docs/schema-mandatory-properties-v43}{DataCite 4.3 Mandatory Properties} and
#' \href{https://support.datacite.org/docs/schema-optional-properties-v43}{DataCite 4.3 Optional Properties}
#' @family metadata functions
#' @export
#' @examples
#' my_iris <- datacite_add(
#'    x = iris,
#'    Title = "Iris Dataset",
#'    Creator = person(family = "Anderson", given = "Edgar", role = "aut"),
#'    Publisher= "American Iris Society",
#'    PublicationYear = 1935,
#'    Geolocation = "US",
#'    Language = "en")
#'
#' datacite(my_iris)

datacite <- function(x) {

  attributes_measurements <- attributes(x)
  attributes_measurements$row.names <- NULL
  attributes_measurements$class <- NULL

  attributes_measurements
}

#' @rdname datacite
#' @export
datacite_add <- function(x,
                         Title,
                         titleType = NULL,
                         Creator,
                         Identifier = NULL,
                         Publisher = NULL,
                         PublicationYear = "THIS",
                         Subject = NULL,
                         Contributor = NULL, Date = NULL,
                         Language = NULL,
                         AlternateIdentifier = NULL, RelatedIdentifier = NULL,
                         Format = NULL, Version = NULL, Rights = NULL,
                         Description = NULL, Geolocation = NULL,
                         FundingReference = NULL,
                         overwrite = TRUE) {

  if (PublicationYear == "THIS") as.integer(substr(Sys.Date(),1,4))

  x <- title_add (x, Title = Title, titleType = titleType)

  attr(x, "Subject") <- Subject
  attr(x, "Creator") <- Creator
  attr(x, "Publisher") <- Publisher

  #x <- identifier_add(x, Identifer = Identifier, overwrite = overwrite)
  attr(x, "Identifer") <- Identifier

  attr(x, "Issued") <- ifelse(is.null(Date), PublicationYear, Date)
  attr(x, "PublicationYear") <- PublicationYear
  attr(x, "ResourceType") <- ifelse(inherits(x, "data.frame"), "Dataset",
                                     ifelse (inherits(x, "list"), "Dataset",
                                             "Other R object"))

  attr(x, "Description") <- Description
  attr(x, "Geolocation") <- Geolocation

  if (!is.null(Language)) x <- language_add (x, Language)
  if (!is.null(Version))  x <- version_add (x, Version)

  x <- size_add(x)
  x
}

#' @title Add Size metadata to an object
#' @description Add the optional DataCite \code{Size} property as an attribute to an R object.
#' @details \code{Size} is an optional property in
#' \href{https://support.datacite.org/docs/schema-optional-properties-v43#13-size}{DataCite 4.3}.
#' The object size is estimated with \code{[utils]{object.size}}.
#' @param x An R object, such as a data.frame, a tibble, or a character vector.
#' @return The estimated object size in memory is added as an attribute to \code{x} in SI
#' kB and IEC KiB (legacy Kb) units,
#' rounded to two decimals.
#' @family metadata functions
#' @importFrom utils object.size
#' @examples
#' iris_dataset <- size_add(iris)
#' attr(iris_dataset, "Size")
#' @export

size_add <- function(x) {

  a <- object.size(x)

  this_object_size <- paste0(round((as.numeric(a) / 1000),2), " kB [", round((as.numeric(a)/1024),2), " KiB]")

  attr(x, "Size") <- this_object_size

  x
}
