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
#' available to the community.
#' DataCite then develops additional services to improve the DOI management experience,
#' making it easier for our members to connect and share their DOIs with the broader
#' research ecosystem and to assess the use of their DOIs within that ecosystem.
#' DataCite is an active participant in the research community and promotes data sharing
#' and citation through community-building efforts and outreach activities.
#' @param df An R object of type data.frame, or inherited data.table, tibble; alternatively a well
#' structured R list.
#' @details The \code{ResourceType} property will be by definition "Dataset".
#' The \code{Size} attribute (e.g. bytes, pages, inches, etc.) will automatically added to the dataset.
#' @param Creator The main researchers involved in producing the data, or the authors of the publication, in priority order. To supply multiple creators, repeat this property.
#' @param Pulisher The name of the entity that holds, archives, publishes prints, distributes,
#' releases, issues, or produces the resource. This property will be used to formulate the
#' citation, so consider the prominence of the role. For software, use Publisher for the
#' code repository. If there is an entity other than a code repository, that "holds, archives,
#' publishes, prints, distributes, releases, issues, or produces" the code, use the property
#' Contributor/contributorType/ hostingInstitution for the code repository. Corresponds to dct:Publisher in
#' \code{\link{dublincore}}.
#' @param PublicationYear The year when the data was or will be made publicly available in \code{YYYY} format.
#' @param Subject Recommended for discovery. Subject, keyword, classification code, or key
#' phrase describing the resource. Similar to \href{http://purl.org/dc/elements/1.1/subject}{dct:subject}.
#' @param Contributor Recommended for discover. The institution or person responsible for collecting, managing, distributing, or otherwise contributing to the development of the resource.
#' @param Date Recommended for discovery in DataCite. Similar to \href{http://purl.org/dc/elements/1.1/date}{dct:date} in
#' \code{\link{dublincore}}.
#' @param Language  The primary language of the resource. Allowed values are taken from
#' IETF BCP 47, ISO 639-1 language code. See \code{\link{add_language}}.
#' @param AlternateIdentifier An identifier or identifiers other than the primary Identifier applied to the resource being registered. This may be any alphanumeric string which is unique within its domain of issue. May be used for local identifiers. AlternateIdentifier should be used for another identifier of the same instance (same location, same file).
#' @param RelatedIdentifier Recommended for discovery. Similar to \href{http://purl.org/dc/elements/1.1/relation}{dct:relation}.
#' @param Format Technical format of the resource. Similar to \href{http://purl.org/dc/elements/1.1/format}{dct:format}.
#' @param Version Free text. Suggested practice: track major_version.minor_version. See \code{\link{add_version}}.
#' @param Rights  Any rights information for this resource. The property may be repeated to record complex rights characteristics.
#' Free text.
#' @param Description Recommended for discovery. All additional information that does not
#' fit in any of the other categories. May be used for technical information. A free text.
#' Similar to \href{http://purl.org/dc/elements/1.1/description}{dct:description}.
#' @param Geolocation Recommended for discovery. Spatial region or named place where the data was gathered or about which the data is focused.
#' @param FundingReference Information about financial support (funding) for the resource being registered.
#' @return An R object with at least the mandatory DataCite attributes.
#' @importFrom utils person
#' @source \href{https://support.datacite.org/docs/schema-mandatory-properties-v43}{DataCite 4.3 Mandatory Properties} and
#' \href{https://support.datacite.org/docs/schema-optional-properties-v43}{DataCite 4.3 Optional Properties}
#' @family metadata functions
#' @export
#' @examples
#' datacite_add(
#'    df = iris,
#'    title = "Iris Dataset",
#'    Creator = person("Anderson", "Edgar", role = "aut"),
#'    Publisher= "American Iris Society",
#'    PublicationYear = 1935,
#'    Geolocation = "US",
#'    Language = "en")
#'
#' datacite(df)

datacite <- function(df) {

  attributes_dataset <- attributes(df)
  attributes_dataset$row.names <- NULL
  attributes_dataset$class <- NULL

  attributes_dataset
}

#' @rdname datacite
#' @export
datacite_add <- function(df, Title, Creator, Publisher, PublicationYear = "THIS",
                     Subject = NULL, Contributor = NULL, Date = NULL,
                     Language = NULL,
                     AlternateIdentifer = NULL, RelatedIdentifier = NULL,
                     Format = NULL, Version = NULL, Rights = NULL,
                     Description = NULL, Geolocation = NULL,
                     FundingReference = NULL ) {

  if (PublicationYear == "THIS") as.integer(substr(Sys.Date(),1,4))

  attr(df, "title") <- Title
  attr(df, "creator") <- Creator
  attr(df, "publisher") <- Publisher
  attr(df, "issued") <- ifelse(is.null(Date), PublicationYear, Date)
  attr(df, "PublicationYear") <- PublicationYear
  attr(df, "ResourceType") <- ifelse(inherits(df, "data.frame"), "Dataset",
                                     ifelse (inherits(df, "list"), "Dataset",
                                             "Other R object"))

  attr(df, "Description") <- Description
  attr(df, "Geolocation") <- Geolocation

  if (!is.null(Language)) df <- add_language (df, Language)
  if (!is.null(Version)) df <- add_version (df, Version)

  df <- add_size(df)
  df
}


