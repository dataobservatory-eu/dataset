#' @title Add DataCite metadata to a bibentry object
#'
#' @description Add metadata conforming the \href{https://schema.datacite.org/}{DataCite Metadata Schema}
#' to a \code{utils::\link[utils]{bibentry}} object, for an accurate and consistent identification
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
#' or the name of a piece of software. Similar to \href{https://purl.org/dc/elements/1.1/title}{dct:title}.\cr
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
#' @param PublicationYear The year when the data was or will be made publicly available in
#' \code{YYYY} format.See \code{\link{publication_year}}.
#' @param Subject Recommended for discovery. Subject, keyword, classification code, or key
#' phrase describing the resource. Similar to \href{https://purl.org/dc/elements/1.1/subject}{dct:subject}. \cr
#' Use \code{\link{subject}} to properly add a key phrase from a controlled vocabulary
#' and create structured Subject objects with \code{\link{subject_create}}.
#' @param Contributor Recommended for discovery. The institution or person responsible for collecting, managing, distributing, or otherwise contributing to the development of the resource.
#' @param Date Recommended for discovery in DataCite. Similar to \href{https://purl.org/dc/elements/1.1/date}{dct:date} in
#' \code{\link{dublincore}}.
#' @param Publisher The name of the entity that holds, archives, publishes prints,
#' distributes, releases, issues, or produces the resource. This property will be used to
#' formulate the citation, so consider the prominence of the role.
#' For software, use Publisher for the code repository. Mandatory in DataCite, and similar to
#' dct:publisher. See \code{\link{publisher}}.
#' @param Type Defaults to \code{Dataset}.
#' The DataCite resourceType definition refers back to
#' \href{https://www.dublincore.org/specifications/dublin-core/dcmi-terms/#https://purl.org/dc/elements/1.1/type}{dcm:type}.
#' The \code{Type$resourceTypeGeneral} is set to  \code{Dataset}, while the user can set a more
#' specific \code{Type$resourceType} value. See \code{\link{resource_type}}.
#' @param Language The primary language of the resource. Allowed values are taken from
#' IETF BCP 47, ISO 639-1 language code. See \code{\link{language}}.
#' @param AlternateIdentifier An identifier or identifiers other than the primary Identifier applied to the resource being registered. This may be any alphanumeric string which is unique within its domain of issue. May be used for local identifiers. AlternateIdentifier should be used for another identifier of the same instance (same location, same file).
#' @param RelatedIdentifier Recommended for discovery. Similar to \href{https://purl.org/dc/elements/1.1/relation}{dct:relation}.
#' @param Format Technical format of the resource. Similar to \href{https://purl.org/dc/elements/1.1/format}{dct:format}.
#' @param Version Free text. Suggested practice: track major_version.minor_version. See \code{\link{version}}.
#' @param Rights Any rights information for this resource. The property may be repeated to record complex rights characteristics.
#' Free text. See \code{\link{rights}}.
#' @param Description Recommended for discovery. All additional information that does not
#' fit in any of the other categories. May be used for technical information. A free text.
#' Similar to \href{https://purl.org/dc/elements/1.1/description}{dct:description}.
#' @param Geolocation Recommended for discovery. Spatial region or named place where the data was gathered
#' or about which the data is focused. See \code{\link{geolocation}}.
#' @param FundingReference Information about financial support (funding) for the resource
#' being registered.
#' @param overwrite If pre-existing metadata properties should be overwritten,
#' defaults to \code{TRUE}.
#' @return A \code{utils::\link[utils]{bibentry}} object DataCite attributes.
#' @importFrom utils person bibentry
#' @source \href{https://support.datacite.org/docs/schema-mandatory-properties-v43}{DataCite 4.3 Mandatory Properties} and
#' \href{https://support.datacite.org/docs/schema-optional-properties-v43}{DataCite 4.3 Optional Properties}
#' @family metadata functions
#' @export
#' @importFrom utils bibentry
#' @examples
#' datacite(
#'    Title = "Iris Dataset",
#'    Creator = person(family = "Anderson", given = "Edgar", role = "aut"),
#'    Publisher = "American Iris Society",
#'    PublicationYear = 1935,
#'    Geolocation = "US",
#'    Language = "en")
#'

datacite <- function(Title,
                     Creator,
                     Identifier = NULL,
                     Publisher = NULL,
                     PublicationYear = NULL,
                     Subject = NULL,
                     Type = "Dataset",
                     Contributor = NULL,
                     DateList = NULL,
                     Language = NULL,
                     AlternateIdentifier = NULL,
                     RelatedIdentifier = NULL,
                     Format = NULL,
                     Version = NULL,
                     Rights = NULL,
                     Description = NULL,
                     Geolocation = NULL,
                     FundingReference = NULL) {

  DateList <- ifelse (is.null(DateList), ":tba", as.character(DateList))
  Format <- ifelse (is.null(Format), ":tba", as.character(Format))
  AlternateIdentifier <- ifelse (is.null(AlternateIdentifier), ":unas", AlternateIdentifier)
  RelatedIdentifier <- ifelse (is.null(RelatedIdentifier), ":unas", RelatedIdentifier)
  Rights <- ifelse (is.null(Rights), ":tba", as.character(Rights))
  Geolocation <- ifelse (is.null(Geolocation), ":unas", as.character(Geolocation))
  FundingReference <- ifelse (is.null(FundingReference), ":unas", as.character(FundingReference))

  new_datacite(Title = Title,
               Creator = Creator,
               Identifier = Identifier,
               Publisher = Publisher,
               PublicationYear = PublicationYear,
               Subject = Subject,
               Type = "Dataset",
               Contributor = Contributor,
               DateList = DateList,
               Language = Language,
               AlternateIdentifier = AlternateIdentifier,
               RelatedIdentifier = RelatedIdentifier,
               Forma = Format,
               Version = Version,
               Rights = Rights,
               Description = Description,
               Geolocation = Geolocation,
               FundingReference = FundingReference)
}


new_datacite <- function (Title,
                          Creator,
                          Identifier,
                          Publisher,
                          PublicationYear,
                          Subject,
                          Type = "Dataset",
                          Contributor,
                          DateList,
                          Language,
                          AlternateIdentifier,
                          RelatedIdentifier,
                          Format, Version, Rights,
                          Description, Geolocation,
                          FundingReference) {

  datacite_object <- bibentry(bibtype = "Misc",
                              title = Title,
                              author = Creator,
                              publisher = Publisher,
                              year = PublicationYear,
                              date = DateList,
                              language = Language,
                              alternateidentifier = AlternateIdentifier,
                              relatedidentifier = RelatedIdentifier,
                              format = Format,
                              version = Version,
                              rights = Rights,
                              description = Description,
                              geolocation = Geolocation,
                              fundingreference = FundingReference)

  class(datacite_object) <- c("datacite", class(datacite_object))
  datacite_object
}


#' @rdname datacite
is.datacite <- function(x) {
  UseMethod("is.datacite", x)
}

#' @rdname datacite
#' @param x An object that is tested if it has a class "datacite".
#' @exportS3Method
is.datacite.datacite <- function(x) inherits(x, "datacite")


