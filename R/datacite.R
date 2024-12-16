#' @title Add or get DataCite metadata
#' @description Add or retrieve metadata conforming
#' the \href{https://schema.datacite.org/}{DataCite Metadata Schema}.
#' @details DataCite is a leading global non-profit organisation that provides
#' persistent identifiers (DOIs) for research data and other research outputs.
#' Organisations within the research community join DataCite as members to be
#' able to assign DOIs to all their research outputs. This way, their outputs
#' become discoverable, and associated metadata is made available to the community.
#' @details The \code{ResourceType} property will be by definition "Dataset".
#' The \code{Size} attribute (e.g. bytes, pages, inches, etc.) will automatically added to
#' the dataset.
#' @param Title The name(s) or title(s) by which a resource is known. May be the
#' title of a dataset or the name of a piece of software.
#' Similar to
#' \href{https://www.dublincore.org/specifications/dublin-core/dcmi-terms/elements11/title/}{dct:title}.\cr
#' @param Creator The main researchers involved in producing the data, or the authors of the publication, in
#' priority order. To supply multiple creators, repeat this property.
#' @param Identifier The Identifier is a unique string that identifies a resource. For software, determine
#' whether the identifier is for a specific version of a piece of software, (per the
#' \href{https://force11.org/info/software-citation-principles-published-2016/}{Force11 Software Citation Principles},
#' or for all versions. Similar to \code{dct:title} in [dublincore()].
#' @param Publisher The name of the entity that holds, archives, publishes prints, distributes,
#' releases, issues, or produces the resource. This property will be used to formulate the
#' citation, so consider the prominence of the role. For software, use Publisher for the
#' code repository. If there is an entity other than a code repository, that "holds, archives,
#' publishes, prints, distributes, releases, issues, or produces" the code, use the property
#' Contributor/contributorType/ hostingInstitution for the code repository. Corresponds to dct:Publisher in
#' [dublincore()].
#' @param PublicationYear The year when the data was or will be made publicly available in
#' \code{YYYY} format.See [publication_year()].
#' @param Subject Recommended for discovery. Subject, keyword, classification code, or key
#' phrase describing the resource. Similar to
#' \href{https://www.dublincore.org/specifications/dublin-core/dcmi-terms/elements11/subject/}{dct:subject}. \cr
#' Use \code{\link{subject}} to properly add a key phrase from a controlled vocabulary
#' and create structured Subject objects with \code{\link{subject_create}}.
#' @param Contributor Recommended for discovery. The institution or person responsible for collecting, managing, distributing, or otherwise contributing to the development of the resource.
#' @param Publisher The name of the entity that holds, archives, publishes prints,
#' distributes, releases, issues, or produces the resource. This property will be used to
#' formulate the citation, so consider the prominence of the role.
#' For software, use Publisher for the code repository. Mandatory in DataCite, and similar to
#' \code{dct:publisher}. See [publisher()].
#' @param Type Defaults to \code{Dataset}.
#' The DataCite resourceType definition refers back to
#' \href{https://www.dublincore.org/specifications/dublin-core/dcmi-terms/elements11/type/}{dcm:type}.
#' The \code{Type$resourceTypeGeneral} is set to  \code{"Dataset"}, while the user can set a more
#' specific \code{Type$resourceType} value.
#' @param Language The primary language of the resource. Allowed values are taken from
#' IETF BCP 47, ISO 639-1 language code. See [language()].
#' @param AlternateIdentifier An identifier or identifiers other than the primary
#' Identifier applied to the resource being registered. This may be any
#' alphanumeric string unique within its domain of issue. It may be used for
#' local identifiers. \code{AlternateIdentifier} should be used for another
#' identifier of the same instance (same location, same file).
#' @param RelatedIdentifier Recommended for discovery. Similar to
#' \href{https://www.dublincore.org/specifications/dublin-core/dcmi-terms/elements11/relation/}{dct:relation}.
#' @param Format Technical format of the resource.
#' Similar to \href{https://www.dublincore.org/specifications/dublin-core/dcmi-terms/elements11/format/}{dct:format}.
#' @param Version Free text. Suggested practice: track major_version.minor_version. See \code{\link{version}}.
#' @param Rights Any rights information for this resource. The property may be repeated to record complex rights characteristics.
#' Free text. See \code{\link{rights}}.
#' @param DateList DataCite 4.4 allows to set multiple dates to a resource, they should
#' be added as a list. See:
#' \href{https://support.datacite.org/docs/datacite-metadata-schema-v44-recommended-and-optional-properties#8-date}{datacite:Date}.
#' @param Description Recommended for discovery. All additional information that does not
#' fit in any of the other categories. It may be used for technical information—a free text.
#' Similar to \href{https://www.dublincore.org/specifications/dublin-core/dcmi-terms/elements11/description/}{dct:description}.
#' @param Geolocation Recommended for discovery. Spatial region or named place where the data was gathered
#' or about which the data is focused. See [geolocation()].
#' @param FundingReference Information about financial support (funding) for the resource
#' being registered.
#' @return A \code{utils::\link[utils]{bibentry}} object DataCite attributes.
#' \code{as_datacite} returns the existing metadata of a dataset object.
#' @source \href{https://support.datacite.org/docs/schema-mandatory-properties-v43}{DataCite 4.3 Mandatory Properties} and
#' \href{https://support.datacite.org/docs/schema-optional-properties-v43}{DataCite 4.3 Optional Properties}
#' @family metadata functions
#' @importFrom utils person bibentry
#' @examples
#' datacite(
#'    Title = "Iris Dataset",
#'    Creator = person(family = "Anderson", given = "Edgar", role = "aut"),
#'    Publisher = "American Iris Society",
#'    PublicationYear = 1935,
#'    Geolocation = "US",
#'    Language = "en")
#'
#' as_datacite(iris_dataset, type="dataset")
#' @export

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
               Format = Format,
               Version = Version,
               Rights = Rights,
               Description = Description,
               Geolocation = Geolocation,
               FundingReference = FundingReference)
}

#' @rdname datacite
#' @param x A dataset object created with \code{dataset::\link{dataset}}.
#' @param type A DataCite 4.4  metadata can be returned as a \code{type="list"},
#' a \code{type="dataset"}, or a \code{type="bibentry"} (default).
#' @param ... Optional parameters to add to a \code{datacite} object.
#' \code{author=person("Jane", "Doe")} adds an author to the citation
#' object if \code{type="dataset"}.
#' @export
as_datacite <- function(x, type = "bibentry", ... ) {

  citation_author <- person(NULL, NULL)

  is_person <- function(p) ifelse (inherits(p, "person"), TRUE, FALSE)

  arguments <- list(...)
  if (!is.null(arguments$author)) {
    if ( is_person(arguments$author))  {
      citation_author <- arguments$author
      } else {
      stop("as_datacite(x, ..., author = ): author must be created with utils::person().")
        }
  }

  if (! type %in% c("bibentry", "list", "dataset")) {
    warning_message <- "as_datacite(ds, type=...) type cannot be "
    warning(warning_message, type, ". Reverting to 'bibentry'.")
    type <- 'bibentry'
  }

  ds_bibentry <- get_bibentry(x)
  Title   <- ds_bibentry$title
  Creator <- ds_bibentry$author
  Publisher <- ifelse (is.null(ds_bibentry$publisher), ":unas", as.character(ds_bibentry$publisher))
  Identifier <- ifelse (is.null(ds_bibentry$identifier), ":tba", as.character(ds_bibentry$identifier))
  Version <- ifelse (is.null(ds_bibentry$version), ":unas", as.character(ds_bibentry$version))
  Description <- ifelse (is.null(ds_bibentry$description), ":unas", as.character(ds_bibentry$description))
  Language <- ifelse (is.null(ds_bibentry$language), ":unas", as.character(ds_bibentry$language))
  DateList <- ifelse (is.null(ds_bibentry$DateList), ":tba", as.character(ds_bibentry$DateList))
  PublicationYear <- ifelse (is.null(ds_bibentry$year), ":unas", as.character(ds_bibentry$year))
  Format <- ifelse (is.null(ds_bibentry$format), ":tba", as.character(ds_bibentry$format))
  AlternateIdentifier <- ifelse (is.null(ds_bibentry$alternateidentifier), ":unas", ds_bibentry$alternateidentifier)
  RelatedIdentifier <- ifelse (is.null(ds_bibentry$relatedidentifier), ":unas", ds_bibentry$relatedidentifier)
  Rights <- ifelse (is.null(ds_bibentry$rights), ":tba", as.character(ds_bibentry$rights))
  Geolocation <- ifelse (is.null(ds_bibentry$geolocation), ":unas", as.character(ds_bibentry$geolocation))
  FundingReference <- ifelse (is.null(ds_bibentry$fundingreference), ":unas", as.character(ds_bibentry$fundingreference))
  Contributor <- ifelse (is.null(ds_bibentry$contributor), "", as.character(ds_bibentry$contributor))
  Subject <- ifelse (is.null(ds_bibentry$subject), "", as.character(ds_bibentry$subject))

  if (Contributor == "") Contributor <- NULL
  if (Subject == "") Subject <- NULL

  if (type == "bibentry") {
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
                 Format = Format,
                 Version = Version,
                 Rights = Rights,
                 Description = Description,
                 Geolocation = Geolocation,
                 FundingReference = FundingReference)
  } else if (type== "list") {

    list(Title = Title,
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
         Format = Format,
         Version = Version,
         Rights = Rights,
         Description = Description,
         Geolocation = Geolocation,
         FundingReference = FundingReference)
  } else if ( type  == "dataset") {
    dataset_df (
      data.frame(Title = Title,
                 Creator = as.character(Creator),
                 Identifier = Identifier,
                 Publisher = Publisher,
                 PublicationYear = PublicationYear,
                 Subject = ifelse (is.null(Subject), "", as.character(Subject)),
                 Type = "Dataset",
                 Contributor = ifelse (is.null(Contributor), ":unas", as.character(Contributor)),
                 DateList = DateList,
                 Language = Language,
                 AlternateIdentifier = AlternateIdentifier,
                 RelatedIdentifier = RelatedIdentifier,
                 Format = Format,
                 Version = Version,
                 Rights = Rights,
                 Description = Description,
                 Geolocation = Geolocation,
                 FundingReference = FundingReference),
      reference = list(title = paste0("The DataCite Metadata of `", ds_bibentry$title, "'"),
                       author = citation_author,
                       year = substr(as.character(Sys.Date()),1,4))
    )
  }
}

#' @keywords internal
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
                          Format,
                          Version,
                          Rights,
                          Description,
                          Geolocation,
                          FundingReference) {

  datacite_object <- bibentry(bibtype = "Misc",
                              title = Title,
                              author = Creator,
                              identifier = Identifier,
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
