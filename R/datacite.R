#' @title Create a bibentry object with DataCite metadata fields
#' @description Add metadata conforming the
#'   \href{https://schema.datacite.org/}{DataCite Metadata Schema}.
#' @details DataCite is a leading global non-profit organisation that provides
#'   persistent identifiers (DOIs) for research data and other research outputs.
#'   Organisations within the research community join DataCite as members to be
#'   able to assign DOIs to all their research outputs. This way, their outputs
#'   become discoverable, and associated metadata is made available to the
#'   community.
#' @details The \code{ResourceType} property will be by definition "Dataset".
#'   The \code{Size} attribute (e.g. bytes, pages, inches, etc.) will
#'   automatically added to the dataset.
#' @param Title The name(s) or title(s) by which a resource is known. May be the
#'   title of a dataset or the name of a piece of software. Similar to
#'   \href{https://www.dublincore.org/specifications/dublin-core/dcmi-terms/elements11/title/}{dct:title}.\cr
#' @param Creator The main researchers involved in producing the data, or the
#'   authors of the publication, in priority order. To supply multiple creators,
#'   repeat this property.
#' @param Identifier The Identifier is a unique string that identifies a
#'   resource. For software, determine whether the identifier is for a specific
#'   version of a piece of software, (per the
#'   \href{https://force11.org/info/software-citation-principles-published-2016/}{Force11
#'   Software Citation Principles}, or for all versions. Similar to
#'   \code{dct:title} in [dublincore()].
#' @param Publisher The name of the entity that holds, archives, publishes
#'   prints, distributes, releases, issues, or produces the resource. This
#'   property will be used to formulate the citation, so consider the prominence
#'   of the role. For software, use Publisher for the code repository. If there
#'   is an entity other than a code repository, that "holds, archives,
#'   publishes, prints, distributes, releases, issues, or produces" the code,
#'   use the property Contributor/contributorType/ hostingInstitution for the
#'   code repository. Corresponds to dct:Publisher in [dublincore()].
#' @param PublicationYear The year when the data was or will be made publicly
#'   available in \code{YYYY} format.See [publication_year()].
#' @param Subject Recommended for discovery. Subject, keyword, classification
#'   code, or key phrase describing the resource. Similar to
#'   \href{https://www.dublincore.org/specifications/dublin-core/dcmi-terms/elements11/subject/}{dct:subject}.
#'   \cr Use \code{\link{subject}} to properly add a key phrase from a
#'   controlled vocabulary and create structured Subject objects with
#'   \code{\link{subject_create}}.
#' @param Contributor Recommended for discovery. The institution or person
#'   responsible for collecting, managing, distributing, or otherwise
#'   contributing to the development of the resource.
#' @param Publisher The name of the entity that holds, archives, publishes
#'   prints, distributes, releases, issues, or produces the resource. This
#'   property will be used to formulate the citation, so consider the prominence
#'   of the role. For software, use Publisher for the code repository. Mandatory
#'   in DataCite, and similar to \code{dct:publisher}. See [publisher()].
#' @param Type Defaults to \code{Dataset}. The DataCite resourceType definition
#'   refers back to
#'   \href{https://www.dublincore.org/specifications/dublin-core/dcmi-terms/elements11/type/}{dcm:type}.
#'   The \code{Type$resourceTypeGeneral} is set to  \code{"Dataset"}, while the
#'   user can set a more specific \code{Type$resourceType} value.
#' @param Language The primary language of the resource. Allowed values are
#'   taken from IETF BCP 47, ISO 639-1 language code. See [language()].
#' @param AlternateIdentifier An identifier or identifiers other than the
#'   primary Identifier applied to the resource being registered. This may be
#'   any alphanumeric string unique within its domain of issue. It may be used
#'   for local identifiers. \code{AlternateIdentifier} should be used for
#'   another identifier of the same instance (same location, same file).
#'   Defaults to \code{":unas"} for unassigned values.
#' @param RelatedIdentifier Recommended for discovery. Defaults to
#'   \code{":unas"} for unassigned values. Similar to
#'   \href{https://www.dublincore.org/specifications/dublin-core/dcmi-terms/elements11/relation/}{dct:relation}.
#' @param Format Technical format of the resource. Use file extension or MIME
#'   type where possible, e.g., PDF, XML, MPG or application/pdf, text/xml,
#'   video/mpeg. Similar to
#'   \href{https://www.dublincore.org/specifications/dublin-core/dcmi-terms/elements11/format/}{dct:format}.
#' @param Version Free text. Suggested practice: track
#'   major_version.minor_version. Defaults to \code{"0.1.0"}. See
#'   \code{\link{version}}.
#' @param Rights Any rights information for this resource. The property may be
#'   repeated to record complex rights characteristics, but this is not yet
#'   supported. Free text. See \code{\link{rights}}. Defaults to \code{":tba"}.
#' @param Date A character string in any of the following formats: \code{YYYY},
#'   \code{YYYY-MM-DD} or \code{YYYY-MM-DDThh:mm:ssTZD}, or an R Date or POSIXct
#'   object. A list of dates (parameter \code{DateList}) is not yet implemented.
#' @param DateList DataCite 4.4 allows to set multiple dates to a resource, they
#'   should be added as a list. Currently not yet implemented.
#'    See:
#'   \href{https://support.datacite.org/docs/datacite-metadata-schema-v44-recommended-and-optional-properties#'8-date}{datacite:Date}.
#' @param Description Recommended for discovery. All additional information that
#'   does not fit in any of the other categories. It may be used for technical
#'   information—a free text. Defaults to \code{":tba"}. Similar to
#'   \href{https://www.dublincore.org/specifications/dublin-core/dcmi-terms/elements11/description/}{dct:description}.
#' @param Geolocation Recommended for discovery. Spatial region or named place
#'   where the data was gathered or about which the data is focused. See
#'   [geolocation()].
#' @param FundingReference Information about financial support (funding) for the
#'   resource being registered. Defaults to \code{":unas"} for unassigned
#'   values. Complex types with subproperties are not yet implemented.
#' @return \code{datacite()} creates a \code{utils::\link[utils]{bibentry}}
#'   object extended with standard Dublin Core bibliographical metadata,
#'   \code{as_datacite()} retrieves the contents of this bibentry object of a
#'   dataset_df from its attributes, and returns the contents as list,
#'   dataset_df, or bibentry object.
#' @source
#'   \href{https://support.datacite.org/docs/schema-mandatory-properties-v43}{DataCite
#'   4.3 Mandatory Properties} and
#'   \href{https://support.datacite.org/docs/schema-optional-properties-v43}{DataCite
#'   4.3 Optional Properties}
#' @family bibentry functions
#' @importFrom utils person bibentry
#' @examples
#' datacite(
#'   Title = "Growth of Orange Trees",
#'   Creator = c(
#'     person(
#'       given = "N.R.",
#'       family = "Draper",
#'       role = "cre",
#'       comment = c(VIAF = "http://viaf.org/viaf/84585260")
#'     ),
#'     person(
#'       given = "H",
#'       family = "Smith",
#'       role = "cre"
#'     )
#'   ),
#'   Publisher = "Wiley",
#'   Date = 1998,
#'   Language = "en"
#' )
#'
#' as_datacite(orange_df)
#' @export

datacite <- function(Title,
                     Creator,
                     Identifier = NULL,
                     Publisher = NULL,
                     PublicationYear = NULL,
                     Subject = subject_create(
                       term = "data sets",
                       subjectScheme = "Library of Congress Subject Headings (LCSH)",
                       schemeURI = "https://id.loc.gov/authorities/subjects.html",
                       valueURI = "http://id.loc.gov/authorities/subjects/sh2018002256"
                     ),
                     Type = "Dataset",
                     Contributor = NULL,
                     Date = ":tba",
                     DateList = NULL,
                     Language = NULL,
                     AlternateIdentifier = ":unas",
                     RelatedIdentifier = ":unas",
                     Format = ":tba",
                     Version = "0.1.0",
                     Rights = ":tba",
                     Description = ":tba",
                     Geolocation = ":unas",
                     FundingReference = ":unas") {
  Date <- ifelse(is.null(DateList), ":tba", as.character(Date))
  DateList <- ifelse(is.null(DateList), ":tba", as.character(DateList))
  Format <- ifelse(is.null(Format), ":tba", as.character(Format))
  AlternateIdentifier <- ifelse(is.null(AlternateIdentifier), ":unas", AlternateIdentifier)
  RelatedIdentifier <- ifelse(is.null(RelatedIdentifier), ":unas", RelatedIdentifier)
  Rights <- ifelse(is.null(Rights), ":tba", as.character(Rights))
  Geolocation <- ifelse(is.null(Geolocation), ":unas", as.character(Geolocation))
  FundingReference <- ifelse(is.null(FundingReference), ":unas", as.character(FundingReference))

  new_datacite(
    Title = Title,
    Creator = Creator,
    Identifier = Identifier,
    Publisher = Publisher,
    PublicationYear = PublicationYear,
    Subject = Subject,
    Type = "Dataset",
    Contributor = Contributor,
    Date = Date,
    DateList = DateList,
    Language = Language,
    AlternateIdentifier = AlternateIdentifier,
    RelatedIdentifier = RelatedIdentifier,
    Format = Format,
    Version = Version,
    Rights = Rights,
    Description = Description,
    Geolocation = Geolocation,
    FundingReference = FundingReference
  )
}


#' @keywords internal
new_datacite <- function(Title,
                         Creator,
                         Identifier,
                         Publisher,
                         PublicationYear,
                         Subject,
                         Type = "Dataset",
                         Contributor,
                         Date,
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
  #' Create year from Date
  if (!is.null(Date)) {
    year <- substr(as.character(Date), 1, 4)
  } else {
    year <- NA_character_
  }

  datacite_object <- bibentry(
    bibtype = "Misc",
    title = Title,
    author = Creator,
    year = year,
    identifier = Identifier,
    publisher = Publisher,
    year = PublicationYear,
    date = Date,
    language = Language,
    subject = Subject$term,
    alternateidentifier = AlternateIdentifier,
    relatedidentifier = RelatedIdentifier,
    format = Format,
    version = Version,
    rights = Rights,
    description = Description,
    geolocation = Geolocation,
    fundingreference = FundingReference
  )

  class(datacite_object) <- c("datacite", class(datacite_object))
  datacite_object
}

#' @rdname datacite
#' @param x A dataset object created with \code{dataset::\link{dataset}}.
#' @param type A DataCite 4.4  metadata can be returned as a \code{type="list"},
#'   a \code{type="dataset_df"}, or a \code{type="bibentry"} (default).
#' @param ... Optional parameters to add to a \code{datacite} object.
#'   \code{author=person("Jane", "Doe")} adds an author to the citation object
#'   if \code{type="dataset"}. as_datacite(orange_df, type="list")
#' @return \code{as_datacite(x, type)} returns the DataCite bibliographical
#'   metadata of x either as a list, a bibentry object, or a dataset_df object.
#' @export
as_datacite <- function(x, type = "bibentry", ...) {
  citation_author <- person(NULL, NULL)

  is_person <- function(p) ifelse(inherits(p, "person"), TRUE, FALSE)

  arguments <- list(...)
  if (!is.null(arguments$author)) {
    if (is_person(arguments$author)) {
      citation_author <- arguments$author
    } else {
      stop("as_datacite(x, ..., author = ): author must be created with utils::person().")
    }
  }

  if (!type %in% c("bibentry", "list", "dataset_df")) {
    warning_message <- "as_datacite(ds, type=...) type cannot be "
    warning(warning_message, type, ". Reverting to 'bibentry'.")
    type <- "bibentry"
  }

  ds_bibentry <- get_bibentry(x)
  Title <- ds_bibentry$title
  Creator <- ds_bibentry$author
  Publisher <- ifelse(is.null(ds_bibentry$publisher), ":unas", as.character(ds_bibentry$publisher))
  Identifier <- ifelse(is.null(ds_bibentry$identifier), ":tba", as.character(ds_bibentry$identifier))
  Version <- ifelse(is.null(ds_bibentry$version), ":unas", as.character(ds_bibentry$version))
  Description <- ifelse(is.null(ds_bibentry$description), ":unas", as.character(ds_bibentry$description))
  Language <- ifelse(is.null(ds_bibentry$language), ":unas", as.character(ds_bibentry$language))
  Date <- ifelse(is.null(ds_bibentry$Date), ":tba", as.character(ds_bibentry$Date))
  DateList <- ifelse(is.null(ds_bibentry$DateList), ":tba", as.character(ds_bibentry$DateList))
  PublicationYear <- ifelse(is.null(ds_bibentry$year), ":unas", as.character(ds_bibentry$year))
  Format <- ifelse(is.null(ds_bibentry$format), ":tba", as.character(ds_bibentry$format))
  AlternateIdentifier <- ifelse(is.null(ds_bibentry$alternateidentifier), ":unas", ds_bibentry$alternateidentifier)
  RelatedIdentifier <- ifelse(is.null(ds_bibentry$relatedidentifier), ":unas", ds_bibentry$relatedidentifier)
  Rights <- ifelse(is.null(ds_bibentry$rights), ":tba", as.character(ds_bibentry$rights))
  Geolocation <- ifelse(is.null(ds_bibentry$geolocation), ":unas", as.character(ds_bibentry$geolocation))
  FundingReference <- ifelse(is.null(ds_bibentry$fundingreference), ":unas", as.character(ds_bibentry$fundingreference))
  Contributor <- ifelse(is.null(ds_bibentry$contributor), "", as.character(ds_bibentry$contributor))
  Subject <- ifelse(is.null(subject(x)), new_Subject(":tba"), subject(x))

  if (type == "bibentry") {
    new_datacite(
      Title = Title,
      Creator = Creator,
      Identifier = Identifier,
      Publisher = Publisher,
      PublicationYear = PublicationYear,
      Subject = Subject,
      Type = "Dataset",
      Contributor = Contributor,
      Date = Date,
      DateList = DateList,
      Language = Language,
      AlternateIdentifier = AlternateIdentifier,
      RelatedIdentifier = RelatedIdentifier,
      Format = Format,
      Version = Version,
      Rights = Rights,
      Description = Description,
      Geolocation = Geolocation,
      FundingReference = FundingReference
    )
  } else if (type == "list") {
    list(
      Title = Title,
      Creator = Creator,
      Identifier = Identifier,
      Publisher = Publisher,
      PublicationYear = PublicationYear,
      Subject = Subject,
      Type = "Dataset",
      Contributor = Contributor,
      Date = Date,
      DateList = DateList,
      Language = Language,
      AlternateIdentifier = AlternateIdentifier,
      RelatedIdentifier = RelatedIdentifier,
      Format = Format,
      Version = Version,
      Rights = Rights,
      Description = Description,
      Geolocation = Geolocation,
      FundingReference = FundingReference
    )
  } else if (type == "dataset_df") {
    dataset_df(
      data.frame(
        Title = Title,
        Creator = as.character(Creator),
        Identifier = Identifier,
        Publisher = Publisher,
        PublicationYear = PublicationYear,
        Subject = ifelse(is.null(Subject), "", as.character(Subject)),
        Type = "Dataset",
        Contributor = ifelse(is.null(Contributor), ":unas", as.character(Contributor)),
        Date = Date,
        DateList = DateList,
        Language = Language,
        AlternateIdentifier = AlternateIdentifier,
        RelatedIdentifier = RelatedIdentifier,
        Format = Format,
        Version = Version,
        Rights = Rights,
        Description = Description,
        Geolocation = Geolocation,
        FundingReference = FundingReference
      )
    )
  }
}


#' @rdname datacite
#' @return \code{is.datacite(x)} returns a logical values (if the object
#' \code{x} is of class \code{datacite}).
is.datacite <- function(x) {
  UseMethod("is.datacite", x)
}

#' @rdname datacite
#' @param x An object that is tested if it has a class "datacite".
#' @exportS3Method
is.datacite.datacite <- function(x) inherits(x, "datacite")
