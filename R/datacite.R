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
#' # Bibliographic metadata as bibentry...
#' as_datacite(orange_df)
#'
#' # ... or a list:
#' as_datacite(orange_df, "list")
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
  if (is.null(PublicationYear)) {
    if (!is.null(Date) && grepl("^\\d{4}", as.character(Date))) {
      PublicationYear <- substr(as.character(Date), 1, 4)
    } else {
      PublicationYear <- ":tba"
    }
  }

  Date <- ifelse(is.null(DateList), ":tba", as.character(Date))
  DateList <- ifelse(is.null(DateList), ":tba", as.character(DateList))
  Format <- ifelse(is.null(Format), ":tba", as.character(Format))
  AlternateIdentifier <- ifelse(is.null(AlternateIdentifier),
                                ":unas", AlternateIdentifier)
  RelatedIdentifier <- ifelse(is.null(RelatedIdentifier),
                              ":unas", RelatedIdentifier)
  Rights <- ifelse(is.null(Rights), ":tba", as.character(Rights))
  Geolocation <- ifelse(is.null(Geolocation), ":unas",
                        as.character(Geolocation))
  FundingReference <- ifelse(is.null(FundingReference),
                             ":unas", as.character(FundingReference))

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
  datacite_object <- bibentry(
    bibtype = "Misc",
    title = Title,
    author = Creator,
    year = as.character(PublicationYear),
    identifier = Identifier,
    publisher = Publisher,
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

  # Store contributor as structured attribute
  if (!is.null(Contributor)) {
    attr(datacite_object, "contributor") <- Contributor
  }

  class(datacite_object) <- c("datacite", class(datacite_object))
  datacite_object
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

#' @exportS3Method
print.datacite <- function(x, ...) {
  cat("DataCite Metadata Record\n")
  cat("--------------------------\n")
  cat("Title:        ", x$title, "\n")
  cat("Creator(s):   ", paste(format(x$author), collapse = "; "), "\n")

  contributor <- attr(x, "contributor")
  if (!is.null(contributor)) {
    cat("Contributor(s):", fix_contributor(contributor), "\n")
  }

  if (!is.null(x$identifier)) cat("Identifier:   ", x$identifier, "\n")
  if (!is.null(x$publisher)) cat("Publisher:    ", x$publisher, "\n")
  if (!is.null(x$year)) cat("Year:         ", x$year, "\n")
  if (!is.null(x$language)) cat("Language:     ", x$language, "\n")
  if (!is.null(x$description)) cat("Description: ", x$description, "\n")

  invisible(x)
}

#' @keywords internal
datacite_to_triples <- function(dc_list,
                                dataset_id = "http://example.com/dataset") {

  if (is.null(dc_list$title) || nchar(dc_list$title) == 0) {
    stop("datacite_to_triples(): title is required")
  }

  base <- "http://datacite.org/schema/kernel-4/"
  triples <- character()

  triples <- c(triples, n_triple(dataset_id,
                                 paste0(base, "title"),
                                 dc_list$title))

  if (!is.null(dc_list$creator)) {
    triples <- c(triples, n_triple(dataset_id,
                                   paste0(base, "creator"),
                                   dc_list$creator))
  }

  if (!is.null(dc_list$contributor)) {
    triples <- c(triples, n_triple(dataset_id,
                                   paste0(base, "contributor"),
                                   dc_list$contributor))
  }

  if (!is.null(dc_list$identifier)) {
    triples <- c(triples, n_triple(dataset_id,
                                   paste0(base, "identifier"),
                                   dc_list$identifier))
  }

  if (!is.null(dc_list$publisher)) {
    triples <- c(triples, n_triple(dataset_id,
                                   paste0(base, "publisher"),
                                   dc_list$publisher))
  }

  if (!is.null(dc_list$publicationyear)) {
    triples <- c(triples, n_triple(dataset_id,
                                   paste0(base, "publicationYear"),
                                   dc_list$publicationyear))
  }

  if (!is.null(dc_list$language)) {
    triples <- c(triples, n_triple(dataset_id,
                                   paste0(base, "language"),
                                   dc_list$language))
  }

  if (!is.null(dc_list$rights)) {
    triples <- c(triples, n_triple(dataset_id,
                                   paste0(base, "rights"),
                                   dc_list$rights))
  }

  if (!is.null(dc_list$description)) {
    triples <- c(triples, n_triple(dataset_id,
                                   paste0(base, "descriptions"),
                                   dc_list$description))
  }

  if (!is.null(dc_list$subject)) {
    triples <- c(triples, n_triple(dataset_id,
                                   paste0(base, "subjects"),
                                   dc_list$subject))
  }

  if (!is.null(dc_list$format)) {
    triples <- c(triples, n_triple(dataset_id,
                                   paste0(base, "formats"),
                                   dc_list$format))
  }

  if (!is.null(dc_list$version)) {
    triples <- c(triples, n_triple(dataset_id,
                                   paste0(base, "version"),
                                   dc_list$version))
  }

  n_triples(triples)
}
