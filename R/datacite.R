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
#' @param Title  A name or title by which a resource is known. May be the title of a dataset
#' or the name of a piece of software. Similar to \href{http://purl.org/dc/elements/1.1/title}{dct:title}.
#' @param Creator The main researchers involved in producing the data, or the authors of the publication, in
#' priority order. To supply multiple creators, repeat this property.
#' @param Identifier  The Identifier is a unique string that identifies a resource. For software, determine
#' whether the identifier is for a specific version of a piece of software, (per the \href{Force11 Software
#' Citation Principles}{https://force11.org/info/software-citation-principles-published-2016/},
#' or for all versions. Similar to \code{dct:title} in \code{\link{dublincore}}.
#' @param Pulisher The name of the entity that holds, archives, publishes prints, distributes,
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
#' @param Language  The primary language of the resource. Allowed values are taken from
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
datacite_add <- function(x, Title, Creator,
                         Identifier = NULL,
                         Publisher = NULL,
                         PublicationYear = "THIS",
                         Subject = NULL,
                         Contributor = NULL, Date = NULL,
                         Language = NULL,
                         AlternateIdentifer = NULL, RelatedIdentifier = NULL,
                         Format = NULL, Version = NULL, Rights = NULL,
                         Description = NULL, Geolocation = NULL,
                         FundingReference = NULL,
                         overwrite = TRUE) {

  if (PublicationYear == "THIS") as.integer(substr(Sys.Date(),1,4))

  attr(x, "title") <- Title
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

#' @title Add the primary language of the dataset
#' @description Add the optional Language property as an attribute to an R object.
#' @details Language is an optional property in
#' \href{https://support.datacite.org/docs/schema-optional-properties-v43#13-size}{DataCite 4.3} and
#' it is part of the "core" of the
#' \href{https://www.dublincore.org/specifications/dublin-core/dcmi-terms/}{Dublin Core metadata terms}.
#' The language parameter is validated against the \code{[ISOcodes]{ISO_639_2}} table.\cr
#' The attribute \code{language} is added to the object. It will be exported into DataCite
#' applications in a capitalized \code{Lanugage} format.
#' @param x An R object, such as a data.frame, a tibble, or a character vector.
#' @param language The language to be added to the object attributes, added by name, or
#' as a 2- or 3-character code for the language. You can add a language code or language name,
#' and the parameter is normalised to \code{tolower(language)}. (The ISO 639 standard capitalizes
#' language names and uses lower case for the codes.)
#' @param iso_639_code Defaults to \code{ISO 639-3}, alternative is \code{ISO 639-1}.
#' @return The Language is added to the \code{x} as
#' \code{ISO 639-1}, the Datacite recommendation, or \code{ISO 639-3} used by the
#' Zenodo data repository.
#' @examples
#' iris_dataset <- language_add(x = iris, language= "English")
#' attr(iris_dataset, "Language")
#' @family Metadata functions
#' @export

language_add <- function(x, Language, iso_639_code = "639-3" ) {

  ISO_639 <- ISOcodes::ISO_639_2

  if (nchar(Language)==2) {
    Language <- tolower(Language)
    lang_entry <- ISO_639[which(Language == ISO_639$Alpha_2),]
  } else if ( nchar(Language)== 3) {
    Language <- tolower(Language)
    lang_entry <- ISO_639[which(Language == ISO_639$Alpha_3),]
  } else {
    Language <- tolower(Language)
    lang_entry <-ISO_639[which(Language == tolower(ISO_639$Name)),]
  }

  if (nrow(lang_entry)==0) {
    stop(glue::glue("{Language} is not a valid ISO 639 language code."))
  }

  if (iso_639_code == "639-1") {
    attr(x, "Language") <- lang_entry$Alpha_2
  } else {
    attr(x, "Language") <- lang_entry$Alpha_3_T
  }
  x
}

#' @rdname language_add
#' @family Metadata functions
#' @export
language_get <- function (x) {
  attr(x, "Language")
}


#' @title Add the version of the object.
#' @description Add the optional Version property as an attribute to an R object.
#' @details Version is an optional property in
#' \href{https://support.datacite.org/docs/schema-optional-properties-v43#13-size}{DataCite 4.3}.
#' It is not part of the "core" Dublin Core terms, but ...
#' \href{https://www.dublincore.org/specifications/dublin-core/dcmi-terms/}{Dublin Core metadata terms}.
#' @param x An R object, such as a data.frame, a tibble, or a character vector.
#' @param Version The version as a character set.
#' @return The Version attribute as a character of length 1 is added to \code{x}.
#' @examples
#' iris_dataset <- version_add(x = iris, version= "1.0")
#' attr(iris_dataset, "Version")
#' @family Optional metadata
#' @export
version_add <- function(x, Version ) {

  assertthat::assert_that(is.null(dim(version)),
                          msg = "verion_add(x, Version): version must be a character string or a number that can be coerced into a character string of length=1.")

  attr(x, "Version") <- Version
  x
}
