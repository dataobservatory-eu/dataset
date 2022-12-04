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
#' @param Title \href{https://purl.org/dc/elements/1.1/title}{dct:title}, a name given to the resource.
#' \code{\link{datacite}} allows the use of alternate titles, too. See \code{\link{dataset_title}}.
#' @param Creator An entity primarily responsible for making the resource. \href{https://purl.org/dc/elements/1.1/creator}{dct:creator}
#' Corresponds to \code{Creator} in \code{\link{datacite}}. See \code{\link{creator}}.
#' @param Identifier An unambiguous reference to the resource within a given context.
#' Recommended practice is to identify the resource by means of a string conforming to an
#' identification system. Examples include International Standard Book Number (ISBN),
#' Digital Object Identifier (DOI), and Uniform Resource Name (URN).
#' Select and identifier scheme from
#' \href{https://www.ukoln.ac.uk/metadata/dcmi-ieee/identifiers/index.html}{registered URI schemes maintained by IANA}.
#' More details: \href{https://www.ukoln.ac.uk/metadata/dcmi-ieee/identifiers/}{Guidelines for using resource identifiers in Dublin Core metadata and IEEE LOM}.
#' Similar to \code{Identifier} in \code{\link{datacite}}. See \code{\link{identifier}}.
#' @param Publisher Corresponds to \href{https://www.dublincore.org/specifications/dublin-core/dcmi-terms/#publisher}{dct:publisher}
#' and Publisher in DataCite.
#' The name of the entity that holds, archives, publishes prints, distributes, releases,
#' issues, or produces the resource. This property will be used to formulate the citation,
#' so consider the prominence of the role. For software, use \code{Publisher} for the
#' code repository. If there is an entity other than a code repository, that
#' "holds, archives, publishes, prints, distributes, releases, issues, or produces" the
#' code, use the property Contributor/contributorType/hostingInstitution for the code
#' repository. See \code{\link{publisher}}.
#' @param Subject In \href{https://purl.org/dc/elements/1.1/subject}{dct:subject}. In
#' \code{\link{datacite}} it is a recommended property for discovery. In DataCite, a more complex
#' referencing is used. See \code{\link{subject}} and create structured Subject objects with
#' \code{\link{subject_create}}.
#' @param Date Corresponds to a point or period of time associated with an event in the
#' lifecycle of the resource. \href{https://purl.org/dc/elements/1.1/date}{dct:date}.
#' \code{Date} is also recommended for
#' discovery in \code{\link{datacite}}.
#' @param Source A related resource from which the described resource is derived.
#' See \href{https://purl.org/dc/elements/1.1/source}{dct:source} and
#' \code{\link{dataset_source}}.
#' @param Language The primary language of the resource. Allowed values are taken from
#' IETF BCP 47, ISO 639-1 language code. See \code{\link{language}}. Corresponds to Language in Datacite.
#' @param Format The file format, physical medium, or dimensions of the resource.
#' \href{	https://purl.org/dc/elements/1.1/format}{dct:format}
#' Examples of dimensions include size and duration. Recommended best practice is to use a controlled
#' vocabulary such as the list of \href{https://www.iana.org/assignments/media-types/media-types.xhtml}{Internet Media Types, formerly known as MIME}. It is similar to \code{Format} in
#' \code{\link{datacite}}.
#' @param Rights Corresponds to \href{https://purl.org/dc/elements/1.1/rights}{dct:rights} and
#' \code{\link{datacite}} Rights. Information about rights held in and over the resource.
#' Typically, rights information includes a statement about various property rights associated with the resource,
#' including intellectual property rights. See \code{\link{rights}}.
#' @param Description An account of the resource. It may include but is not limited to:
#' an abstract, a table of contents, a graphical representation, or a free-text account of the resource.
#' \href{https://purl.org/dc/elements/1.1/description}{dct:description}. In
#' \code{\link{datacite}} it is recommended for discovery. See \code{\link{description}}.
#' @param Relation A related resource. Recommended best practice is to identify the related
#' resource by means of a string conforming to a formal identification system. See: \href{https://purl.org/dc/elements/1.1/relation}{dct:relation}.
#' Similar to \code{RelatedItem} in \code{\link{datacite}}, which is recommended for discovery.
#' @param Type The nature or genre of the resource. Recommended best practice is to use a controlled vocabulary such as the DCMI Type Vocabulary
#' \href{https://www.dublincore.org/specifications/dublin-core/dcmi-type-vocabulary/}{DCMITYPE}.
#' For a dataset, the correct term is \code{Dataset}.
#' To describe the file format, physical medium, or dimensions of the resource, use the
#' Format element.
#' @param Subject Defaults to \code{NULL}. See \code{\link{subject}} to add subject descriptions
#' to your dataset.
#' @param overwrite If pre-existing metadata properties should be overwritten,
#' defaults to \code{TRUE}.
#' @importFrom utils person
#' @source \href{https://support.datacite.org/docs/schema-mandatory-properties-v43}{DataCite 4.3 Mandatory Properties} and
#' \href{https://support.datacite.org/docs/schema-optional-properties-v43}{DataCite 4.3 Optional Properties}
#' @family metadata functions
#' @return The Dublin Core Metadata elements of the dataset.
#' @export
#' @examples
#' dct_iris <- dublincore_add(
#'                    x = iris,
#'                    Title = "Iris Dataset",
#'                    Creator = person("Anderson", "Edgar", role = "aut"),
#'                    Publisher = "American Iris Society",
#'                    Source = "https://doi.org/10.1111/j.1469-1809.1936.tb02137.x",
#'                    Date = 1935,
#'                    Language = "en"
#'                    )
#'
#' dublincore(dct_iris)

dublincore <- function(x) {

  attributes_measures <- attributes(x)
  attributes_measures$row.names <- NULL
  attributes_measures$class <- NULL
  attributes_measures$Size <- NULL

  simple_attributes <- attributes_measures
  simple_attributes$dimensions <- NULL
  simple_attributes$measures   <- NULL
  simple_attributes$attributes <- NULL
  simple_attributes$Type <- NULL
  simple_attributes$Title <- NULL
  simple_attributes$names <- NULL

  cat(paste(paste0(names(unlist(attributes_measures$Title)), ": ", unlist(attributes_measures$Title)), collapse = " | "), "\n")
  cat("Publiser: ", simple_attributes$Publisher, " | ")
  cat("Source: ", simple_attributes$Source, " | ")
  cat("Date: ", simple_attributes$Date, " | ")
  cat("Language: ", simple_attributes$Language, " | ")
  cat("Identifier: ", simple_attributes$Identifier, " | ")
  cat("Rights: ", simple_attributes$Rights, " | ")
  cat("Description: ", simple_attributes$Description, " | ")
  cat("\n")

  cat("names: ", paste(attributes_measures$names, collapse = ", "), "\n")

  if(length(attributes_measures$dimensions$names)==0) {
    cat("- dimensions: <none>\n")
  } else {
    cat("- dimensions:", paste0(attributes_measures$dimensions$names, " (", attributes_measures$dimensions$class, ") "), "\n")
  }

  if(length(attributes_measures$measures$names)==0) {
    cat("- measures: <none>\n")
  } else {
    cat("- measures:", paste0(attributes_measures$measures$names, " (", attributes_measures$measures$class, ") "), "\n")
  }

  if(length(attributes_measures$attributes$names)==0) {
    cat("- attributes: <none>\n")
  } else {
    cat("- attributes:", paste0(attributes_measures$attributes$names, " (", attributes_measures$attributes$class, ") "), "\n")
  }




  invisible(attributes_measures)
}

#' @rdname dublincore
#' @export
dublincore_add <- function(x,
                           Title = NULL,
                           Creator = NULL,
                           Identifier = NULL,
                           Publisher = NULL,
                           Subject = NULL,
                           Date = NULL,
                           Source = NULL,
                           Language = NULL,
                           Format = NULL,
                           Rights = NULL,
                           Relation = NULL,
                           Description = NULL,
                           Type = "DCMITYPE:Dataset",
                           overwrite = FALSE) {

  ## Set the Title property ------------------------------------------------
  if (is.null(attr(x, "Title"))) {
    dataset_title(x) <- Title
  } else if ( is.na(attr(x, "Title")[1]) ) {
    dataset_title(x, overwrite = TRUE) <- Title
  } else if ( overwrite ) {
    dataset_title(x, overwrite = TRUE) <- Title
  } else {
    message ("The dataset already has Title(s): ", dataset_title(x) )
  }

  ## Set the Identifier property ---------------------------------------------
  if (is.null(attr(x, "Identifier"))) {
    identifier(x) <- Identifier
  } else if ( is.na(attr(x, "Identifier")) ) {
    identifier(x) <- Identifier
  } else if ( overwrite ) {
    identifier(x) <- Identifier
  } else {
    message ("The dataset already has an Identifier: ", identifier(x) )
  }

  ## Set the Creator property ---------------------------------------------
  if (is.null(attr(x, "Creator"))) {
    creator(x) <- Creator
  } else if ( is.na(attr(x, "Creator")) ) {
    creator(x) <- Creator
  } else if ( overwrite ) {
    creator(x) <- Creator
  } else {
    message ("The dataset already has a Creator: ",  creator(x) )
  }

  ## Set the Subject property ------------------------------------------------
  if (is.null(attr(x, "Subject"))) {
    subject(x) <- Subject
  } else if ( is.na(attr(x, "Subject")) ) {
    subject(x) <- Subject
  } else if ( overwrite ) {
    subject(x) <- Subject
  } else {
    message ("The dataset already has Subject(s): ", subject(x) )
  }

  ## Set the Source property ------------------------------------------------
  if (is.null(attr(x, "Source"))) {
    dataset_source(x) <- Source
 # } else if ( is.na(attr(x, "Subject")) ) {
#    dataset_source(x, overwrite = overwrite) <- Source
  } else if ( overwrite ) {
    dataset_source(x, overwrite = overwrite) <- Source
  } else {
    message ("The dataset already has a Source: ", dataset_source(x) )
  }

  ## Set the Publisher property ---------------------------------------------
  if (is.null(attr(x, "Publisher"))) {
    publisher(x) <- Publisher
  } else if ( is.na(attr(x, "Publisher")) ) {
    publisher(x) <- Publisher
  } else if ( overwrite ) {
    publisher(x, overwrite = overwrite) <- Publisher
  } else {
    message ("The dataset already has a Publisher: ", publisher(x) )
  }

  ## Set the Right property ---------------------------------------------
  if (is.null(attr(x, "Rights"))) {
    rights(x) <- Rights
  } else if ( is.na(attr(x, "Rights")) ) {
    rights(x) <- Rights
  } else if ( overwrite ) {
    rights(x, overwrite = overwrite) <- Rights
  } else {
    message ("The dataset already has a Rights (declaration): ", rights(x) )
  }

  ## Set the Format property ---------------------------------------------
  if (is.null(attr(x, "Format"))) {
    attr(x, "Format") <- Format
  } else if ( overwrite ) {
    attr(x, "Format") <- Format
  } else {
    message ("The dataset already has a Format: ",  attr(x, "Format") )
  }

  ## Set the Issued property ---------------------------------------------
  attr(x, "Issued") <- Date

  ## Set the Type property ------------------------------------------------
  if (is.null(Type)) {
    Type <- resource_type(x)
    }
  if (is.null(Type)) Type <- "DCMITYPE:Dataset"
  resource_type(x) <- Type

  ## Set the Description property -----------------------------------------
  if (is.null(attr(x, "Description"))) {
    description(x) <- Description
  } else if ( is.na(attr(x, "Description")) ) {
    description(x) <- Description
  } else if ( overwrite ) {
    description(x) <- Description
  } else {
    message ("The dataset already has a Description property: ", description(x) )
  }

  ## Set the Language property --------------------------------
  if (is.null(attr(x, "Language"))) {
    language(x) <- Language
  } else if ( is.na(attr(x, "Language")) ) {
    language(x) <- Language
  } else if ( overwrite ) {
    language(x) <- Language
  } else {
    message ("The dataset already has a Language property: ", language(x) )
  }

  x <- size(x)
  x
}



