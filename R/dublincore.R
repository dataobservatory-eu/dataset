#' @title Add or get Dublin Core metadata
#' @description Add metadata conforming the
#'   \href{https://www.dublincore.org/specifications/dublin-core/dcmi-terms/terms/format/}{DCMI
#'   Metadata Terms}. to datasets, i.e. structured R data.frame or list objects,
#'   for an accurate and consistent identification of a resource for citation
#'   and retrieval purposes.
#' @details The Dublin Core, also known as the Dublin Core Metadata Element Set
#'   (DCMES), is a set of fifteen main metadata items for describing digital or
#'   physical resources, such as datasets or their printed versions. Dublin Core
#'   has been formally standardized internationally as ISO 15836, as IETF RFC
#'   5013 by the Internet Engineering Task Force (IETF), as well as in the U.S.
#'   as ANSI/NISO Z39.85.\cr
#'   \cr
#'   To provide compatibility with \code{\link[utils]{bibentry}}  we try to add
#'   \code{dataset_date} parameter first as \code{publication_date} metadata
#'   field, and as a \code{year} field, too. This element can be get or set with
#'   \code{\link{publication_year}}.
#' @param x An R object of type data.frame, or inherited data.table, tibble;
#'   alternatively a well structured R list.
#' @details The \code{ResourceType} property will be by definition "Dataset".
#'   The \code{Size} attribute (e.g. bytes, pages, inches, etc.) will
#'   automatically added to the dataset.
#' @param title
#'   \href{https://www.dublincore.org/specifications/dublin-core/dcmi-terms/elements11/title/}{dct:title},
#'   a name given to the resource. \code{\link{datacite}} allows the use of
#'   alternate titles, too. See \code{\link{dataset_title}}.
#' @param creator An entity primarily responsible for making the resource.
#'   \href{https://www.dublincore.org/specifications/dublin-core/dcmi-terms/elements11/creator/}{dct:creator}
#'   Corresponds to \code{Creator} in \code{\link{datacite}}. See
#'   \code{\link{creator}}.
#' @param identifier An unambiguous reference to the resource within a given
#'   context. Recommended practice is to identify the resource by means of a
#'   string conforming to an identification system. Examples include
#'   International Standard Book Number (ISBN), Digital Object Identifier (DOI),
#'   and Uniform Resource Name (URN). Select and identifier scheme from
#'   \href{https://www.ukoln.ac.uk/metadata/dcmi-ieee/identifiers/index.html}{registered
#'   URI schemes maintained by IANA}. More details:
#'   \href{https://www.ukoln.ac.uk/metadata/dcmi-ieee/identifiers/}{Guidelines
#'   for using resource identifiers in Dublin Core metadata and IEEE LOM}.
#'   Similar to \code{Identifier} in \code{\link{datacite}}. See
#'   \code{\link{identifier}}.
#' @param publisher Corresponds to
#'   \href{https://www.dublincore.org/specifications/dublin-core/dcmi-terms/#publisher}{dct:publisher}
#'   and Publisher in DataCite. The name of the entity that holds, archives,
#'   publishes prints, distributes, releases, issues, or produces the resource.
#'   This property will be used to formulate the citation, so consider the
#'   prominence of the role. For software, use \code{Publisher} for the code
#'   repository. If there is an entity other than a code repository, that
#'   "holds, archives, publishes, prints, distributes, releases, issues, or
#'   produces" the code, use the property
#'   Contributor/contributorType/hostingInstitution for the code repository. See
#'   \code{\link{publisher}}.
#' @param subject In
#'   \href{https://www.dublincore.org/specifications/dublin-core/dcmi-terms/elements11/subject/}{dct:subject}.
#'   In \code{\link{datacite}} it is a recommended property for discovery. In
#'   DataCite, a more complex referencing is used. See \code{\link{subject}} and
#'   create structured Subject objects with \code{\link{subject_create}}.
#' @param dataset_date Corresponds to a point or period of time associated with
#'   an event in the lifecycle of the resource.
#'   \href{https://www.dublincore.org/specifications/dublin-core/dcmi-terms/elements11/date/}{dct:date}.
#'   \code{Date} is also recommended for discovery in \code{\link{datacite}},
#'   but it requires a different formatting. To avoid confusion with
#'   date-related functions, instead of the DCMITERMS date or the DataCite Date
#'   term, the parameter name is \code{dataset_date}.
#' @param language The primary language of the resource. Allowed values are
#'   taken from IETF BCP 47, ISO 639-1 language code. See
#'   \code{\link{language}}. Corresponds to Language in Datacite.
#' @param format The file format, physical medium, or dimensions of the
#'   resource.
#'   \href{https://www.dublincore.org/specifications/dublin-core/dcmi-terms/elements11/format/}{dct:format}
#'   Examples of dimensions include size and duration. Recommended best practice
#'   is to use a controlled vocabulary such as the list of
#'   \href{https://www.iana.org/assignments/media-types/media-types.xhtml}{Internet
#'   Media Types, formerly known as MIME}. It is similar to \code{Format} in
#'   \code{\link{datacite}}.
#' @param rights Corresponds to
#'   \href{https://www.dublincore.org/specifications/dublin-core/dcmi-terms/elements11/rights/}{dct:rights}
#'   and \code{\link{datacite}} Rights. Information about rights held in and
#'   over the resource. Typically, rights information includes a statement about
#'   various property rights associated with the resource, including
#'   intellectual property rights. See \code{\link{rights}}.
#' @param description An account of the resource. It may include but is not
#'   limited to: an abstract, a table of contents, a graphical representation,
#'   or a free-text account of the resource.
#'   \href{https://www.dublincore.org/specifications/dublin-core/dcmi-terms/elements11/description/}{dct:description}.
#'   In \code{\link{datacite}} it is recommended for discovery. See
#'   \code{\link{description}}.
#' @param relation A related resource. Recommended best practice is to identify
#'   the related resource by means of a string conforming to a formal
#'   identification system. See:
#'   \href{https://www.dublincore.org/specifications/dublin-core/dcmi-terms/elements11/relation/}{dct:relation}.
#'   Similar to \code{RelatedItem} in \code{\link{datacite}}, which is
#'   recommended for discovery.
#' @param type The nature or genre of the resource. Recommended best practice is
#'   to use a controlled vocabulary such as the DCMI Type Vocabulary
#'   \href{https://www.dublincore.org/specifications/dublin-core/dcmi-type-vocabulary/}{DCMITYPE}.
#'   For a dataset, the correct term is \code{Dataset}. To describe the file
#'   format, physical medium, or dimensions of the resource, use the Format
#'   element.
#' @param subject Defaults to \code{NULL}. See \code{\link{subject}} to add
#'   subject descriptions to your dataset.
#' @param datasource The source of the dataset,
#'   \href{https://www.dublincore.org/specifications/dublin-core/dcmi-terms/elements11/source/}{DCMI:
#'   Source}, which corresponds to a \code{relatedItem} in the DataCite
#'   vocabulary. We use \code{datasource} instead of \code{source} to avoid
#'   naming conflicts with the
#' @param format The file format, physical medium, or dimensions of the dataset.
#'   See
#'   \href{https://www.dublincore.org/specifications/dublin-core/dcmi-terms/elements11/source/}{DCMI:
#'   Format}.
#' @param coverage The spatial or temporal topic of the resource, spatial
#'   applicability of the dataset, or jurisdiction under which the dataset is
#'   relevant. See
#'   \href{https://www.dublincore.org/specifications/dublin-core/dcmi-terms/elements11/coverage/}{DCMI:
#'   Coverage}.
#' @param contributor An entity responsible for making contributions to the
#'   dataset. See
#'   \href{https://www.dublincore.org/specifications/dublin-core/dcmi-terms/elements11/contributor/}{DCMI:
#'   Contributor}, and for possible contribution type, please review
#'   \href{https://www.loc.gov/marc/relators/relaterm.html}{MARC Code List for
#'   Relators}.
#' @param language A language of the dataset. See
#'   \href{https://www.dublincore.org/specifications/dublin-core/dcmi-terms/elements11/language/}{DCMI:
#'   Language}.
#' @importFrom utils person bibentry
#' @source
#'   \href{https://www.dublincore.org/specifications/dublin-core/dcmi-terms/terms/format/}{	DCMI
#'   Metadata Terms}.
#' @family bibentry functions
#' @return \code{dublincore()} creates a \code{utils::\link[utils]{bibentry}}
#'   object extended with standard Dublin Core bibliographical metadata,
#'   \code{as_dublincore()} retrieves the contents of this bibentry object of a
#'   dataset_df from its attributes, and returns the contents as list,
#'   dataset_df, or bibentry object, or an ntriples string.
#' @export
#' @examples
#' orange_bibentry <- dublincore(
#'   title = "Growth of Orange Trees",
#'   creator = c(
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
#'   contributor = person(
#'     given = "Antal",
#'     family = "Daniel",
#'     role = "dtm"
#'   ), #' Add data manager
#'   publisher = "Wiley",
#'   datasource = "https://isbnsearch.org/isbn/9780471170822",
#'   dataset_date = 1998,
#'   identifier = "https://doi.org/10.5281/zenodo.14917851",
#'   language = "en",
#'   description = "The Orange data frame has 35 rows and 3 columns of records of the growth of orange trees."
#' )
#'
#' # To review the existing dataset_bibentry of a dataset_df object:
#' as_dublincore(orange_df, type = "list")
dublincore <- function(
    title,
    creator,
    contributor = NULL,
    publisher = NULL,
    identifier = NULL,
    subject = NULL,
    type = "DCMITYPE:Dataset",
    dataset_date = NULL,
    language = NULL,
    relation = NULL,
    format = "application/r-rds",
    rights = NULL,
    datasource = NULL,
    description = NULL,
    coverage = NULL) {
  if (missing(creator) || is.null(creator)) {
    stop("dublincore(): A valid `creator` (as person or list of person) is required.")
  }

  if (inherits(creator, "person")) {
    creator <- list(creator)
  }

  creators <- normalize_roles(creator, default_role = "cre")

  dataset_date <- ifelse(is.null(dataset_date), ":tba", as.character(dataset_date))
  identifier <- ifelse(is.null(identifier), ":tba", as.character(identifier))
  format <- ifelse(is.null(format), ":tba", as.character(format))
  relation <- ifelse(is.null(relation), ":unas", relation)
  format <- ifelse(is.null(relation), ":unas", relation)
  rights <- ifelse(is.null(rights), ":tba", as.character(rights))
  coverage <- ifelse(is.null(coverage), ":unas", as.character(coverage))
  datasource <- ifelse(is.null(datasource), ":unas", as.character(datasource))
  publishers <- if (is.null(publisher)) ":unas" else publisher
  contributor <- if (is.null(contributor)) NULL else contributor
  creators <- if (is.null(creator)) creators <- ":tba" else creators <- creator
  year <- if (!is.null(dataset_date)) substr(as.character(dataset_date), 1, 4) else NULL

  publisher <- fix_publisher(publishers = publishers)

  new_dublincore(
    title = title,
    creator = creators,
    identifier = identifier,
    publisher = publisher,
    subject = subject,
    type = type,
    contributor = contributor,
    dataset_date = dataset_date,
    year = year,
    language = language,
    relation = relation,
    format = format,
    rights = rights,
    datasource = datasource,
    description = description,
    coverage = coverage
  )
}

#' @keywords internal
dublincore_to_triples <- function(
    dclist,
    dataset_id = "http://example.com/dataset") {
  if (is.null(dclist) || is.null(dclist$title) || nchar(dclist$title) == 0) {
    stop("Error: dublincore_to_triples(dclist, dataset_id): no title found in dclist")
  }

  dctriples <- n_triple(
    dataset_id,
    "http://purl.org/dc/terms/title",
    dclist$title
  )

  if (!is.null(dclist$description)) {
    dctriples <- c(dctriples, n_triple(
      dataset_id,
      "http://purl.org/dc/terms/description",
      dclist$description
    ))
  }

  if (!is.null(dclist$creator)) {
    tcreator <- n_triple(
      dataset_id,
      "http://purl.org/dc/terms/creator",
      dclist$creator
    )
    dctriples <- c(dctriples, tcreator)
  }

  if (!is.null(dclist$publisher)) {
    dctriples <- c(dctriples, n_triple(
      dataset_id,
      "http://purl.org/dc/terms/publisher",
      dclist$publisher
    ))
  }

  if (!is.null(dclist$identifier)) {
    dctriples <- c(dctriples, n_triple(
      dataset_id,
      "http://purl.org/dc/terms/identifier",
      dclist$identifier
    ))
  }

  if (!is.null(dclist$subject)) {
    dctriples <- c(dctriples, n_triple(
      dataset_id,
      "http://purl.org/dc/terms/subject",
      dclist$subject
    ))
  }

  if (!is.null(dclist$type)) {
    dctriples <- c(dctriples, n_triple(
      dataset_id,
      "http://purl.org/dc/terms/type",
      gsub("DCMITYPE:", "http://purl.org/dc/terms/DCMIType", dclist$type)
    ))
  }

  if (!is.null(dclist$contributor)) {
    dctriples <- c(dctriples, n_triple(
      dataset_id,
      "http://purl.org/dc/terms/contributor",
      dclist$contributor
    ))
  }

  # if ( !is.null(dclist$date) ) {
  #  dctriples <- c(dctriples, n_triple(dataset_id,
  #                                     "http://purl.org/dc/terms/date",
  #                                     dclist$date))
  # }

  if (!is.null(dclist$language)) {
    dctriples <- c(dctriples, n_triple(
      dataset_id,
      "http://purl.org/dc/terms/language",
      dclist$language
    ))
  }

  if (!is.null(dclist$datasource)) {
    dctriples <- c(dctriples, n_triple(
      dataset_id,
      "http://purl.org/dc/terms/source",
      dclist$datasource
    ))
  }

  if (!is.null(dclist$coverage)) {
    dctriples <- c(dctriples, n_triple(
      dataset_id,
      "http://purl.org/dc/terms/coverage",
      dclist$coverage
    ))
  }
  n_triples(dctriples)
}




#' @keywords internal
new_dublincore <- function(title,
                           creator,
                           identifier = NULL,
                           publisher = NULL,
                           subject = NULL,
                           type = "DCMITYPE:Dataset",
                           contributor = NULL,
                           dataset_date = NULL,
                           year = NULL,
                           language = NULL,
                           relation = NULL,
                           format = NULL,
                           rights = NULL,
                           datasource = NULL,
                           description = NULL,
                           coverage = NULL) {

  year <- if (!is.null(dataset_date)) {
    substr(as.character(dataset_date), 1, 4)
    } else NULL

  dublincore_object <- bibrecord(
    title = title,
    author = creator,
    identifier = identifier,
    publisher = publisher,
    subject = subject,
    type = type,
    contributor = contributor,
    date = dataset_date,
    year = year,
    language = language,
    relation = relation,
    format = format,
    rights = rights,
    datasource = datasource,
    description = description,
    coverage = coverage
  )
  class(dublincore_object) <- c("dublincore", class(dublincore_object))
  dublincore_object
}

#' @rdname dublincore
is.dublincore <- function(x) {
  UseMethod("is.dublincore", x)
}

#' @rdname dublincore
#' @param x An object that is tested if it has a class "dublincore".
#' @return A logical value, if the bibliographic entries are listed
#' according to the Dublin Core specification.
#' @exportS3Method
is.dublincore.dublincore <- function(x) inherits(x, "dublincore")


#' @exportS3Method
print.dublincore <- function(x, ...) {
  cat("Dublin Core Metadata Record\n")
  cat("--------------------------\n")
  cat("Title:       ", x$title, "\n")
  cat("Creator(s):  ", paste(format(x$author), collapse = "; "), "\n")

  contributor <- attr(x, "contributor")
  if (!is.null(contributor)) {
    cat("Contributor(s): ", fix_contributor(contributor), "\n")
  }

  if (!is.null(x$publisher)) cat("Publisher:   ", x$publisher, "\n")
  if (!is.null(x$year)) cat("Year:        ", x$year, "\n")
  if (!is.null(x$language)) cat("Language:    ", x$language, "\n")
  if (!is.null(x$description)) cat("Description: ", x$description, "\n")

  invisible(x)
}
