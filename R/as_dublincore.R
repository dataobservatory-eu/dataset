#' @rdname dublincore
#' @param type For \code{as_dublincore}, any of \code{"bibentry", "dataset_df", "list", "ntriples"}.
#' @param ... Optional parameters to add to a \code{dublincore} object.
#' \code{author=person("Jane", "Doe")} adds an author to the citation
#' object if \code{type="dataset"}.
#' @export
as_dublincore <- function(x, type = "bibentry", ...) {
  citation_author <- person(NULL, NULL)

  is_person <- function(p) ifelse(inherits(p, "person"), TRUE, FALSE)

  arguments <- list(...)

  if (!is.null(arguments$author)) {
    if (is_person(arguments$author)) {
      citation_author <- arguments$author
    } else {
      stop("as_dublincore(x, ..., author = ): author must be created with utils::person().")
    }
  }

  if (!type %in% c("bibentry", "list", "dataset_df", "ntriples")) {
    warning(paste0(
      "as_dublincore(ds, type=...) type cannot be ",
      type, ". Reverting to 'bibentry'."
    ))
    type <- "bibentry"
  }

  dataset_bibentry <- get_bibentry(x)
  dataset_title <- dataset_bibentry$title
  dataset_creator <- dataset_bibentry$author

  if (!is_person(dataset_creator)) {
    stop('attr(x, "dataset_bibentry")$author is not a person object.')
  }

  if (!is.null(dataset_bibentry$year)) {
    if (is.null(dataset_bibentry$dataset_date)) {
      dataset_date <- as.character(dataset_bibentry$year)
    } else {
      dataset_date <- as.character(dataset_bibentry$date)
    }
  } else if (!is.null(dataset_bibentry$date)) {
    dataset_date <- dataset_bibentry$date
  } else {
    dataset_date <- ":tba"
  }

  dataset_relation <- ifelse(is.null(dataset_bibentry$relation), ":unas", as.character(dataset_bibentry$relation))
  dataset_identifier <- ifelse(is.null(dataset_bibentry$identifier), ":tba", as.character(dataset_bibentry$identifier))
  dataset_version <- ifelse(is.null(dataset_bibentry$version), ":unas", as.character(dataset_bibentry$version))
  dataset_description <- ifelse(is.null(dataset_bibentry$description), ":unas", as.character(dataset_bibentry$description))
  dataset_language <- ifelse(is.null(dataset_bibentry$language), ":unas", as.character(dataset_bibentry$language))
  dataset_format <- ifelse(is.null(dataset_bibentry$format), ":tba", as.character(dataset_bibentry$format))
  dataset_rights <- ifelse(is.null(dataset_bibentry$rights), ":tba", as.character(dataset_bibentry$rights))
  dataset_coverage <- ifelse(is.null(dataset_bibentry$coverage), ":unas", as.character(dataset_bibentry$coverage))
  datasource <- ifelse(is.null(dataset_bibentry$datasource), ":unas", as.character(dataset_bibentry$datasource))
  dataset_contributor <- ifelse(is.null(dataset_bibentry$contributor), "", as.character(dataset_bibentry$contributor))
  dataset_subject <- ifelse(is.null(dataset_bibentry$subject), "", as.character(dataset_bibentry$subject))
  dataset_publisher <- ifelse(is.null(dataset_bibentry$publisher), "", as.character(dataset_bibentry$publisher))

  properties <- c(
    length(dataset_title),
    length(as.character(dataset_creator)),
    length(dataset_identifier),
    length(dataset_publisher),
    length(dataset_subject),
    length("DCMITYPE:Dataset"),
    length(dataset_contributor),
    length(dataset_date),
    length(dataset_language),
    length(dataset_relation),
    length(dataset_format),
    length(dataset_rights),
    length(datasource),
    length(dataset_description),
    length(dataset_coverage)
  )

  if (type == "bibentry") {
    new_dublincore(
      title = dataset_title,
      creator = dataset_creator,
      identifier = dataset_identifier,
      publisher = dataset_publisher,
      subject = dataset_subject,
      type = "DCMITYPE:Dataset",
      contributor = dataset_contributor,
      dataset_date = dataset_date,
      language = dataset_language,
      relation = dataset_relation,
      format = dataset_format,
      rights = dataset_rights,
      datasource = datasource,
      description = dataset_description,
      coverage = dataset_coverage
    )
  } else if (type == "list") {
    if (dataset_contributor == "") dataset_contributor <- NULL
    if (dataset_subject == "") dataset_subject <- NULL

    if (is.null(attr(dataset_bibentry, "contributor"))) {
      dataset_contributor <- ""
    } else {
      dataset_contributor <- attr(dataset_bibentry, "contributor")
    }

    list(
      title = dataset_title,
      creator = dataset_creator,
      identifier = dataset_identifier,
      publisher = dataset_publisher,
      subject = dataset_subject,
      type = "DCMITYPE:Dataset",
      contributor = dataset_contributor,
      date = dataset_date,
      language = dataset_language,
      relation = dataset_relation,
      format = dataset_format,
      rights = dataset_rights,
      datasource = datasource,
      description = dataset_description,
      coverage = dataset_coverage
    )
  } else if (type == "dataset_df") {
    assertthat::assert_that(
      all(properties) == 1,
      msg = "In as_dublincore() not all properties have a length 1 to export into datataset (data.frame)."
    )
    dataset_df(
      data.frame(
        title = dataset_title,
        creator = as.character(dataset_creator),
        identifier = dataset_identifier,
        publisher = dataset_publisher,
        subject = dataset_subject,
        type = "DCMITYPE:Dataset",
        contributor = dataset_contributor,
        date = dataset_date,
        language = dataset_language,
        relation = dataset_relation,
        format = dataset_format,
        rights = dataset_rights,
        datasource = datasource,
        description = dataset_description,
        coverage = dataset_coverage
      ),
      reference = list(
        title = paste0("The Dublin Core Metadata of `", dataset_bibentry$title, "'"),
        author = citation_author,
        year = substr(as.character(Sys.Date()), 1, 4)
      )
    )
  } else if (type == "ntriples") {
    dataset_id <- if (is.null(dataset_identifier) || dataset_identifier == ":tba") {
      "http://example.com/dataset_tba/"
    } else {
      dataset_identifier
    }


    dataset_contributor <- attr(dataset_bibentry, "contributor")

    # Create full DC metadata list and delegate filtering/serialization
    dclist <- list(
      title       = dataset_title,
      creator     = dataset_creator,
      identifier  = dataset_identifier,
      publisher   = dataset_publisher,
      subject     = dataset_subject,
      type        = "http://purl.org/dc/dcmitype/Dataset", # Fixed URI
      contributor = dataset_contributor,
      date        = dataset_date,
      language    = dataset_language,
      relation    = dataset_relation,
      format      = dataset_format,
      rights      = dataset_rights,
      datasource  = datasource,
      description = dataset_description,
      coverage    = dataset_coverage
    )

    return(dublincore_to_triples(
      dclist = dclist,
      dataset_id = dataset_id
    ))
  }
}
