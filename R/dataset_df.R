#' @title Create a new dataset_df object
#' @description The \code{dataset_df} constructor creates the objects of this
#' class, which are semantically rich, modern data frames inherited from
#' \code{\link[tibble:tibble]{tibble::tibble}}.
#' @details To check if an object has the class dataset_df use
#' \code{is.dataset_df}.\cr \cr \code{print} is the method to print out the
#' semantically rich data frames created with the constructor of
#' \code{dataset_df}.\cr \cr \code{summary} is the method to summarise these
#' semantically rich data frames.\cr \cr
#' For more details, please check the \code{vignette("dataset_df",
#' package = "dataset")}
#' vignette.
#' @param identifier Defaults to \code{c(eg="http://example.com/dataset#")},
#'   which should be changed to the permanent identifier of the dataset. For
#'   example, if your dataset will be released with the Digital Object
#'   Identifier (DOI) `https;//doi.org/1234`, you should use a short prefixed
#'   identifier like \code{c(obs="https://doi.org/1234#")}, which will resolve
#'   to the rows being identified as
#'   https://doi.org/1234#1...https://doi.org/1234#n.
#' @param dataset_bibentry A list of bibliographic references and descriptive
#'   metadata about the dataset as a whole created with \code{\link{datacite}}
#'   or \code{\link{dublincore}}.
#' @param var_labels The long, human readable labels of each variable.
#' @param units The units of measurement for the measured variables.
#' @param concepts The linked concepts of the variables, attributes, or
#'   constants.
#' @param dataset_subject The subject of the dataset, see \code{\link{subject}}.
#' @param ... The vectors (variables) that should be included in the dataset.
#' @param x A \code{dataset_df} object for S3 methods.
#' @param df A \code{data.frame} to be converted to \code{dataset_df}.
#' @return \code{dataset_df} is the constructor of this type, it returns an
#'   object inherited from a data frame with semantically rich metadata.
#' @import vctrs
#' @import pillar
#' @examples
#' my_dataset <- dataset_df(
#'   country_name = defined(
#'     c("AD", "LI"),
#'     concept = "http://data.europa.eu/bna/c_6c2bb82d",
#'     namespace = "https://www.geonames.org/countries/$1/"
#'   ),
#'   gdp = defined(
#'     c(3897, 7365),
#'     label = "Gross Domestic Product",
#'     unit = "million dollars",
#'     concept = "http://data.europa.eu/83i/aa/GDP"
#'   ),
#'   dataset_bibentry =  dublincore(
#'     title = "GDP of Andorra And Lichtenstein",
#'     description = "A small but semantically rich datset example.",
#'     creator = person("Jane", "Doe", role = "cre"),
#'     publisher = "Open Data Institute",
#'     language = "en")
#'  )
#'
#' # Use standard methods, like print, summary, head, tail
#' print(my_dataset)
#' head(my_dataset)
#' tail(my_dataset)
#'
#' # Check class:
#' is.dataset_df(my_dataset)
#'
#' # To check the bibliographic metadata of a dataset,
#' # use as_dublincore for DCTERMS:
#' as_dublincore(my_dataset)
#'
#' # ... and as_datacite for DataCite:
#' as_datacite(my_dataset)
#' @export

# User constructor
dataset_df <- function(...,
                       identifier = c(eg = "http://example.com/dataset#"),
                       var_labels = NULL,
                       units = NULL,
                       concepts = NULL,
                       dataset_bibentry = NULL,
                       dataset_subject = NULL) {
  dots <- list(...)

  if (!"rowid" %in% names(dots)) {
    add_rowid <- TRUE
  } else {
    add_row_id <- FALSE
  }

  sys_time <- Sys.time()
  year <- substr(as.character(sys_time), 1, 4)

  if (is.null(dataset_subject)) {
    dataset_subject <- subject_create(
      term = "data sets",
      subjectScheme = "Library of Congress Subject Headings (LCSH)",
      schemeURI = "https://id.loc.gov/authorities/subjects.html",
      valueURI = "http://id.loc.gov/authorities/subjects/sh2018002256",
      classificationCode = NULL,
      prefix = ""
    )
  }

  if (is.null(dataset_bibentry)) {
    Title <- "Untitled Dataset"
    Creator <- person("Author", "Unknown")
    dataset_bibentry <- datacite(Title = Title,
                                 Creator = Creator,
                                 Subject = dataset_subject,
                                 Date = Sys.Date())
  }

  tmp <- new_dataset(
    x = tibble::tibble(...),
    identifier = identifier,
    dataset_bibentry = dataset_bibentry,
    var_labels = var_labels,
    units = units,
    concepts = concepts
  )

  dataset_bibentry <- get_bibentry(tmp)
  if ( dataset_bibentry$year == ":tba" ) dataset_bibentry$year <- year
  if ( dataset_bibentry$date == ":tba" ) {
    dataset_bibentry$date <- as.character(Sys.Date())
    }

  attr(tmp, "dataset_bibentry") <- dataset_bibentry
  attr(tmp, "subject") <- dataset_subject
  tmp
}

#' @rdname dataset_df
#' @export
as_dataset_df <- function(df,
                          identifier = c(eg = "http://example.com/dataset#"),
                          var_labels = NULL,
                          units = NULL,
                          concepts = NULL,
                          dataset_bibentry = NULL,
                          dataset_subject = NULL, ...) {
  dots <- list(...)

  if (is.null(dots$dataset_bibentry)) {
    dataset_bibentry <- set_default_bibentry()
  }

  new_dataset(df,
    identifier = identifier,
    dataset_bibentry = dataset_bibentry,
    var_labels = var_labels,
    units = units,
    concepts = concepts
  )
}

# Developer constructor
#' @importFrom tibble new_tibble
#' @keywords internal
new_dataset <- function(x,
                        add_rowid = TRUE,
                        identifier,
                        dataset_bibentry = NULL,
                        var_labels = NULL,
                        units = NULL,
                        concepts = NULL) {
  assertthat::assert_that(is.data.frame(x),
    msg = "Error: new_dataset(x): x is not a data frame"
  )

  generated_at_time <- Sys.time()

  tmp <- tibble::new_tibble(
    x,
    class = "dataset_df",
    nrow = nrow(x)
  )

  add_rowid <- ifelse("rowid" %in% names(tmp), FALSE, TRUE)

  if (add_rowid) {
    tmp <- tibble::rowid_to_column(tmp)
    prefix <- paste0(names(identifier)[1], ":")
    tmp$rowid <- defined(paste0(prefix, tmp$rowid),
                         namespace = identifier)
  }

  if (is.null(dataset_bibentry)) {
    dataset_bibentry <- set_default_bibentry()
  }

  attr(tmp, "dataset_bibentry") <- dataset_bibentry

  # tmp <- set_var_labels(tmp, var_labels = var_labels)

  prov <- default_provenance(
    generated_at_time = generated_at_time,
    author = dataset_bibentry$author
  )

  attr(tmp, "prov") <- prov

  tmp
}
#' @rdname dataset_df
#' @return \code{is.dataset_df} returns a logical value
#' (if the object is of class \code{dataset_df}.)
#' @export
is.dataset_df <- function(x) {
  ifelse("dataset_df" %in% class(x), TRUE, FALSE)
}

#' @rdname dataset_df
#' @export
print.dataset_df <- function(x, ...) {

  dataset_bibentry <- get_bibentry(x)
  if (is.null(dataset_bibentry)) {
    dataset_bibentry <- set_default_bibentry()
  }

  # Extract fields
  authors <- dataset_bibentry$author
  year  <- dataset_bibentry$year
  title <- dataset_bibentry$title
  doi <- dataset_bibentry$identifier
  dataset_date <- dataset_bibentry$Date

  # Format author(s)
  author_fmt <- function(authors) {
    if (length(authors) == 1) {
      return(authors[[1]]$family %||% format(authors[[1]]))
    }

    is_institutional <- vapply(authors,
                               function(a) is.null(a$given) && !is.null(a$family),
                               logical(1))
    if (all(is_institutional)) {
      return(paste(vapply(authors,
                          function(a) a$family, character(1)),
                   collapse = "-"))
    }

    if (length(authors) == 2) {
      return(paste(vapply(authors,
                          function(a) a$family,
                          character(1)), collapse = "-"))
    }

    return(paste0(authors[[1]]$family, " et al."))
  }

  apa_header <- sprintf("%s (%s): %s [dataset]",
                        author_fmt(authors),
                        year,
                        title)

  if ( ! is.null(doi) && grepl("doi.org", doi)) {
    apa_header <- paste0(apa_header, ", ", doi)
  }

  cat(trimws(apa_header), "\n", sep = "")

  # Generate the tibble-like format
  df_fmt <- format(x)
  table_header <- df_fmt[1]
  table_body <- df_fmt[-1]

  # Extract column header line
  col_line <- table_body[1]

  # Print column header, label row, and table body
  cat(col_line, "\n")
  cat(paste0(table_body[-1], collapse = "\n"), "\n")

  invisible(x)
}


#' @export
tbl_sum.dataset_df <- function(x, ...) {
  NextMethod()
}

#' @export
summary.dataset_df <- function(object, ...) {

  dataset_bibentry <- get_bibentry(object)
  if (is.null(dataset_bibentry)) {
    dataset_bibentry <- set_default_bibentry()
  }
  # Extract fields
  authors <- dataset_bibentry$author
  year  <- dataset_bibentry$year
  title <- dataset_bibentry$title
  doi <- dataset_bibentry$identifier

  # Format author(s)
  author_fmt <- function(authors) {
    if (length(authors) == 1) {
      return(authors[[1]]$family %||% format(authors[[1]]))
    }

    is_institutional <- vapply(authors,
                               function(a) is.null(a$given) && !is.null(a$family),
                               logical(1))
    if (all(is_institutional)) {
      return(paste(vapply(authors,
                          function(a) a$family, character(1)),
                   collapse = "-"))
    }

    if (length(authors) == 2) {
      return(paste(vapply(authors,
                          function(a) a$family,
                          character(1)), collapse = "-"))
    }

    return(paste0(authors[[1]]$family, " et al."))
  }

  apa_header <- sprintf("%s (%s): Summary of %s [dataset]",
                        author_fmt(authors),
                        year,
                        title)


  if ( ! is.null(doi) && grepl("doi.org", doi)) {
    apa_header <- paste0(apa_header, ", ", doi)
  }

  cat(trimws(apa_header), "\n\n", sep = "")

  NextMethod()
}

#' @export
plot.dataset_df <- function(x, y = NULL, ..., main = NULL, sub = NULL) {
  title <- dataset_title(x)
  bib <- get_bibentry(x)
  author <- tryCatch(as.character(bib$author), error = function(e) "")
  year <- bib$year %||% substr(bib$date, 1, 4)
  publisher <- bib$publisher %||% ""

  main <- main %||% title
  sub <- sub %||% paste(author, "(", year, ")", "-", publisher)

  df <- as.data.frame(x)

  # Identify numeric or defined columns
  numeric_like_cols <- which(vapply(df, function(col) {
    inherits(col, "numeric") || inherits(col, "double") || inherits(col, "integer")
  }, logical(1)))

  if (length(numeric_like_cols) < 2) {
    stop("Not enough numeric or defined columns to create a plot.")
  }

  for (j in numeric_like_cols) {
    df[[j]] <- as_numeric(df[[j]])
  }

  xcol <- df[[numeric_like_cols[1]]]
  ycol <- df[[numeric_like_cols[2]]]

  x_label <- ifelse(is.null(var_label(x)[numeric_like_cols[1]][[1]]),
    names(x)[numeric_like_cols[1]],
    var_label(x)[numeric_like_cols[1]]
  )

  y_label <- ifelse(is.null(var_label(x)[numeric_like_cols[2]][[1]]),
    names(x)[numeric_like_cols[2]],
    var_label(x)[numeric_like_cols[2]]
  )

  plot(xcol, ycol,
    xlab = x_label,
    ylab = y_label,
    main = main,
    ...
  )
}



#' @rdname dataset_df
#' @export
is_dataset_df <- function(x) {
  inherits(x, "dataset_df")
}

#' @keywords internal
names.dataset_df <- function(x) {
  NextMethod("names")
}

#' @export
`[.dataset_df` <- function(x, i, j, drop = FALSE) {
  out <- NextMethod("[")
  attributes_to_preserve <- c("dataset_bibentry", "subject", "prov")

  for (attr_name in attributes_to_preserve) {
    attr(out, attr_name) <- attr(x, attr_name)
  }

  class(out) <- class(x)
  out
}
