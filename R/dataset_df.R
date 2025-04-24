#' @title Create a new dataset_df object
#' @description
#' The \code{dataset_df} constructor creates the objects of this class, which
#' are semantically rich, modern data frames inherited from
#'  \code{\link[tibble:tibble]{tibble::tibble}}.
#' @details
#' To check if an object has the class dataset_df use \code{is.dataset_df}.\cr
#' \cr
#' \code{print} is the method to print out the semantically rich data frames
#' created with the constructor of \code{dataset_df}.\cr
#' \cr
#' \code{summary} is the method to summarise these semantically rich data frames.\cr
#' \cr
#' For more details, please check the \code{vignette("dataset_df", package = "dataset")}
#' vignette.
#' @param identifier Defaults to \code{c(eg="http://example.com/dataset#")}, which should be
#' changed to the permanent identifier of the dataset. For example, if your dataset will be
#' released with the Digital Object Identifier (DOI) `https;//doi.org/1234`, you should use
#' a short prefixed identifier like \code{c(obs="https://doi.org/1234#")}, which will resolve
#' to the rows being identified as https://doi.org/1234#1...https://doi.org/1234#n.
#' @param dataset_bibentry A list of bibliographic references and descriptive metadata
#' about the dataset as a whole created with \code{\link{datacite}} or
#' \code{\link{dublincore}}.
#' @param var_labels The long, human readable labels of each variable.
#' @param units The units of measurement for the measured variables.
#' @param definitions The linked definitions of the variables, attributes, or constants.
#' @param dataset_subject The subject of the dataset, see \code{\link{subject}}.
#' @param ... The vectors (variables) that should be included in the dataset.
#' @param x A \code{dataset_df} object for S3 methods.
#' @param df A \code{data.frame} to be converted to \code{dataset_df}.
#' @return \code{dataset_df} is the constructor of this type, it returns an object
#' inherited from a data frame with semantically rich metadata.
#' @import vctrs
#' @import pillar
#' @examples
#' my_dataset <- dataset_df(
#'   country_name = defined(
#'     c("AD", "LI"),
#'     definition = "http://data.europa.eu/bna/c_6c2bb82d",
#'     namespace = "https://www.geonames.org/countries/$1/"
#'   ),
#'   gdp = defined(
#'     c(3897, 7365),
#'     label = "Gross Domestic Product",
#'     unit = "million dollars",
#'     definition = "http://data.europa.eu/83i/aa/GDP"
#'   )
#' )
#'
#' print(my_dataset)
#'
#' is.dataset_df(my_dataset)
#' @export

# User constructor
dataset_df <- function(...,
                       identifier = c(eg = "http://example.com/dataset#"),
                       var_labels = NULL,
                       units = NULL,
                       definitions = NULL,
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
    dataset_bibentry <- datacite(Title = Title, Creator = Creator, Subject = dataset_subject)
  }

  tmp <- new_my_tibble(
    x = tibble::tibble(...),
    identifier = identifier,
    dataset_bibentry = dataset_bibentry,
    var_labels = var_labels,
    units = units,
    definitions = definitions
  )
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
                          definitions = NULL,
                          dataset_bibentry = NULL,
                          dataset_subject = NULL, ...) {
  dots <- list(...)

  if (is.null(dots$dataset_bibentry)) {
    dataset_bibentry <- set_default_bibentry()
  }

  new_my_tibble(df,
    identifier = identifier,
    dataset_bibentry = dataset_bibentry,
    var_labels = var_labels,
    units = units,
    definitions = definitions
  )
}

# Developer constructor
#' @importFrom tibble new_tibble
#' @keywords internal
new_my_tibble <- function(x,
                          add_rowid = TRUE,
                          identifier,
                          dataset_bibentry = NULL,
                          var_labels = NULL,
                          units = NULL,
                          definitions = NULL) {
  assertthat::assert_that(is.data.frame(x),
    msg = "Error: new_my_tibble(x): x is not a data frame"
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
    tmp$rowid <- defined(paste0(prefix, tmp$rowid), namespace = identifier)
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

  # Retrieve metadata
  year <- dataset_bibentry$year
  if (is.null(year)) {
    year <- substr(dataset_bibentry$date, 1, 4)
  }

  # Generate the tibble-like format
  df_fmt <- format(x)
  table_header <- df_fmt[1]
  table_body <- df_fmt[-1]

  # Extract column header line
  col_line <- table_body[1]

  # Detect column start positions and widths
  col_starts <- gregexpr("\\S+", col_line)[[1]]
  col_ends <- c(col_starts[-1] - 1, nchar(col_line))
  col_widths <- col_ends - col_starts + 1

  # Safely extract var_labels
  labels <- vapply(x, function(col) {
    lbl <- var_label(col)
    if (is.null(lbl)) "" else lbl
  }, character(1))

  # Format each label to match column width
  label_row_parts <- mapply(function(label, width) {
    if (!nzchar(label)) {
      strrep(" ", width)
    } else if (nchar(label) > width) {
      substr(label, 1, width)
    } else {
      format(label, width = width, justify = "centre")
    }
  }, labels, col_widths, USE.NAMES = FALSE)

  # Build a character vector and insert parts by column position
  label_row <- rep(" ", nchar(col_line))
  for (i in seq_along(col_starts)) {
    start <- col_starts[i]
    end <- col_ends[i]
    label_chars <- strsplit(label_row_parts[i], "")[[1]]
    label_row[start:end] <- label_chars[seq_len(end - start + 1)]
  }
  label_row_str <- paste0(label_row, collapse = "")

  # Print citation
  print(dataset_bibentry, "text")

  # Print column header, label row, and table body
  cat(col_line, "\n")
  cat(label_row_str, "\n")
  cat(paste0(table_body[-1], collapse = "\n"), "\n")

  invisible(x)
}


#' @importFrom vctrs df_list
#' @export
# dataset_df <- function(...) {
#  data <- df_list(...)
#  new_dataset(data)
# }

#' @export
tbl_sum.dataset_df <- function(x, ...) {
  NextMethod()
}

#' @export
summary.dataset_df <- function(object, ...) {
  print(get_bibentry(object), "text")
  NextMethod()
}

#' @rdname dataset_df
#' @export
is_dataset_df <- function(x) {
  inherits(x, "dataset_df")
}

#' @keywords internal
#' @importFrom rlang caller_env env_is_user_facing
names.dataset_df <- function(x) {
  should_inform <- rlang::env_is_user_facing(rlang::caller_env())
  # if (should_inform) {
  #  cli::cli_inform(c(
  #    `!` = "The {.fn names} method of {.cls dataset_df} is for internal use only.",
  #    i = "Did you mean {.fn colnames}?"
  ##  ))
  # }
  NextMethod("names")
}


# `[[.dataset_df` <- function(x, i, j, ..., exact = TRUE) {
#  NextMethod()
#  }
