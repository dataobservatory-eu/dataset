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
#'    country_name = defined(
#'      c("AD", "LI"),
#'      definition = "http://data.europa.eu/bna/c_6c2bb82d",
#'      namespace = "https://www.geonames.org/countries/$1/"),
#'    gdp = defined(
#'      c(3897, 7365),
#'      label = "Gross Domestic Product",
#'      unit = "million dollars",
#'      definition = "http://data.europa.eu/83i/aa/GDP")
#' )
#'
#' print(my_dataset)
#'
#' is.dataset_df(my_dataset)
#' @export

# User constructor
dataset_df <- function(...,
                       dataset_bibentry=NULL,
                       var_labels=NULL,
                       units=NULL,
                       definitions=NULL,
                       dataset_subject=NULL ) {

  dots <- list(...)

  sys_time <- Sys.time()
  year <- substr(as.character(sys_time),1,4)

  if (is.null(dataset_subject)) {
    dataset_subject <- subject_create(term="data sets",
                              subjectScheme="Library of Congress Subject Headings (LCSH)",
                              schemeURI="https://id.loc.gov/authorities/subjects.html",
                              valueURI="http://id.loc.gov/authorities/subjects/sh2018002256",
                              classificationCode = NULL,
                              prefix = ""
    )
  }

  if (is.null(dataset_bibentry)) {
    Title   <- "Untitled Dataset"
    Creator <- person("Author", "Unknown")
    dataset_bibentry <- datacite(Title=Title, Creator=Creator, Subject=dataset_subject)
  } else if(is.null(dataset_bibentry$subject)) {
      #dataset_bibentry$subject <- dataset_subject$term
   }

  tmp <- new_my_tibble(
                x = tibble::tibble(...),
                dataset_bibentry=dataset_bibentry,
                var_labels = var_labels,
                units = units,
                definitions = definitions)

  attr(tmp, "subject") <- dataset_subject
  tmp
}


#' @rdname dataset_df
#' @export
as_dataset_df <- function(df,
                          var_labels=NULL,
                          units=NULL,
                          definitions =NULL,
                          dataset_bibentry=NULL,
                          dataset_subject=NULL,  ...) {

  dots <- list(...)

  sys_time <- Sys.time()
  year <- substr(as.character(sys_time),1,4)

  if (is.null(dots$dataset_bibentry)) {
    Title <- "Untitled Dataset"
    Creator <- person("Author", "Unknown")

    if(is.null(dataset_bibentry$year)) dataset_bibentry$year <- year
  }

  dataset_bibentry <- datacite(Title=Title, Creator=Creator)

  new_my_tibble(df,
                dataset_bibentry=dataset_bibentry,
                var_labels = var_labels,
                units = units,
                definitions = definitions)
}

# Developer constructor
#' @importFrom tibble new_tibble
#' @keywords internal
new_my_tibble <- function(x,
                          dataset_bibentry = NULL,
                          var_labels = NULL,
                          units = NULL,
                          definitions = NULL) {
  started_at_time <- Sys.time()
  assertthat::assert_that(is.data.frame(x),
                          msg="Error: new_my_tibble(x): x is not a data frame")

  tmp <- tibble::new_tibble(
    x,
    class = "dataset_df",
    nrow = nrow(x)
  )

  ended_at_time <- Sys.time()

  set_var_labels(tmp, var_labels = var_labels)

  prov <- default_provenance(started_at_time = started_at_time, ended_at_time = ended_at_time)

  attr(tmp, "dataset_bibentry") <- dataset_bibentry
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
#' @importFrom cli cat_line
#' @export
print.dataset_df <- function(x, ...) {

  dataset_bibentry <- get_bibentry(x)

  author_person <- dataset_bibentry$author
  year          <- dataset_bibentry$year
  title         <- dataset_bibentry$title

  if (inherits(author_person, "persont")) {
    print_name <- ""
    if (!is.null(author_person$family)) print_name <- paste0(author_person$family, ", ")
    if (!is.null(author_person$given))  print_name <- paste0(print_name, author_person$given, ": ")
  } else if (is.character(attr(x, "person"))) {
    print_name <- paste0(attr(x, "person"), ": ")
  } else { print_name <- ""}

  #if(!is.null(title)) {
  #  print_title <- title
  #} else { print_title: "A dataset"}

  #cat(print_name)
  #cat(print_title)
  if (!is.null(year)) {
    #cat(paste0(" (", substr(as.character(year), 1,4), ")"))
  }
  print(get_bibentry(x), "text")
  vr_labels <- vapply(attr(x, "var_labels"), function(x) x, character(1))
  cli::cat_line(format(x)[-1])
}

#' @importFrom vctrs df_list
#' @export
#dataset_df <- function(...) {
#  data <- df_list(...)
#  new_dataset(data)
#}

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
  #if (should_inform) {
  #  cli::cli_inform(c(
  #    `!` = "The {.fn names} method of {.cls dataset_df} is for internal use only.",
  #    i = "Did you mean {.fn colnames}?"
  ##  ))
  #}
  NextMethod("names")
}


#`[[.dataset_df` <- function(x, i, j, ..., exact = TRUE) {
#  NextMethod()
#  }
