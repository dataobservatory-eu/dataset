#' @title Create a new dataset_df object
#' @details
#' To check if an object is a dataset_df use \code{is.dataset_df(x)}.
#' @param reference A list of bibliographic references and descriptive metadata
#' about the dataset as a whole.
#' @param var_labels The long, human readable labels of each variable.
#' @param units The units of measurement for the measured variables.
#' @param definitions The linked definitions of the variables, attributes, or constants.
#' @param x A dataset for S3 methods.
#' @param df A data.frame to be converted to dataset_df.
#' @param ... The vectors (variables) that should be included in the dataset.
#' @return A dataset_df object with rich metadata.
#' @import vctrs
#' @import pillar
#' @export
# User constructor
dataset_df <- function(reference=NULL, var_labels=NULL, units=NULL, definitions =NULL, ...) {

  dots <- list(...)

  sys_time <- Sys.time()
  year <- substr(as.character(sys_time),1,4)

  if(is.null(reference)) {
    reference <- list(title="Untitled Dataset",
                      author="Unknown Author")
  }

  dataset_bibentry <- create_bibentry(reference)

  new_my_tibble(tibble::tibble(...),
                dataset_bibentry=dataset_bibentry,
                var_labels = var_labels,
                units = units,
                definitions = definitions)
}


#' @rdname dataset_df
#' @export
as_dataset_df <- function(df, var_labels=NULL, units=NULL, definitions =NULL, reference=NULL, ...) {

  dots <- list(...)

  sys_time <- Sys.time()
  year <- substr(as.character(sys_time),1,4)

  if(is.null(reference)) {
    reference <- list(title="Untitled Dataset",
                      author="Unknown Author")
  }

  if(is.null(reference$year)) reference$year <- year

  reference
  dataset_bibentry <- create_bibentry(reference)

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
  stopifnot(is.data.frame(x))

  tmp <- tibble::new_tibble(
    x,
    dataset_bibentry = dataset_bibentry,
    class = "dataset_df",
    nrow = nrow(x)
  )

  ended_at_time <- Sys.time()

  set_var_labels(tmp, var_labels = var_labels)

  prov <- default_provenance(started_at_time = started_at_time, ended_at_time = ended_at_time)

  attr(tmp, "prov") <- prov

  tmp
}



#' @rdname dataset_df
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
  } else { print_name = ""}

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
