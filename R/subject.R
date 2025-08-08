#' @title Create, add, or retrieve a subject
#'
#' @description
#' Manage the subject metadata of a dataset. The subject can be a simple
#' character term or a structured object with subproperties created by
#' [subject_create()].
#'
#' @details
#' The [DataCite subject property](https://schema.datacite.org/meta/kernel-4/)
#' supports multiple subproperties. These cannot be stored directly in a
#' standard [`utils::bibentry`] object.
#' Therefore:
#'
#' * If you set a character string as the subject, it is stored in the bibentry
#'   and as a `"subject"` attribute.
#' * If you set a structured subject (via [subject_create()]), the `$term`
#'   value is stored in the bibentry, and the complete object is stored in the
#'   `"subject"` attribute of the [`dataset_df`] object.
#'
#' @param x A dataset object created with [dataset_df()] or [as_dataset_df()].
#'
#' @return
#' * `subject(x)` returns the `"subject"` attribute or the `subject` field
#'   from the dataset's bibentry.
#' * `subject(x) <- value` sets the `"subject"` attribute and updates the
#'   bibentry's `subject` field, returning the modified dataset invisibly.
#'
#' @examples
#' df <- dataset_df(data.frame(x = 1))
#'
#' # Set subject as a structured object
#' subject(df) <- subject_create(
#'   term = "Oranges",
#'   schemeURI = "http://id.loc.gov/authorities/subjects",
#'   valueURI = "http://id.loc.gov/authorities/subjects/sh85095257",
#'   subjectScheme = "LCCH",
#'   prefix = "lcch:"
#' )
#'
#' # Retrieve subject
#' subject(df)
#'
#' @export
#' @family bibliographic helper functions
#' @importFrom assertthat assert_that
subject <- function(x) {
  assertthat::assert_that(
    is.dataset_df(x),
    msg = "subject(x): x must be a dataset_df object created with dataset_df() or as_dataset_df()."
  )

  if ("subject" %in% names(attributes(x))) {
    attr(x, "subject")
  } else if (!is.null(get_bibentry(x)$subject)) {
    get_bibentry(x)$subject
  } else {
    message("No subject is recorded.")
    NULL
  }
}

#' @rdname subject
subject_create <- function(term,
                           schemeURI = NULL,
                           valueURI = NULL,
                           prefix = NULL,
                           subjectScheme = NULL,
                           classificationCode = NULL) {
  if (is.null(term)) term <- ":tba"

  # if (! all.equal(length(heading), length(subjectScheme))) {
  #  stop("You must provide exactly one subjectSchemes, URIs and Codes for each heading.")
  # }

  if (length(term) > 1) {
    dataset_subject <- lapply(seq_along(term), function(x) {
      new_Subject(term[x],
        subjectScheme = subjectScheme[x],
        schemeURI = schemeURI[x],
        classificationCode = classificationCode[x],
        prefix = prefix[x]
      )
    })
    # this is not nice
    class(dataset_subject) <- c("subject", class(subject))
  } else {
    dataset_subject <- new_Subject(
      term = term,
      subjectScheme = subjectScheme,
      schemeURI = schemeURI,
      valueURI = valueURI,
      classificationCode = classificationCode,
      prefix = prefix
    )
  }

  dataset_subject
}

#' @inheritParams Subject
#' @keywords internal
new_Subject <- function(term,
                        schemeURI = NULL,
                        valueURI = NULL,
                        prefix = NULL,
                        subjectScheme = NULL,
                        classificationCode = NULL) {
  if (is.null(subjectScheme)) subjectScheme <- ""
  if (is.null(schemeURI)) schemeURI <- ""
  if (is.null(valueURI)) valueURI <- ""
  if (is.null(prefix)) prefix <- ""

  if (!is.null(classificationCode)) {
    dataset_subject <- list(
      term = term,
      subjectScheme = subjectScheme,
      schemeURI = schemeURI,
      classificationCode = classificationCode,
      prefix = prefix
    )
  } else {
    dataset_subject <- list(
      term = term,
      subjectScheme = subjectScheme,
      schemeURI = schemeURI,
      valueURI = valueURI,
      classificationCode = classificationCode,
      prefix = prefix
    )
  }

  class(dataset_subject) <- c("subject", class(dataset_subject))

  dataset_subject
}

#' @rdname subject
#' @export
`subject<-` <- function(x, value) {
  assert_that(is.dataset_df(x),
    msg = "subject<-(x, value): x must be a dataset object created with dataset_df() or as_dataset_df()."
  )

  ds_bibentry <- get_bibentry(x)

  if (is.null(value)) {
    value <- new_Subject(term = ":tba")
  } else if (is.character(value)) {
    value <- new_Subject(term = value)
  } else if (!is.subject(value)) {
    stop("subject(x, value)<- : value must be a created with 'subject_create()` or it must be a character string.")
  }

  ds_bibentry$subject <- ifelse(is.character(value), value, value$term)
  attr(x, "dataset_bibentry") <- ds_bibentry
  attr(x, "subject") <- value
  invisible(x)
}

#' @rdname subject
is.subject <- function(x) {
  ifelse(inherits(x, "subject"), TRUE, FALSE)
}

#' @keywords internal
default_subject <- subject_create(
  term = "Data sets",
  subjectScheme = "LCSH",
  schemeURI = "http://id.loc.gov/authorities/subjects",
  valueURI = "http://id.loc.gov/authorities/subjects/sh2018002256",
  prefix = "lcsh:"
)
