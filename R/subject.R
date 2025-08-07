#' @title Create/add/retrieve a subject
#' @details
#' The subject class and its function record the subject property of the dataset.
#' The DataCite definition allows the use of multiple subproperties, however, these
#' cannot be added to the standard \code{\link[utils:bibentry]{utils::bibentry}}
#' object. Therefore, if the user sets the value of the subject field to a
#' character string, it is added to the bibentry of the dataset, and also to
#' a separate \code{subject} attribute. If the user wants to use the more detailed
#' subproperties (see examples with \code{subject_create}), then the subject$term
#' value is added to the bibentry as a text, and the more complex subject object
#' is added as a separate attribute to the dataset_df object.
#' @param x A dataset object created with [dataset_df()] or
#' [as_dataset_df()].
#' @examples
#' # To set the subject of a dataset_df object:
#' subject(orange_df) <- subject_create(
#'   term = "Oranges",
#'   schemeURI = "http://id.loc.gov/authorities/subjects",
#'   valueURI = "http://id.loc.gov/authorities/subjects/sh85095257",
#'   subjectScheme = "LCCH",
#'   prefix = "lcch:"
#' )
#'
#' # To retrieve the subject with its subproperties:
#' subject(orange_df)
#' @export
#' @return \code{subject(x)} returns the subject attribute of the
#' [dataset_df()] object \code{x}, \code{subject(x)<-value} sets
#' the same attribute to \code{value} and invisibly returns the
#' \code{x} object with the changed attributes.
#' @importFrom assertthat assert_that
#' @rdname subject
subject <- function(x) {
  assert_that(is.dataset_df(x),
    msg = "subject(x): x must be a dataset_df object created with dataset_df() or as_dataset_df()."
  )

  if ("subject" %in% names(attributes(x))) {
    attr(x, "subject")
  } else if (!is.null(get_bibentry(x)$subject)) {
    get_bibentry(x)$subject
  } else {
    message("No subject is recorded.")
  }
}

#' @rdname subject
#' @param term A subject term, for example, \code{"Data sets"}.
#' @param subjectScheme The name of the subject scheme or classification code or
#' authority if one is used. It is a namespace.
#' @param schemeURI The URI of the subject identifier scheme, for example
#' \code{"http://id.loc.gov/authorities/subjects"}
#' @param valueURI The URI of the subject term.
#' \code{"https://id.loc.gov/authorities/subjects/sh2018002256"}
#' @param prefix An abbreviated prefix of a scheme URI, for example,
#' \code{"lcch:"} representing \code{"http://id.loc.gov/authorities/subjects"}.
#' Widely used namespaces (schemes) have conventional abbreviations.
#' @param classificationCode The classificationCode subproperty may be used for
#' subject schemes, like ANZSRC, which do not have valueURIs for each subject term.
#' @return A \code{subject_create} returns a named list with the subject term,
#' the subject scheme, URIs and prefix.
#' @export
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
#' @param value A subject field created by[subject()].
#' The subject field is overwritten with this value.
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
#' @return \code{is.subject} returns a logical value, \code{TRUE} if the subject as a list
#' is well-formatted by[subject_create()] with its necessary key-value pairs.
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
