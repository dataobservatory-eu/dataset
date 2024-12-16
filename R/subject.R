#' @title Create/add/retrieve a subject
#' @param x A dataset object created with \code{dataset::\link{dataset_df}} or
#' \code{dataset::\link{as_dataset_df}}.
#' @examples
#' subject(iris_dataset,
#'         overwrite = TRUE) <- subject_create(
#'                                   term  = "Irises (plants)",
#'                                   schemeURI = "http://id.loc.gov/authorities/subjects",
#'                                   valueURI = "https://id.loc.gov/authorities/subjects/sh85068079",
#'                                   subjectScheme = "LCCH",
#'                                   prefix = "lcch:")
#' subject(iris_dataset)
#' @export
#' @rdname subject

#' @return The Subject field of the dataset's attributes.
subject <- function(x) {
  assert_that(is.dataset_df(x),
              msg = "subject(x): x must be a dataset object created with dataset() or as_dataset().")

  if("Subject" %in% names(attributes(x)))  {
    attr(x, "Subject")
  } else if(!is.null(get_bibentry(x)$subject)) {
    as.character(get_bibentry(x)$subject)
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
#' @return A named list with the subject term, the subject scheme, URIs and prefix.
#' @export
subject_create <- function(term,
                           schemeURI = NULL,
                           valueURI = NULL,
                           prefix = NULL,
                           subjectScheme = NULL,
                           classificationCode = NULL ) {

  if (is.null(term)) term <- ":tba"

  #if (! all.equal(length(heading), length(subjectScheme))) {
  #  stop("You must provide exactly one subjectSchemes, URIs and Codes for each heading.")
  #}

  if (length(term)>1) {
    dataset_subject <-  lapply (1:length(term), function(x) new_Subject(term[x],
                                                                subjectScheme = subjectScheme[x],
                                                                schemeURI = schemeURI[x],
                                                                classificationCode = classificationCode[x],
                                                                prefix = prefix[x])
    )
    # this is not nice
    class(dataset_subject) <- c("subject", class(subject))
  } else {
    dataset_subject <- new_Subject(term=term,
                                   subjectScheme=subjectScheme,
                                   schemeURI=schemeURI,
                                   valueURI=valueURI,
                                   classificationCode = classificationCode,
                                   prefix = prefix)
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
                        classificationCode = NULL)
 {

  if (is.null(subjectScheme)) subjectScheme <- ""
  if (is.null(schemeURI)) schemeURI <- ""
  if (is.null(valueURI)) valueURI <- ""
  if (is.null(prefix)) prefix <- ""

  if (!is.null(classificationCode)) {
    dataset_subject <- list ( term = term,
                              subjectScheme = subjectScheme,
                              schemeURI = schemeURI,
                              classificationCode = classificationCode,
                              prefix = prefix)
  } else {
    dataset_subject  <- list ( term = term,
                               subjectScheme = subjectScheme,
                               schemeURI = schemeURI,
                               valueURI = valueURI,
                               classificationCode = classificationCode,
                               prefix=prefix)
  }

  class(dataset_subject ) <- c("subject", class(subject))

  dataset_subject
}

#' @rdname subject
#' @param value A subject field created by \code{\link{subject}}.
#' @param overwrite If the attributes should be overwritten. In case it is set to \code{FALSE},
#' it gives a message with the current \code{Subject} property instead of overwriting it.
#' Defaults to \code{FALSE}.
#' @export
`subject<-` <- function(x, overwrite =FALSE, value) {

  if (is.character(value)) {
    value <- list(term=value)
  }

  if (is.null(value$subjectScheme)) value$subjectScheme <- ""
  if (is.null(value$schemeURI)) value$schemeURI <- ""
  if (is.null(value$valueURI)) value$valueURI <- ""
  if (is.null(value$prefix)) value$prefix <- ""

  if(!is.subject(value)) {
    # it is a conforming list but not a subject class
    dataset_subject <- subject_create(term = value$term,
                                      schemeURI = value$schemeURI,
                                      valueURI = value$valueURI,
                                      prefix = value$prefix,
                                      subjectScheme = value$subjectScheme,
                                      classificationCode = value$classificationCode )
  } else {
   dataset_subject <- value
  }


  attr(x, "Subject") <- dataset_subject

  invisible(x)
}

#' @rdname subject
is.subject <- function(x) {
  ifelse(inherits(x, "subject"), TRUE, FALSE)
}

