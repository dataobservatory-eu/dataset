#' @title Create a subject
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
#' @examples
#' subject ("Dataset",
#'         schemeURI = "http://id.loc.gov/authorities/subjects",
#'         valueURI = "https://id.loc.gov/authorities/subjects/sh2018002256",
#'         subjectScheme = "LCCH",
#'         prefix = "lcch:")
#'
#'  ds <- dataset(iris,
#'         title = "The iris Dataset",
#'         author = c(
#'            person(family ="Anderson",
#'            given ="Edgar",
#'            role = "aut")
#'            ),
#'          identifier = "https://doi.org/10.1111/j.1469-1809.1936.tb02137.x",
#'          year = "1935",
#'          version = "1.0",
#'          description = "The famous dataset that is distributed with R.",
#'          url = "https://en.wikipedia.org/wiki/Iris_flower_data_set",
#'          resourceType = "Dataset"
#'          )
#'
#' @export

subject <- function(term,
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
    subject <-  lapply (1:length(term), function(x) new_Subject(term[x],
                                                                subjectScheme = subjectScheme[x],
                                                                schemeURI = schemeURI[x],
                                                                classificationCode = classificationCode[x],
                                                                prefix = prefix[x])
    )
    # this is not nice
    class(subject) <- c("subject", class(subject))
  } else {
    subject <- new_Subject(term=term,
                           subjectScheme=subjectScheme,
                           schemeURI=schemeURI,
                           valueURI=valueURI,
                           classificationCode = classificationCode,
                           prefix = prefix)
  }

  subject
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
    subject <- list ( term = term,
                      subjectScheme = subjectScheme,
                      schemeURI = schemeURI,
                      classificationCode = classificationCode,
                      prefix = prefix)
  } else {
    subject <- list ( term = term,
                      subjectScheme = subjectScheme,
                      schemeURI = schemeURI,
                      valueURI = valueURI,
                      classificationCode = classificationCode,
                      prefix=prefix)
  }

  class(subject) <- c("subject", class(subject))

  subject
}

#' @rdname subject
#' @param x A dataset object created with \code{dataset::\link{dataset}}.
#' @return The Subject field of the dataset's attributes.
subject <- function(x) {
  attr(x, "Subject")
}

value <- list(term  = "Dataset",
              subjectScheme = "LCCH",
              schemeURI = "http://id.loc.gov/authorities/subjects",
              valueURI = "https://id.loc.gov/authorities/subjects/sh2018002256",
              prefix = "lcch:")

#' @rdname subject
#' @param subject A subject field created by \code{\link{subject}}.
`subject<-` <- function(ds, value) {

  if (is.character(value)) {
    value <- list(term=value)
  }

  if (is.null(value$subjectScheme)) value$subjectScheme <- ""
  if (is.null(value$schemeURI)) value$schemeURI <- ""
  if (is.null(value$valueURI)) value$valueURI <- ""
  if (is.null(value$prefix)) value$prefix <- ""

  attr(ds, "Subject") <- new_Subject(term = value$term,
                                     schemeURI = value$schemeURI,
                                     valueURI = value$valueURI,
                                     prefix = value$prefix,
                                     subjectScheme = value$subjectScheme,
                                     classificationCode = value$classificationCode )

  invisible(ds)
}

#' @rdname subject
is.subject <- function(x) {
  UseMethod("is.subject", x)
}

#' @rdname subject
#' @param x An object that is tested if it has a class "subject".
#' @exportS3Method
is.subject.subject <- function(x) ifelse(inherits(x, "subject"), TRUE, FALSE)
