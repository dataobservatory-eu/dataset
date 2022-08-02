#' @title Get/Add subject(s) to a dataset
#' @description Add one or more subject terms to the dataset's metadata.
#' @details In the Dublin Core elements, \href{https://www.dublincore.org/specifications/dublin-core/dcmi-terms/#https://purl.org/dc/elements/1.1/subject}{dct::subject} is
#' defined Typically, the subject will be represented using keywords, key phrases, or classification codes. It is
#' recommended as a best practice to use a controlled vocabulary. \cr
#' In DataCite, subjects are defined as key phrases from a controlled library.
#' @param x An R object
#' @param value Subject terms, or a Subject object created by \code{\link{subject_create}}.
#' @param term A term, or a character vector of multiple terms.
#' @param subjectScheme The scheme to which the term correspondes. If there are multiple terms,
#' provide the subjectScheme(s) in the same order. Optional.
#' @param schemeURI The URI(s) of the subject identifier scheme. If there are multiple terms,
#' provide the schemeURIs in the same order as the terms. Optional.
#' @param valueURI The URI of the subject term. If there are multiple terms,
#' provide the valueURIs in the same order as the terms. Optional.
#' @param overwrite Defaults to \code{FALSE}, in which case new subject(x) <- "Subject" calls
#' are binding further Subjects to the already set Subject properties.
#' @return The subjects as a data.frame of terms
#' @examples
#' x <- data.frame( geo = c("AL", "MK"),
#'                 value = c(1,2))
#' my_subject <- subject_create (
#'                   term = c("R (Computer program language)",
#'                            "Questionnaires--Computer programs"),
#'                   subjectScheme = rep("LC Subject Headings", 2),
#'                   schemeURI = rep("http://id.loc.gov/authorities/subjects",2),
#'                   valueURI = c("https://id.loc.gov/authorities/subjects/sh2002004407.html",
#'                                "http://id.worldcat.org/fast/1085693/")
#' )
#'  subject(x) <- my_subject
#'  subject(x)
#'
#'  y <- data.frame()
#'  subject(y) <- "R (Computer program language)"
#'  subject(y) <- "Questionnaires--Computer programs"
#'  subject(y)
#' @export

subject <- function(x) {

  attr(x, "Subject")
}

#' @rdname subject
#' @export
`subject<-` <- function(x, overwrite = FALSE, value) {

  if (is.null(value)) {
    attr(x, "Subject") <- NULL
    return(x)
  }

  if ( any(c("character", "factor") %in% class(value)) ) {
    value <- subject_create(term = value,
                            subjectScheme = rep(NA_character_, length(value)),
                            schemeURI = rep(NA_character_, length(value)),
                            valueURI = rep(NA_character_,length(value)))
  }

  if (! inherits(value, 'data.frame')) {
    stop("subject <- value: value must be a data.frame object.")
  }

  if (! all(names(value) %in% c("term", "subjectScheme", "schemeURI", "valueURI"))) {
    stop("subject <- value: value must be a data.frame object with 'term', 'subjectScheme', 'schemeURI' and 'valueURI' columns.")
  }

  if ((is.null(attr(x, "Subject"))) | overwrite ) {
    attr(x, "Subject") <- value
  } else {
    attr(x, "Subject") <- rbind(attr(x, "Subject"), value)
  }

  x
}

#' @rdname subject
#' @export
subject_create <- function (term,
                            subjectScheme = NA_character_,
                            schemeURI = NA_character_,
                            valueURI = NA_character_) {
  if(!all.equal(length(term), length(subjectScheme), length(schemeURI), length(valueURI))) {
    stop("subject_add(term, subjectScheme, schemeURI, valueURI): you must give the same number of terms, subjectSchemes, subjectURIs and valueURIs.")
  }

  Subject <- data.frame(term = term,
                        subjectScheme = subjectScheme,
                        schemeURI = schemeURI,
                        valueURI = valueURI)

  attr(Subject, "class") <- c("Subject", "data.frame")

  Subject
}
