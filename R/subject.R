#' @title Get/Add subject(s) to a dataset
#' @description Add one or more subject terms to the dataset's metadata.
#' @details In the Dublin Core elements, \href{https://www.dublincore.org/specifications/dublin-core/dcmi-terms/#http://purl.org/dc/elements/1.1/subject}{dct::subject} is
#' defined Typically, the subject will be represented using keywords, key phrases, or classification codes. It is
#' recommended as a best practice to use a controlled vocabulary. \cr
#' In DataCite, subjects are defined as key phrases from a controlled library.
#' @param x An R object
#' @param term A term, or a character vector of multiple terms.
#' @param scheme The scheme to which the term correspondes. If there are multiple terms,
#' provide the scheme(s) in the same order.
#' @param identifier The identifier of the term definition(s), in the order of the terms.
#' @return The subjects as a data.frame of terms
#' @examples
#' x <- data.frame()
#' subject_add (x, term = c("R (Computer program language)",
#'                          "Questionnaires--Computer programs"),
#'                 scheme = rep("url", 2),
#'                 identifier = c("https://id.loc.gov/authorities/subjects/sh2002004407.html",
#'                                "http://id.worldcat.org/fast/1085693/") )
#'  subject(x)
#' @export

subject <- function(x) {

  attr(x, "Subject")
}

#' @rdname subject
#' @export
subject_add <- function(x, term, scheme, identifier) {

  if(!all.equal(length(term), length(scheme), length(identifier))) {
    stop("subject_add(term, scheme, identifier): you must give the same number of terms, schemes, identifiers.")
  }

  attr(x, "Subject") <- data.frame(term = term,
                                   scheme = scheme,
                                   identifier = identifier)

  x
}
