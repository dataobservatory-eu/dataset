#' @title Get/set the Description of the object.
#' @description Get/set the optional \code{Description} property as an attribute to an R object.
#' @details The \code{Description} is recommended for discovery in DataCite. All additional information that does not
#' fit in any of the other categories. May be used for technical information. A free text.
#' Similar to \href{http://purl.org/dc/elements/1.1/description}{dct:description}.
#' @param x An R object, such as a data.frame, a tibble, or a data.table.
#' @param value The \code{Description} as a character set.
#' @param overwrite If the \code{Description} attribute should be overwritten. In case it is set to \code{FALSE},
#' it gives a message with the current \code{Description} property instead of overwriting it.
#' Defaults to \code{TRUE} when the attribute is set to \code{value} regardless of previous
#' setting.
#' @return The \code{Description} attribute as a character of length 1 is added to \code{x}.
#' @examples
#' iris_dataset <- iris
#' description(iris_dataset) <- "The famous iris dataset used in R language examples."
#' description(iris_dataset)
#' @family Reference metadata functions
#' @export

#' @export
description <- function(x) {

  attr(x, "Description")

}

#' @rdname description
#' @export
`description<-`  <- function(x, value, overwrite = TRUE) {

  if (is.null(attr(x, "Description"))) {
    if (is.null(value)) {
      attr(x, "Description") <- NA_character_
    } else {
      attr(x, "Description") <- value
    }
  } else if ( overwrite ) {
    attr(x, "Description") <- value
  } else {
    message ("The dataset has already an Description: ",  attr(x, "Description") )
  }
  x
}
