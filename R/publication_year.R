#' @title Get/set the publication_year of the object.
#' @description Get/set the optional \code{publication_year} property as an attribute to an R object.
#' @details The \code{PublicationYear} is the year when the data was or will be made
#' publicly available in \code{YYYY} format.
#' @param x An R object, such as a data.frame, a tibble, or a data.table.
#' @param value The publication_year as a character set.
#' @param overwrite If the attributes should be overwritten. In case it is set to \code{FALSE},
#' it gives a message with the current \code{PublicationYear} property instead of overwriting it.
#' Defaults to \code{TRUE} when the attribute is set to \code{value} regardless of previous
#' setting.
#' @return The \code{publication_year} attribute as a character of length 1 is added to \code{x}.
#' @examples
#' iris_dataset <- iris
#' publication_year(iris_dataset) <- 1935
#' publication_year(iris_dataset)
#' @family Reference metadata functions
#' @export

#' @export
publication_year <- function(x) {

  attr(x, "publication_year")

}

#' @rdname publication_year
#' @export
`publication_year<-`  <- function(x,  overwrite = TRUE, value) {

  if (is.null(attr(x, "publication_year"))) {
    if (is.null(value)) {
      attr(x, "publication_year") <- NA_character_
    } else {
      attr(x, "publication_year") <- value
    }
  } else if ( overwrite ) {
    attr(x, "publication_year") <- value
  } else {
    message ("The dataset has already an publication_year: ",  attr(x, "publication_year") )
  }
  x
}
