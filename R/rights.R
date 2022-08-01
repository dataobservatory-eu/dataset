#' @title Get/set the Rights of the object.
#' @description Get/set the optional \code{Rights} property as an attribute to an
#' R object.
#' @details \code{Rights} corresponds to
#' \href{http://purl.org/dc/elements/1.1/rights}{dct:rights} and
#' \code{\link{datacite}} Rights. Information about rights held in and over the resource.
#' Typically, rights information includes a statement about various property rights associated with the resource,
#' including intellectual property rights.
#' @param x An R object, such as a data.frame, a tibble, or a data.table.
#' @param value The \code{Rights} as a character set.
#' @param overwrite If the \code{Rights} attribute should be overwritten. In case it is set to \code{FALSE},
#' it gives a message with the current \code{Rights} property instead of overwriting it.
#' Defaults to \code{TRUE} when the attribute is set to \code{value} regardless of previous
#' setting.
#' @return The \code{Rights} attribute as a character of length 1 is added to \code{x}.
#' @examples
#' iris_dataset <- iris
#' rights(iris_dataset) <- "CC-BY-SA"
#' rights(iris_dataset)
#' @family Reference metadata functions
#' @export
rights <- function(x) {
  attr(x, "Rights")
}

#' @rdname rights
#' @export
`rights<-` <- function(x, value, overwrite = TRUE) {

  if (is.null(attr(x, "Rights"))) {
    if (is.null(value)) {
      attr(x, "Rights") <- NA_character_
    } else {
      attr(x, "Rights") <- value
    }
  } else if ( overwrite ) {
    attr(x, "Rights") <- value
  } else {
    message ("The dataset has already an Rights: ",  rights(x) )
  }
  x
}
