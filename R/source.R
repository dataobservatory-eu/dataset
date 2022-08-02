#' @title Get/set the Source of the object.
#' @description Get/set the optional \code{Source} property as an attribute to an
#' R object. DO not confuse with the base R \code{source()} function.
#' @details The \code{Source} is a related resource from which the described resource is
#' derived. See \href{https://purl.org/dc/elements/1.1/source}{dct:source}.
#' @param value The \code{Source} as a character string of lengths one.
#' @inheritParams dublincore
#' @return The \code{Source} attribute as a character of length 1 is added to \code{x}.
#' @examples
#' iris_dataset <- iris
#' dataset_source(iris_dataset) <- "https://doi.org/10.1111/j.1469-1809.1936.tb02137.x"
#' dataset_source(iris_dataset)
#' @family Reference metadata functions
#' @export
dataset_source <- function(x) {
  attr(x, "Source")
}

#' @rdname dataset_source
#' @export
`dataset_source<-` <- function(x,  overwrite = TRUE, value) {

  if (is.null(attr(x, "Source"))) {
    if (is.null(value)) {
      attr(x, "Source") <- NA_character_
    } else {
      attr(x, "Source") <- value
    }
  } else if ( overwrite ) {
    attr(x, "Source") <- value
  } else {
    message ("The dataset has already an Source: ",  source(x) )
  }
  x
}
