#' @title Get/Add the version of the object.
#' @description Add the optional Version property as an attribute to an R object.
#' @details Version is an optional property in
#' \href{https://support.datacite.org/docs/schema-optional-properties-v43#13-size}{DataCite 4.3}.
#' It is not part of the "core" Dublin Core terms, but ...
#' \href{https://www.dublincore.org/specifications/dublin-core/dcmi-terms/}{Dublin Core metadata terms}.
#' @param x An R object, such as a data.frame, a tibble, or a character vector.
#' @param Version The version as a character set.
#' @return The Version attribute as a character of length 1 is added to \code{x}.
#' @examples
#' iris_dataset <- version_add(x = iris, Version= "1.0")
#' version(iris_dataset)
#' @family Optional metadata
#' @export
version <- function(x) {
  attr(x, "Version")
}

#' @rdname version
#' @export
version_add <- function(x, Version ) {

  if ( is.null(Version)) {
   stop("verion_add(x, Version): version must be a character string or a number that can be coerced into a character string of length=1.")
  }

  attr(x, "Version") <- Version
  x
}
