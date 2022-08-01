#' @title Add Size metadata to an object
#' @description Add the optional DataCite \code{Size} property as an attribute to an R object.
#' @details \code{Size} is an optional property in
#' \href{https://support.datacite.org/docs/schema-optional-properties-v43#13-size}{DataCite 4.3}.
#' The object size is estimated with \code{[utils]{object.size}}.
#' @param x An R object, such as a data.frame, a tibble, or a character vector.
#' @return The estimated object size in memory is added as an attribute to \code{x} in SI
#' kB and IEC KiB (legacy Kb) units,
#' rounded to two decimals. Returns the \code{x} object.
#' @family Reference metadata functions
#' @importFrom utils object.size
#' @examples
#' iris_dataset <- size(iris)
#' attr(iris_dataset, "Size")
#' @export
size <- function(x) {
  a <- object.size(x)

  this_object_size <- paste0(round((as.numeric(a) / 1000),2), " kB [", round((as.numeric(a)/1024),2), " KiB]")

  attr(x, "Size") <- this_object_size

  x
}

