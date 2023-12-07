#' @title Get/set the Description of the object.
#' @description Get/set the optional \code{Description} property as an attribute to an R object.
#' @details The \code{Description} is recommended for discovery in DataCite. All additional information that does not
#' fit in any of the other categories. May be used for technical information. A free text.
#' Similar to \href{https://purl.org/dc/elements/1.1/description}{dct:description}.
#' @param x A dataset object created with \code{dataset::\link{dataset}}.
#' @param value The \code{Description} as a character set.
#' @param overwrite If the \code{Description} attribute should be overwritten. In case it is set to \code{FALSE},
#' it gives a message with the current \code{Description} property instead of overwriting it.
#' Defaults to \code{FALSE}, when it gives a warning at an accidental overwrite attempt.
#' @return The \code{Description} attribute as a character of length 1 is added to \code{x}.
#' @examples
#' description(iris_dataset) <- "The famous iris dataset used in R language examples."
#' description(iris_dataset)
#' @family Reference metadata functions
#' @export

#' @export
description <- function(x) {
  assert_that(is.dataset(x),
              msg = "description(x): x must be a dataset object created with dataset() or as_dataset().")

  ds_bibentry <- dataset_bibentry(x)
  as.character(ds_bibentry$description)

}

#' @rdname description
#' @export
`description<-`  <- function(x,  overwrite = FALSE, value) {

  assert_that(is.dataset(x),
              msg = "description(x) <- value: x must be a dataset object created with dataset() or as_dataset().")

  ds_bibentry <- dataset_bibentry(x)
  existing_description <- as.character(ds_bibentry$description)

  if (is.null(value) ) {
    value <- ":unas"
  }

  if ( overwrite | existing_description %in% c("", ":unas", ":tba")) {
    ds_bibentry$description <- as.character(value)
    attr(x, "DataBibentry") <- ds_bibentry
  } else {
    warning ("The dataset has already a description: ",   existing_description , "." )
  }
  invisible(x)
}
