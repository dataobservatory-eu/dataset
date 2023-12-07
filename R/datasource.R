#' @title Get/set the Source of the object.
#' @description Get/set the optional \code{Source} property as an attribute to an
#' R object. Do not confuse with the base R \code{source()} function.
#' @details The \code{Source} is a related resource from which the described resource is
#' derived. See \href{https://purl.org/dc/elements/1.1/source}{dct:source}. In Datacite,
#' the source is described by a \code{relatedIdentifierType} with the property
#' \code{relationType="isDerivedFrom"}.
#' @param x A dataset object created with \code{dataset::\link{dataset}}.
#' @param value The \code{Source} as a character string of lengths one.
#' @return The \code{Source} attribute as a character of length 1 is added to \code{x}.
#' @examples
#' datasource(iris_dataset) <- "https://doi.org/10.1111/j.1469-1809.1936.tb02137.x"
#' datasource(iris_dataset)
#' @family Reference metadata functions
#' @importFrom assertthat assert_that
#' @export
datasource <- function(x) {

  assert_that(is.dataset(x),
              msg = "datasource(x) must be a dataset object created with dataset() or as_dataset().")
  DataBibentry <- dataset_bibentry(x)
  DataBibentry$source

}

#' @rdname datasource
#' @export
`datasource<-` <- function(x,  overwrite = TRUE, value) {

  assert_that(is.dataset(x),
              msg = "datasource(x): x must be a dataset object created with dataset() or as_dataset().")

  DataBibentry <- invisible(dataset_bibentry(x))

  if ( is.null(value) ) {
    DataBibentry$source <- ":unas"
    attr(x, "DataBibentry") <- DataBibentry
    return(invisible(x))
  }

  if (length(value)>1) {
    stop("source(x) <- value: value must be of length 1.")
  }

  is_unas <- DataBibentry$source  ==  ":unas"

  if (is.null(DataBibentry$source)) {
    DataBibentry$source <- value
  } else if (is_unas) {
    DataBibentry$source <- value
  }else if ( overwrite ) {
    DataBibentry$source <- value
  } else {
    message ("The dataset has already a source: ",    DataBibentry$source )
  }

  attr(x, "DataBibentry") <- DataBibentry
  invisible(x)
}
