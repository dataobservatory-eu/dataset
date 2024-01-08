#' @title Get/set the source property of a dataset.
#' @description Get/set the optional \code{Source} property as an attribute to an
#' R object. Do not confuse with the base R \code{source()} function.
#' @details The \code{Source} is a related resource from which the described resource is
#' derived. See \href{https://www.dublincore.org/specifications/dublin-core/dcmi-terms/elements11/source}{dct:source}. In Datacite,
#' the source is described by a \code{relatedIdentifierType} with the property
#' \code{relationType="isDerivedFrom"}.
#' @param x A dataset object created with \code{dataset::\link{dataset}}.
#' @param value The \code{Source} as a character string of lengths one.
#' @return The \code{Source} attribute as a character of length 1 is added to
#' \code{x}.
#' @examples
#' iris_dataset <- datasource_set(iris_dataset, "https://doi.org/10.1111/j.1469-1809.1936.tb02137.x")
#' datasource_get(iris_dataset)
#' @family Reference metadata functions
#' @importFrom assertthat assert_that
#' @export
datasource_get <- function(x) {

  assert_that(is.dataset(x),
              msg = "datasource_get(x) must be a dataset object created with dataset() or as_dataset().")
  DataBibentry <- dataset_bibentry(x)
  DataBibentry$source

}

#' @rdname datasource_get
#' @param overwrite If the attributes should be overwritten. In case it is set
#' to \code{FALSE},it gives a warning with the current \code{datasource}
#' property instead of overwriting it. Defaults to \code{FALSE}.
#' @export
datasource_set <- function(x,  value, overwrite = TRUE) {

  assert_that(is.dataset(x),
              msg = "datasource_set(x): x must be a dataset object created with dataset() or as_dataset().")

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
