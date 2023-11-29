#' @title Get/set the Rights of the object.
#' @description Get/set the optional \code{Rights} property as an attribute to an
#' R object.
#' @details \code{Rights} corresponds to
#' \href{https://purl.org/dc/elements/1.1/rights}{dct:rights} and
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
  assertthat::assert_that(is.dataset(x),
                          msg = "rights(x): x must be a dataset object created with dataset() or as_dataset().")
  DataBibentry <- dataset_bibentry(x)
  DataBibentry$source
}

#' @rdname rights
#' @export
`rights<-` <- function(x,  overwrite = TRUE, value) {

  assertthat::assert_that(is.dataset(x),
                          msg = "rights(x): x must be a dataset object created with dataset() or as_dataset().")

  DataBibentry <- invisible(dataset_bibentry(x))

  if ( is.null(value) ) {
    DataBibentry$source <- ":unas"
    attr(x, "DataBibentry") <- DataBibentry
    return(invisible(x))
  }

  if (length(value)>1) {
    stop("rights(x) <- value: value must be of length 1.")
  }

  is_unas <- DataBibentry$source  ==  ":unas"

  if (is.null(DataBibentry$rights)) {
    DataBibentry$rights <- value
  } else if (is_unas) {
    DataBibentry$rights <- value
  }else if ( overwrite ) {
    DataBibentry$rights <- value
  } else {
    message ("The dataset has already a rights field: ",    DataBibentry$rights )
  }

  attr(x, "DataBibentry") <- DataBibentry
  invisible(x)
}
