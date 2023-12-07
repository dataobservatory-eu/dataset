#' @title Get/Estimate/Add the Size metadata to an object
#' @description Add the optional DataCite \code{Size} property as an attribute to an R object.
#' @details \code{Size} is an optional property in DataCite 4.4. See:
#' \href{https://support.datacite.org/docs/datacite-metadata-schema-v44-recommended-and-optional-properties#13-size}{datacite:Size}.
#' The object size is estimated with \code{\link[utils:object.size]{utils::object.size}} and it may differ
#' from the actual serialisation to another file format.
#' @param x A dataset object created with \code{dataset::\link{dataset}}.
#' @return Returns the \code{size} metadata field of the \code{DataBibentry} of
#' the dataset.
#' @family Reference metadata functions
#' @importFrom utils object.size
#' @examples
#' size(iris_dataset) <- "estimate"
#' size(iris_dataset)
#' @export

size <- function(x) {
  assert_that(is.dataset(x),
              msg = "size(x): x must be a dataset object created with dataset() or as_dataset().")

  ds_bibentry <- dataset_bibentry(x)
  as.character(ds_bibentry$size)
}

#' @rdname size
#' @param value A string (size of the dataset in a certain serialisation),
#' or \code{"estimate"} produces and estimate with \code{[utils]{object.size}}.
#' @param overwrite If a warning should be given when trying to overwrite an
#' existing \code{Size} property, defaults to \code{TRUE}.\cr
#' The estimated object size in memory is added in SI
#' kB and IEC KiB (legacy Kb) units,
#' rounded to two decimals.
#' @export
`size<-`  <- function(x,  overwrite = TRUE, value) {
  assert_that(is.dataset(x),
              msg = "size(x) <- value: x must be a dataset object created with dataset() or as_dataset().")

  ds_bibentry <- dataset_bibentry(x)
  existing_size <- ds_bibentry$size

  if (is.null(value) | value %in% c("estimate", "object.size") ) {
    a <- object.size(x)
    object_size_text <- paste0(round((as.numeric(a) / 1000),2), " kB [", round((as.numeric(a)/1024),2), " KiB]")
  } else {
    object_size_text <- as.character(value)
  }

  if ( overwrite | length(existing_size)==0 ) {
    ds_bibentry$size <- as.character(object_size_text)
    attr(x, "DataBibentry") <- ds_bibentry
  } else {
    warning ("The dataset has already an size: ",  object_size_text, "." )
  }
  invisible(x)
}
