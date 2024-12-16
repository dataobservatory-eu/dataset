#' @title Get/set the publication_year of the object.
#' @description Get/set the optional \code{publication_year} property as an attribute to an R object.
#' @details The \code{PublicationYear} is the year when the data was or will be made
#' publicly available in \code{YYYY} format.
#' See
#' \href{https://support.datacite.org/docs/datacite-metadata-schema-v44-mandatory-properties#publicationyearadditional-guidance}{Publication Year: DataCite Additional Guidance}.
#' @param x A semantically rich data frame object created by  \code{dataset::\link{dataset_df}} or
#'  \code{dataset::\link{as_dataset_df}}.
#' @param value The publication_year as a character set.
#' @param overwrite If the attributes should be overwritten. In case it is set to \code{FALSE},
#' it gives a message with the current \code{PublicationYear} property instead of overwriting it.
#' Defaults to \code{TRUE} when the attribute is set to \code{value} regardless of previous
#' setting.
#' @return Returns the \code{year} metadata field of the \code{DataBibentry} of
#' the dataset
#' @examples
#' publication_year(iris_dataset)
#' publication_year(iris_dataset) <- 1936
#' @family Reference metadata functions
#' @export

#' @export
publication_year <- function(x) {
  assert_that(is.dataset_df(x),
              msg = "publication_year(x): x must be a dataset object created with dataset() or as_dataset().")

  ds_bibentry <- get_bibentry(x)
  as.character(ds_bibentry$year)
}

#' @rdname publication_year
#' @export
`publication_year<-`  <- function(x,  overwrite = TRUE, value) {
  assert_that(is.dataset_df(x),
              msg = "publication_year(x) <- value: x must be a dataset object created with dataset() or as_dataset().")

  ds_bibentry <- get_bibentry(x)
  publication_year <- ds_bibentry$year

  if (is.null(value)) {
      value <- ":unas"
    }

  if ( overwrite ) {
    ds_bibentry$year <- as.character(value)
    attr(x, "dataset_bibentry") <- ds_bibentry
   } else {
    warning ("The dataset has already an publication_year: ",  ds_bibentry$year, "." )
  }
  invisible(x)
}
