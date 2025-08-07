#' @title Get or Set the Publication Year of a Dataset Object
#'
#' @description
#' Access or assign the optional `publication_year` attribute to a semantically
#' rich dataset object.
#'
#' @details
#' The `publication_year` represents the year when the dataset was or will be
#' made publicly available, in `YYYY` format. For additional context, see
#' [DataCite: Publication Year – Additional Guidance](https://support.datacite.org/docs/datacite-metadata-schema-v44-mandatory-properties#publicationyearadditional-guidance).
#'
#' @param x A dataset object created by [dataset_df()] or
#'   `dataset::as_dataset_df()`.
#' @param value A character string specifying the publication year.
#' @param overwrite Logical. If `TRUE` (default), the existing
#'   `publication_year` attribute is replaced with `value`. If `FALSE`, the
#'   function returns a message and does not overwrite the existing value.
#'
#' @return The `publication_year` attribute as a character string.
#'
#' @examples
#' publication_year(orange_df)
#' publication_year(orange_df) <- "1998"
#'
#' @family Reference metadata functions
#' @export

publication_year <- function(x) {
  assert_that(is.dataset_df(x),
    msg = "publication_year(x): x must be a dataset object created with dataset() or as_dataset()."
  )

  ds_bibentry <- get_bibentry(x)
  as.character(ds_bibentry$date)
}

#' @rdname publication_year
#' @export
`publication_year<-` <- function(x, overwrite = TRUE, value) {
  assert_that(is.dataset_df(x),
    msg = "publication_year(x) <- value: x must be a dataset object created with dataset_df() or as_dataset_df()."
  )

  ds_bibentry <- get_bibentry(x)
  publication_year <- ds_bibentry$date

  if (is.null(value)) {
    value <- ":unas"
  }

  if (overwrite) {
    ds_bibentry$date <- as.character(value)
    attr(x, "dataset_bibentry") <- ds_bibentry
  } else {
    warning("The dataset has already an publication_year: ", ds_bibentry$date, ".")
  }
  invisible(x)
}
