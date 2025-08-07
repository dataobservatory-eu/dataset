#' @title Get or Set the Geolocation of a Dataset Object
#'
#' @description
#' Access or assign the optional `geolocation` attribute to a semantically rich
#' dataset object.
#'
#' @details
#' The `geolocation` field describes the spatial region or named place where
#' the data was collected or that the dataset is about. This field is
#' recommended for data discovery in DataCite Metadata Schema 4.4.
#'
#' See: [DataCite: Geolocation Guidance](https://support.datacite.org/docs/datacite-metadata-schema-v44-recommended-and-optional-properties#18-geolocation)
#'
#' @param x A dataset object created by [dataset_df()] or
#'   `dataset::as_dataset_df()`.
#' @param value A character string specifying the `geolocation`.
#' @param overwrite Logical. If `TRUE` (default), the existing `geolocation`
#'   attribute is replaced with `value`. If `FALSE`, the function returns a
#'   message and does not overwrite the existing value.
#'
#' @return A character string of length 1, representing the `geolocation`
#'   attribute attached to `x`.
#'
#' @examples
#' orange_dataset <- orange_df
#' geolocation(orange_df) <- "US"
#' geolocation(orange_df)
#'
#' geolocation(orange_df, overwrite = FALSE) <- "GB"
#'
#' @family Reference metadata functions
#' @export

geolocation <- function(x) {
  attr(x, "Geolocation")
}

#' @rdname geolocation
#' @export
`geolocation<-` <- function(x, overwrite = TRUE, value) {
  if (is.null(attr(x, "Geolocation"))) {
    if (is.null(value)) {
      attr(x, "Geolocation") <- NA_character_
    } else {
      attr(x, "Geolocation") <- value
    }
  } else if (overwrite) {
    attr(x, "Geolocation") <- value
  } else {
    message("The dataset has already an Geolocation: ", geolocation(x))
  }
  x
}
