#' @title Get/set the Description of the object.
#' @description Get/set the optional \code{Description} property as an attribute
#'   to an R object.
#' @details The \code{Description} is recommended for discovery in DataCite. All
#'   additional information that does not fit in any of the other categories.
#'   May be used for technical information. A free text. Similar to
#'   \href{https://www.dublincore.org/specifications/dublin-core/dcmi-terms/elements11/description/}{dct:description}.
#' @param x A dataset object created with [dataset_df()] or
#'   \code{dataset::\link{as_dataset_df}}.
#' @param value The \code{Description} as a character set.
#' @param overwrite If the \code{Description} attribute should be overwritten.
#'   In case it is set to \code{FALSE}, it gives a message with the current
#'   \code{Description} property instead of overwriting it. Defaults to
#'   \code{FALSE}, when it gives a warning at an accidental overwrite attempt.
#' @return The \code{Description} attribute as a character of length 1 is added
#'   to \code{x}.
#' @examples
#' description(orange_df)
#' description(
#'   orange_df,
#'   overwrite = TRUE
#' ) <- "The 'orange' dataset has 35 rows and 3 columns
#'                           of records of the growth of orange trees."
#' description(orange_df)
#' @family Reference metadata functions
#' @export

#' @export
description <- function(x) {
  assert_that(is.dataset_df(x),
    msg = "description(x): x must be a dataset object created with dataset_df() or as_dataset_df()."
  )

  ds_bibentry <- get_bibentry(x)
  as.character(ds_bibentry$description)
}

#' @rdname description
#' @export
`description<-` <- function(x, overwrite = FALSE, value) {
  assert_that(is.dataset_df(x),
    msg = "description(x) <- value: x must be a dataset object created with dataset_df() or as_dataset_df()."
  )

  ds_bibentry <- get_bibentry(x)
  existing_description <- as.character(ds_bibentry$description)

  if (is.null(value)) {
    value <- ":unas"
  }

  if (overwrite ||
      length(existing_description) == 0 ||
      existing_description %in% c("", ":unas", ":tba")) {

    ds_bibentry$description <- as.character(value)
    attr(x, "dataset_bibentry") <- ds_bibentry
  } else {
    warning("The dataset already has a description: ", existing_description, ".")
  }
  invisible(x)
}
