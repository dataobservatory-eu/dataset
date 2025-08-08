#' Get or Set the Rights of a Dataset Object
#'
#' Adds or retrieves the optional `"rights"` attribute of a dataset object.
#' This field contains information about intellectual property or usage rights.
#'
#' @details
#' The `"rights"` field corresponds to
#' [dct:rights](https://www.dublincore.org/specifications/dublin-core/dcmi-terms/elements11/rights/)
#' from Dublin Core, and to `rights` in [DataCite](https://schema.datacite.org/).
#'
#' Rights information typically includes statements about legal ownership,
#' licensing, or usage conditions. It helps ensure that users understand how
#' a dataset may be reused, cited, or shared.
#'
#' @param x A semantically rich data frame created with [dataset_df()] or
#'   [as_dataset_df()].
#' @param value A character string specifying the rights (e.g., `"CC-BY-4.0"`).
#' @param overwrite Logical. Should the existing value be replaced? If `FALSE`
#'   and a value already exists, the function emits a message instead of
#'   overwriting. Defaults to `FALSE`.
#'
#' @return
#' The `"rights"` attribute of the dataset as a character string (length 1).
#' When assigning, the updated object `x` is returned invisibly.
#'
#' @examples
#' rights(orange_df) <- "CC-BY-SA"
#' rights(orange_df)
#'
#' @family bibliographic helper functions
#' @export

rights <- function(x) {
  assertthat::assert_that(is.dataset_df(x),
    msg = "rights(x): x must be a dataset object created with dataset_df() or as_dataset_df()."
  )

  DataBibentry <- get_bibentry(x)
  as.character(DataBibentry$rights)
}

#' @rdname rights
#' @export
`rights<-` <- function(x, overwrite = FALSE, value) {
  assertthat::assert_that(is.dataset_df(x),
    msg = "rights(x): x must be a dataset object created with dataset_f() or as_dataset_df()."
  )

  assertthat::assert_that(is.null(value) | length(value) == 1,
    msg = "rights(x): x must have length=1 (or set to NULL)."
  )

  DataBibentry <- invisible(get_bibentry(x))

  if (is.null(value)) {
    DataBibentry$rights <- ":unas"
    attr(x, "dataset_bibentry") <- DataBibentry
    return(invisible(x))
  }

  if (length(value) > 1) {
    stop("rights(x) <- value: value must be of length 1.")
  }

  is_unas <- DataBibentry$rights == ":unas"

  if (is.null(DataBibentry$rights)) {
    DataBibentry$rights <- value
  } else if (is_unas) {
    DataBibentry$rights <- value
  } else if (overwrite) {
    DataBibentry$rights <- value
  } else {
    message("The dataset has already a rights field: ", DataBibentry$rights)
  }

  attr(x, "dataset_bibentry") <- DataBibentry
  invisible(x)
}
