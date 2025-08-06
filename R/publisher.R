#' Get or Set the Publisher of a Dataset Object
#'
#' Adds or retrieves the optional `"publisher"` attribute for a dataset object.
#' This property aligns with `dct:publisher` (Dublin Core) and `publisher`
#' (DataCite).
#'
#' @description
#' The publisher is the entity responsible for holding, archiving, releasing,
#' or distributing the resource. It is typically included in dataset citation
#' metadata.
#'
#' For software, this might refer to a code repository (e.g., GitHub). If both
#' a hosting platform and a producing institution are involved, use the
#' publisher for the institution and [contributor()] with
#' `contributorType = "hostingInstitution"` for the platform.
#'
#' @param x A dataset object created with [dataset_df()] or
#'   [as_dataset_df()].
#' @param overwrite Logical. Should existing publisher metadata be overwritten?
#'   Defaults to `FALSE`. If `FALSE` and the field exists, a warning is issued.
#' @param value A character string specifying the publisher.
#'
#' @return
#' A character string of length one containing the `"publisher"` attribute.
#' When assigning, the updated object `x` is returned invisibly.
#'
#' @examples
#' publisher(orange_df) <- "Wiley"
#' publisher(orange_df)
#'
#' @family Reference metadata functions
#' @importFrom assertthat assert_that
#' @export

publisher <- function(x) {
  assert_that(is.dataset_df(x),
    msg = "publisher(x): x must be a dataset object created with dataset() or as_dataset()."
  )

  DataBibentry <- get_bibentry(x)
  as.character(DataBibentry$publisher)
}

#' @rdname publisher
#' @export
`publisher<-` <- function(x, overwrite = TRUE, value) {
  if (!is.dataset_df(x)) {
    stop("publisher(x): x must be a dataset object created with dataset() or as_dataset().")
  }

  DataBibentry <- invisible(get_bibentry(x))

  if (is.null(value)) {
    DataBibentry$publisher <- ":tba"
    attr(x, "dataset_bibentry") <- DataBibentry
    return(x)
  }

  if (length(value) > 1) {
    stop("publisher(x) <- value: value must be of length 1.")
  }

  is_tba <- DataBibentry$publisher == ":tba"

  if (is.null(DataBibentry$publisher)) {
    DataBibentry$publisher <- value
  } else if (is_tba) {
    DataBibentry$publisher <- value
  } else if (overwrite) {
    DataBibentry$publisher <- value
  } else {
    message(
      "The dataset has already an Publisher: ",
      DataBibentry$publisher
    )
  }

  attr(x, "dataset_bibentry") <- DataBibentry
  invisible(x)
}
