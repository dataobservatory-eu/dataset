#' Add Identifier to First Column of a Dataset
#'
#' Adds a prefixed identifier (e.g., `eg:`) to the first column of a dataset,
#' useful for generating semantic row IDs (e.g., for RDF serialization).
#'
#' @param x A dataset created with [dataset_df()], or a regular data frame.
#' @param prefix A character string used as the prefix for row identifiers.
#'   Defaults to `"eg:"` (referring to [example.com](https://example.com)).
#' @param ids Optional. A character vector of custom IDs to use instead of row names.
#'
#' @return
#' A dataset of the same class as `x`, with the first column updated to include
#' unique prefixed identifiers.
#'
#' @examples
#' # Example with a dataset_df object:
#' id_to_column(orange_df)
#'
#' # Example with a regular data.frame:
#' id_to_column(Orange, prefix = "orange:")
#'
#' @export
id_to_column <- function(x, prefix = "eg:", ids = NULL) {
  is_dataset <- is.dataset_df(x)

  lastcol <- ncol(x)

  if (is.null(ids)) {
    ids <- gsub("[^[:alnum:]]", "-", row.names(x))
  } else if (nrow(x) != length(ids)) {
    stop("id_to_column(x, ..., ids) : ids must be of same lengths as nrow(x).")
  }

  if (is.null(prefix)) {
    prefix <- ""
  }

  if ("rowid" %in% names(x)) {
    x$rowid <- paste0(prefix, ids)
    return(x)
  } else {
    rhs <- x
    x$rowid <- paste0(prefix, ids)
    lhs <- x[, "rowid", drop = FALSE]

    if (is_dataset) {
      DataBibentry <- get_bibentry(rhs)
      dataset_subject <- subject(rhs)
      dataset_prov <- provenance(x)
      tmp <- as_dataset_df(cbind(lhs, rhs),
        reference = list(
          author = DataBibentry$author,
          title = DataBibentry$title
        )
      )
      attr(tmp, "dataset_bibentry") <- DataBibentry
      attr(tmp, "prov") <- dataset_prov
      subject(tmp) <- dataset_subject
    } else {
      tmp <- cbind(lhs, rhs)
    }
  }

  tmp
}
