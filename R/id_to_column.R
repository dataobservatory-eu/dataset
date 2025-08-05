#' @title Add identifier to columns
#'
#' @description Add a prefixed identifier to the first column of the dataset.
#' @param x A dataset created with [dataset_df()].
#' @param prefix Defaults to \code{eg:} (example.com).
#' @param ids Defaults to \code{NULL}.
#' @return A dataset conforming the original sub-class of \code{x}.
#' @examples
#'
#' # Example with a dataset_df object:
#' id_to_column(orange_df)
#'
#' # Example with a data.frame object:
#' id_to_column(Orange, prefix = "orange:")
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
