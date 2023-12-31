#' @title Add identifier to columns
#'
#' @description Add a prefixed identifier to the first column of the dataset.
#' @inheritParams dataset
#' @param prefix Defaults to \code{eg:} (example.com).
#' @param ids Defaults to \code{NULL}.
#' @return A dataset conforming the original sub-class of \code{x}.
#' @examples
#'
#' # Example with a dataaset object:
#' id_to_column(iris_dataset)
#'
#' # Example with a data.frame object:
#'
#' id_to_column(iris, prefix="eg:iris-o")
#' @export
id_to_column <- function(x, prefix = "eg:", ids = NULL) {

  is_dataset <- is.dataset(x)

  lastcol <- ncol(x)

  if (is.null(ids)) {
    ids <- gsub("[^[:alnum:]]", "-", row.names(x))
  } else if (nrow(x)!=length(ids)) {
    stop("id_to_column(x, ..., ids) : ids must be of same lengths as nrow(x).")
  }

  if (is.null(prefix)) { prefix <- "" }

  rhs <- x
  x$rowid <- paste0(prefix, ids)
  lhs <- x[, "rowid", drop=FALSE]

  if (is_dataset) {

    DataBibentry <- dataset_bibentry(rhs)
    tmp <- dataset(cbind(lhs, rhs),
                   author=DataBibentry$author,
                   title = DataBibentry$title)

    if (nrow(tmp)>0) {
      row.names(tmp) <- 1:nrow(tmp)
    } else {
      row.names(tmp) <- NULL
    }

    attr(tmp, "DataBibentry") <-  DataBibentry

  } else {
    tmp <- cbind(lhs, rhs)
  }
  tmp
}



