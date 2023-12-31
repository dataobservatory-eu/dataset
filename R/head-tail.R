#' @title Return the first or last parts of a dataset object
#' @description
#' Returns the first or last parts of a dataset.
#' Both head() and tail() are generic functions.
#' @param x A dataset object created with [dataset].
#' @param n Maximum number of rows.
#' @param ... Ignored.
#' @return A subsetted dataset, the first or last parts of a dataset object.
#' @seealso [subsetting]
#' @examples
#'
#' # Subsetting the top n rows (observations)
#' head(iris_dataset, n=3)
#' @export
head.dataset <- function(x, n=6L, ...) {

  if(is.null(x)) return(x)

  if (nrow(x)>0) {
    n <- min(nrow(x), n)
    x[1:n, ]
  } else {
    x
  }
}

#' @rdname head.dataset
#' @param n an integer vector of length up to \code{dim(x)}
#' (or 1, for non-dimensioned objects). A \code{logical} is silently coerced to
#' integer. Values specify the indices to be selected in the corresponding
#' dimension (or along the length) of the object. A positive value of \code{n[i]}
#' includes the first/last \code{n[i]} indices in that dimension, while a negative
#' value excludes the last/first abs(n[i]), including all remaining indices.
#' \code{NA} or non-specified values (when \code{length(n) < length(dim(x))}) select all
#' indices in that dimension. Must contain at least one non-missing value.
#' @param keepnums in each dimension, if no names in that dimension are present,
#' create them using the indices included in that dimension.
#' Ignored if \code{dim(x)} is \code{NULL} or its length 1.
#' @param ... arguments to be passed to or from other methods.
#' @examples
#'
#' # Subsetting the last n rows (observations)
#' tail(iris_dataset, 3)
#' @export

tail.dataset <- function(x, n, keepnums=FALSE, ...){

  if(is.null(x)) {
    warning("tail.dataset(x, ...): x is NULL")
    return(x)
  }

  DataBibentry <- dataset_bibentry(x)
  tmp <- tail(as.data.frame(x), keepnums=keepnums, ...=...)
  tmp <- dataset(tmp, author = DataBibentry$author,
                 title= paste0( DataBibentry$title, " [subset of last observations]"),
                 identifier = DataBibentry$identifier)
  tmp
}
