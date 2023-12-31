#' @title Subsetting datasets
#' @description
#' Accessing columns, rows, or cells via `$`, `[[`, or `[` is mostly similar to
#' [regular data frames][base::Extract]. However, the
#' * `[` always returns a similar dataset by default, even if
#'   only one column is accessed. The title is modified to
#'   from \code{'Original Title'} to \code{'Original Title [subset]'}
#'  * `[[` always returns a vector.
#'  * `$` always returns a vector.
#' @name subsetting
#' @examples
#' # Subsetting single columns:
#' iris_dataset[, "Species"]
#'
#' # Subsetting single column to vector:
#' iris_dataset$Species
#' iris_dataset[, "Species"]
#'
#' # Subsetting a single cell in tabular data:
#' iris_dataset[[1,2]]


#' @rdname subsetting
#' @param i,j Row and column indices. If `j` is omitted, `i` is used as column index.
#' @param ... Ignored.
#' @param drop logical. If \code{TRUE} the result is coerced to the lowest possible
#' dimension. The default is to drop if only one column is left, but not to
#' drop if only one row is left.
#' @seealso [head.dataset]
#' @export
`[.dataset` <- function(x, i, j, drop = FALSE, ...) {

  i_arg <- substitute(i)
  j_arg <- substitute(j)

  if (missing(i)) {
    if (nrow(x)>0) {
      i <- 1:nrow(x)
      i_arg <- NULL
    } else {
      i <- integer()
    }
  } else if (is.null(i)) {
    if (nrow(x)>0) {
     i <- 1:nrow(x)
    } else {
      i <- integer()
    }
  }

  if (missing(j)) {
    j <- 1:ncol(x)
    j_arg <- NULL
  } else if (is.null(j)) {
    j <- integer()
  }

  DataStructure <- attr(x, "DataStructure")
  DataBibentry <- dataset_bibentry(x)
  x_Subject <- subject(x)
  new_title <- paste0(dataset_title(x), " [subset]")
  new_creator <- creator(x)
  classes <- class(x)
  class(x) <- classes[-which(classes == "dataset")]
  tmp <- x[i, j, drop = drop]
  new_DataStructure <- DataStructure[names(tmp)]
  tmp <- dataset(tmp,
                 author = new_creator,
                 title = new_title,
                 datasubject = x_Subject  )
  DataBibentry$title <- new_title
  attr(tmp, "DataBibentry") <-  DataBibentry
  attr(tmp, "DataStructure") <- DataStructure

  tmp
}



#' @rdname subsetting
#' @param name A [name] or a string.
#' @param x A dataset object created with [dataset].
#' @examples
#'
#' #Subsetting a variable into a vector.
#' iris_dataset$Species
#' @export
`$.dataset` <- function(x, name) {
  NextMethod()
}

#' @rdname subsetting
#' @param i,j Row and column indices. If `j` is omitted, `i` is used as column index.
#' @param ... Ignored.
#' @param exact Ignored, with a warning.
#' @export
`[[.dataset` <- function(x, i, j, ..., exact = TRUE) {


  if (!exact) {
    warning("dataset[[..., exact=FALSE]] ignored.")
  }

  if (exists("x", envir = environment())) {
    x_arg <- get("x", envir = environment())
  }

  n_arguments <- 0
  if(!missing(i)) n_arguments <- 1
  if(!missing(j)) n_arguments <- n_arguments+1

  dot_args <- list(...)

  n_dots <- dots_number(...)
  if (n_dots > 0) {
    warning("dataset[[i,j, ...]: Extra arguments ignored.")
  }


  ## Count real arguments
  n_real_args <- 0
  if(!missing(i)) n_real_args <- 1
  if(!missing(j)) n_real_args <- n_real_args+1

  if(missing(x)) message("x is missing")

  undataset <- x_arg
  classes <- class(undataset)
  class(undataset) <- classes[-which(classes == "dataset")]

  if (is.null(x_arg)) {
    message("return NULL")
    return(x_arg)
  }

  if (n_real_args == 2) {
    return(undataset[[i,j]])
  }

  if (n_real_args == 1) {
    if (missing(i)) {
      stop("Subscript can't be missing for datasets in `[[`.")
    } else {
      return(undataset[[i]])
    }
  }
  if (n_real_args == 0) {
    stop("Argument i is missing without default." )
  }
}

# --------------------- internal functions -------------------------------
#' @keywords internal
dots_number <- function(...) {
  dot_args <- list(...)
  length(unique(names(dot_args)))
}

#' @keywords internal
testf <- function (x, i, j, ...) {
  cat("narg", nargs())
}
