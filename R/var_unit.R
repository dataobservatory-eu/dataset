#' @title Get / Set a unit of measure
#' @param x A vector.
#' @param value A character string or `NULL` to remove the unit of measure.
#' @param ... Further potential parameters reserved for inherited classes.
#' @details
#' The aim of the \code{unit} attribute is to add to the R vector object its
#' unit of measure (for example, physical units like gram and kilogram or
#' currency units like dollars or euros), so that they are not concatenated or
#' joined in a syntactically correct but semantically incorrect way (i.e.,
#' accidentally concatenating values quoted in dollars and euros from different
#' subvectors.) This is particularly useful when working with linked open data,
#' i.e., when joins or concatenations are performed on data arriving from a remote
#' source.\cr \cr
#' `get_variable_units()` is identical to `var_unit()`.
#' \cr
#' \cr
#' See \code{vignette("defined", package = "dataset")} to use comprehensively
#' with variable labels, namespaces, units of measures, and machine-independent
#' permanent variable identifiers.
#' @family defined metadata methods and functions
#' @examples
#' # The defined vector class and dataset_df support units of measure attributes:
#' var_unit(iris_dataset$Sepal.Length)
#'
#' # Normally columns of a data.frame do not have a unit attribute:
#' var_unit(iris$Sepal.Length)
#'
#' # You can add them with the assignment function:
#' var_unit(iris$Sepal.Length) <- "centimeter"
#'
#' # To remove a unit of measure assign the NULL value:
#' var_unit(iris$Sepal.Length) <- NULL
#' @return The unit attribute of a vector constructed with \code{\link{defined}},
#' or any vector that is enriched with a unit attribute. \cr
#' \cr
#' The \code{var_unit<-}
#' assignment method allows to add, remove, or overwrite this attribute on a vector
#' \code{x}. The assignment function returns the \code{x} vector invisibly.
#' @export
var_unit <- function(x, ...) {
  #rlang::check_dots_used()
  UseMethod("var_unit")
}

#' @export
var_unit.default <- function(x, ...) {
  attr(x, "unit", exact = TRUE)
}

#' @rdname var_unit
#' @export
`var_unit<-` <- function(x, value) {
  UseMethod("var_unit<-")
}

#' @export
`var_unit<-.default` <- function(x, value) {
  unit_attribute(x) <- value
  x
}

#' @rdname var_unit
#' @export
get_variable_units <- var_unit


#' @rdname var_unit
#' @export
unit_attribute <- function(x) {
  attr(x, "unit", exact = TRUE)
}


#' @rdname var_unit
#' @export
get_unit_attribute <- function(x) {
  unit_attribute(x)
}


#' @rdname var_unit
#' @export
set_unit_attribute <- function(x, value) {
  if ((!is.character(value) && !is.null(value)) || length(value) > 1) {
    stop(
      "`unit` should be a single character string or NULL",
      call. = FALSE,
      domain = "R-dataset"
    )
  }
  attr(x, "unit") <- value
  x
}


#' @rdname var_unit
#' @export
`unit_attribute<-` <- set_unit_attribute
