#' @title Get / Set a unit of measure
#' @param x A vector.
#' @param value A character string or `NULL` to remove the unit of measure.
#' @param ... Further potential parameters reserved for inherited classes.
#' @details
#'   `get_variable_units()` is identical to `var_unit()`.
#' @examples
#' var_unit(iris$Sepal.Length)
#' var_unit(iris$Sepal.Length) <- "centimeter"
#' \dontrun{
#' View(iris)
#' }
#' # To remove a unit of measure
#' var_unit(iris$Sepal.Length) <- NULL
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
