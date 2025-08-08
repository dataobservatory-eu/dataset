#' @title Get or Set the Namespace of a Variable
#'
#' @description
#' Retrieve or assign the namespace part of a permanent, global variable
#' identifier, independent of the current R session or instance.
#'
#' @details
#' The `namespace` attribute is useful when working with remote, linked, or
#' open data sources. Variable identifiers in such datasets are often qualified
#' with a common namespace prefix. When combined, the prefix and namespace form
#' a persistent URI or IRI for the variable.
#'
#' Retaining the namespace ensures the identifiers remain valid and resolvable
#' during validation, merging, or future updates of the vector (such as when it
#' is used as a column in a dataset).
#'
#' `get_variable_namespaces()` is an alias for `var_namespace()`.
#' `namespace_attribute()` and `set_namespace_attribute()` are internal helpers.
#'
#' For full usage, see:
#' `vignette("defined", package = "dataset")` <U+2014> demonstrating integration of
#' variable labels, namespaces, units of measure, and machine-independent
#' identifiers.
#'
#' @param x A vector.
#' @param value A character string specifying the namespace, or `NULL` to
#'   remove it.
#' @param ... Additional arguments for method compatibility with other classes.
#'
#' @return A character string representing the namespace attribute of a vector
#'   constructed with [defined()]. Returns the updated object (in setter forms).
#'
#' @examples
#' # Define a vector with a namespace
#' x <- defined("Q42", namespace = c(wd = "https://www.wikidata.org/wiki/"))
#'
#' # Get the namespace
#' var_namespace(x)
#' get_variable_namespaces(x)
#'
#' # Set the namespace
#' var_namespace(x) <- "https://example.org/ns/"
#'
#' # Remove the namespace
#' var_namespace(x) <- NULL
#'
#' # Use lower-level helpers (not typically used directly)
#' namespace_attribute(x)
#' namespace_attribute(x) <- "https://example.org/custom/"
#'
#' @family defined metadata methods and functions
#' @rdname var_namespace
#' @export

var_namespace <- function(x, ...) {
  # rlang::check_dots_used()
  UseMethod("var_namespace")
}

#' @export
var_namespace.default <- function(x, ...) {
  attr(x, "namespace", exact = TRUE)
}

#' @rdname var_namespace
#' @export
`var_namespace<-` <- function(x, value) {
  UseMethod("var_namespace<-")
}

#' @export
`var_namespace<-.default` <- function(x, value) {
  namespace_attribute(x) <- value
  x
}

#' @rdname var_namespace
#' @export
get_variable_namespaces <- var_namespace


#' @rdname var_namespace
#' @export
namespace_attribute <- function(x) {
  attr(x, "namespace", exact = TRUE)
}


#' @rdname var_namespace
#' @export
get_namespace_attribute <- function(x) {
  namespace_attribute(x)
}


#' @rdname var_namespace
#' @export
set_namespace_attribute <- function(x, value) {
  if ((!is.character(value) && !is.null(value)) || length(value) > 1) {
    stop(
      "`namespace` should be a single character string or NULL",
      call. = FALSE,
      domain = "R-dataset"
    )
  }
  attr(x, "namespace") <- value
  x
}


#' @rdname var_namespace
#' @export
`namespace_attribute<-` <- set_namespace_attribute
