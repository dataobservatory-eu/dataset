#' @title Get / Set a namespace of measure
#' @description
#' Retain the namespace part of a permanent, global variable identifier which
#' is independent of the R instance in use.
#' @details
#' The namespace attribute is useful when users join or concatenate data from
#' remote, linked, and open data sources. In such cases, variable identifiers
#' (labels or names) are often resolved with a common namespace prefix, which,
#' together with the namespace, forms a URI or IRI permanent identifier for the
#' variable. Retaining the namespace in such cases allows cross-validation or
#' success later updates of the vector (as a column of a dataset.)
#' @param x a vector
#' @param value a character string or `NULL` to remove the namespace of measure.
#' @param ... Further potential parameters reserved for inherited classes.
#' @details
#'   `get_variable_namespaces()` is identical to `var_namespace()`.
#' See \code{vignette("defined", package = "dataset")} to use comprehensively
#' with variable labels, namespaces, units of measures, and machine-independent
#' permanent variable identifiers.
#' @examples
#' qid <- defined(c("Q275912", "Q116196078"),
#'   namespace = c(wd = "https://www.wikidata.org/wiki/")
#' )
#' var_namespace(qid)
#'
#' # To remove a namespace
#' var_namespace(qid) <- NULL
#' @family defined metadata methods and functions
#' @return The namespace attribute of a vector constructed with[defined()].
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
