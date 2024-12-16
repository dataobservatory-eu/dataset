#' @title Get / Set a namespace of measure
#' @param x a vector
#' @param value a character string or `NULL` to remove the namespace of measure.
#' @param ... Further potential parameters reserved for inherited classes.
#' @details
#'   `get_variable_namespaces()` is identical to `var_namespace()`.
#' @examples
#' qid = defined(c("Q275912", "Q116196078"), namespace = "https://www.wikidata.org/wiki/")
#' var_namespace(qid)
#' # To remove a namespace
#' var_namespace(qid) <- NULL
#' @export
var_namespace <- function(x, ...) {
  #rlang::check_dots_used()
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
