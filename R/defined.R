#' @title Create a semantically well-defined, labelled vector
#' @param x A vector to label. Must be either numeric (integer or double) or
#'   character.
#' @param labels A named vector or `NULL`. The vector should be the same type
#'   as `x`. Unlike factors, labels don't need to be exhaustive: only a fraction
#'   of the values might be labelled.
#' @param label A short, human-readable description of the vector or `NULL`.
#' @param unit A character string of length one containing the
#' unit of measure or `NULL`.
#' @param definition A character string of length one containing a
#' linked definition or `NULL`.
#' @param namespace A namespace for individual observations or categories or `NULL`.
#' @param ... Further parameters for inheritance, not in use.
#' @return The constructor \code{defined} returns a vector with defined
#' value labels, a variable label, an optional unit of measurement and linked
#' definition.
#' @importFrom haven labelled
#' @importFrom labelled to_labelled
#' @import vctrs methods
#' @examples
#' defined(
#'   c(3897, 7365),
#'   label = "Gross Domestic Product",
#'   unit = "million dollars",
#'   definition = "http://data.europa.eu/83i/aa/GDP"
#'  )
#' @export

defined <- function(x,
                    labels = NULL,
                    label = NULL,
                    unit = NULL,
                    definition = NULL,
                    namespace = NULL) {

  if (is.numeric(x)) {
    x <- vec_data(x)
    labels <- vec_cast_named(labels, x, x_arg = "labels", to_arg = "x")
    new_labelled_defined(x, labels = labels, label = label, unit = unit, definition = definition, namespace = namespace)
  } else if (is.character(x)) {
    new_labelled_defined(x, labels = labels, label = label, unit = unit, definition = definition, namespace = namespace)
  } else if (inherits(x, "Date")) {
    new_datetime_defined(x, label = label, unit = unit, definition = definition, namespace = namespace)
  } else if (is.factor(x)) {
    labelled_x <- to_labelled(x)
    var_unit(labelled_x) <- unit
    var_definition(labelled_x) <- definition
    var_namespace(labelled_x) <- namespace
    attr(labelled_x, "class") <- c("haven_labelled_defined", class(labelled_x))
    class(labelled_x)
    labelled_x
  }
}

#' @rdname defined
is.defined <- function(x) {
  any( inherits(x, "haven_labelled_defined"), inherits(x, "datetime_defined"))
  }

#' From haven
#' @keywords internal
vec_cast_named <- function (x, to, ...)
{
  stats::setNames(vec_cast(x, to, ...), names(x))
}

#' @importFrom tibble new_tibble
#' @keywords internal
new_labelled_defined <- function(x = double(),
                                 labels = NULL,
                                 label = NULL,
                                 unit = NULL,
                                 definition = NULL,
                                 namespace = NULL) {

  if (!is.null(unit) && (!is.character(unit) || length(unit) != 1)) {
    stop("defined(..., unit): 'unit' must be a character vector of length one.")
  }

  if (!is.null(definition) && (!is.character(definition) || length(definition) != 1)) {
    stop("defined(..., defintion): 'definition' must be a character vector of length one or NULL.")
  }

  if (!is.null(label) && (!is.character(label) || length(label) != 1)) {
    stop("defined(..., label): 'label' must be a character vector of length one or NULL.")
  }

  if (!is.null(namespace) && (!is.character(namespace) || length(namespace) != 1)) {
    stop("defined(..., namespace): 'namespace' must be a character vector of length one or NULL.")
  }

  tmp <- labelled(x, labels=labels, label=label)

  attr(tmp, "unit") <- unit
  attr(tmp, "definition") <- definition
  attr(tmp, "namespace")  <- namespace
  attr(tmp, "class") <- c("haven_labelled_defined", class(tmp))

  tmp
}

new_datetime_defined <- function(x,
                                 label = NULL,
                                 unit = NULL,
                                 definition = NULL,
                                 namespace = NULL) {
  if (!is.null(unit) && (!is.character(unit) || length(unit) != 1)) {
    stop("defined(..., unit): 'unit' must be a character vector of length one.")
  }

  if (!is.null(definition) && (!is.character(definition) || length(definition) != 1)) {
    stop("defined(..., defintion): 'definition' must be a character vector of length one.")
  }

  if (!is.null(label) && (!is.character(label) || length(label) != 1)) {
    stop("defined(..., label): 'label' must be a character vector of length one.")
  }

  if (!is.null(namespace) && (!is.character(namespace) || length(namespace) != 1)) {
    stop("defined(..., namespace): 'namespace' must be a character vector of length one or NULL.")
  }

  tmp <- x

  attr(tmp, "unit") <- unit
  attr(tmp, "definition") <- definition
  attr(tmp, "namespace")  <- namespace
  attr(tmp, "class") <- c("datetime_defined", class(tmp))

  tmp
}

#' @rdname defined
#' @export
as.character.haven_labelled_defined <- function(x, ...) {
  NextMethod()
}

#' @rdname defined
#' @param object An R object to be summarised.
#' @export
summary.haven_labelled_defined <- function(object, ...) {

  title <- ifelse(nchar(var_label(object))>1, var_label(object), "")

  title <- ifelse(nchar(var_unit(object)) & nchar(title)>1,
                  paste0(title, " (", var_unit(object), ")\n"),
                  paste0(title, "\n"))
  cat(title)
  NextMethod()
}

#' @title Coerce a defined vector to numeric
#' @param x A vector created with \code{\link{defined}}.
#' @return A numeric vector.
#' @examples
#' as_numeric(iris_dataset$Petal.Length)
#' @export
as_numeric <- function(x) {
  UseMethod("as_numeric", x)
}

#' @title Coerce to character vector
#' @param x A vector created with \code{\link{defined}}.
#' @return A character vector.
#' @examples
#' as_character(iris_dataset$Species)
#' @export
as_character <- function(x) {
  UseMethod("as_character", x)
}

#' @rdname as_numeric
#' @export
as_numeric.haven_labelled_defined <- function(x) {

  class_x <- class(x)

  if (any(class_x %in% c("numeric", "double"))) {
    class(x) <- "numeric"
    as.numeric(x)
  } else if ( any ( class_x %in% "integer") ) {
    class(x) <- "integer"
    as.integer(x)
  } else {
    stop("as_numeric.haven_labelled_defined(x): x is not numeric")
  }
}

#' @rdname as_character
#' @importFrom haven as_factor
#' @export
as_character.haven_labelled_defined <- function(x) {

  as.character(haven::as_factor(x))
}

