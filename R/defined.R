#' @title Create a semantically well-defined, labelled vector
#' @description
#' Creates a semantically well-defined vector enriched with metadata.
#' `defined()` is an S3 constructor that extends numeric or character vectors
#' with a human-readable label, unit of measurement, linked definition,
#' and optional namespace. These objects preserve semantics while behaving
#' like standard vectors in comparisons, printing, and subsetting.
#'  The \code{defined} constructor creates the objects of this
#' class, which are semantically extended vectors inherited from
#' \code{\link[haven:labelled]{haven::labelled}}.
#' @details
#' A `defined` vector is an extension of a base vector with additional
#' semantic metadata:
#'
#' - A **label** (`label`): a short human-readable description
#' - A **unit** (`unit`): e.g., "kg", "hours", "USD"
#' - A **definition** (`definition`): a URI or textual reference
#' - A **namespace** (`namespace`): for URI-based observation or value identifiers
#'
#' The class inherits from `haven::labelled`, supports typical vector
#' operations (subsetting, comparisons, printing), and integrates with
#' tibbles and tidy workflows via custom `format()`, `print()`, and
#' `as.vector()` methods.
#'
#' Use `is.defined()` to test if an object is of class `defined`.
#' Use `as_numeric()` and `as_character()` to coerce to base types.
#' @param x A vector to label. Must be either numeric (integer or double) or
#'   character.
#' @param labels A named vector or `NULL`. The vector should be the same type as
#'   `x`. Unlike factors, labels don't need to be exhaustive: only a fraction of
#'   the values might be labelled.
#' @param label A short, human-readable description of the vector or `NULL`.
#' @param unit A character string of length one containing the unit of measure
#'   or `NULL`.
#' @param definition A character string of length one containing a linked
#'   definition or `NULL`.
#' @param namespace A namespace for individual observations or categories or
#'   `NULL`.
#' @param ... Further parameters for inheritance, not in use.
#' @family defined metadata methods and functions
#' @return The constructor \code{defined} returns a vector with defined value
#'   labels, a variable label, an optional unit of measurement and linked
#'   definition.\cr \code{is.defined} returns a logical value, stating if the
#'   object is of class \code{defined}.
#' @importFrom haven labelled
#' @importFrom labelled to_labelled is.labelled
#' @import vctrs methods
#' @examples
#'
#' gdp_vector <- defined(
#'   c(3897, 7365, 6753),
#'   label = "Gross Domestic Product",
#'   unit = "million dollars",
#'   definition = "http://data.europa.eu/83i/aa/GDP"
#' )
#'
#' # To check the s3 class of the vector:
#' is.defined(gdp_vector)
#'
#' # To print the defined vector:
#' print(gdp_vector)
#'
#' # To summarise the defined vector:
#' summary(gdp_vector)
#'
#' # Subsetting work as expected:
#' gdp_vector[1:2]
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
  } else if (is.labelled(x)) {
    var_unit(x) <- unit
    var_definition(x) <- definition
    var_namespace(x) <- namespace
    attr(x, "class") <- c("haven_labelled_defined", class(x))
    x
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
#' @export
is.defined <- function(x) {
  any(inherits(x, "haven_labelled_defined"), inherits(x, "datetime_defined"))
}

#' @keywords internal
vec_ptype_abbr.haven_labelled_defined <- function(x, ...) {
  "defined"
}

#' @keywords internal
vec_ptype2.double.haven_labelled_defined <- function(x, y, ...) double()

#' @keywords internal
vec_cast.double.haven_labelled_defined <- function(x, to, ...) vec_data(x)

#' @keywords internal
vec_cast.character.haven_labelled_defined <- function(x, to, ...) vec_data(x)

#' From haven
#' @keywords internal
vec_cast_named <- function(x, to, ...) {
  stats::setNames(vec_cast(x, to, ...), names(x))
}

#' @importFrom tibble new_tibble
#' @importFrom haven labelled
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

  tmp <- haven::labelled(x, labels = labels, label = label)

  attr(tmp, "unit") <- unit
  attr(tmp, "definition") <- definition
  attr(tmp, "namespace") <- namespace
  attr(tmp, "class") <- c("haven_labelled_defined", class(tmp))

  tmp
}

#' @importFrom tibble new_tibble
#' @keywords internal
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
  attr(tmp, "namespace") <- namespace
  attr(tmp, "class") <- c("datetime_defined", class(tmp))

  tmp
}

#' @rdname defined
#' @export
as.character.haven_labelled_defined <- function(x, ...) {
  NextMethod()
}

#' @export
`[.haven_labelled_defined` <- function(x, i, ...) {
  result <- NextMethod("[")
  most_attrs <- c("label", "unit", "definition", "namespace", "labels")
  for (attr_name in most_attrs) {
    attr(result, attr_name) <- attr(x, attr_name)
  }
  class(result) <- class(x)
  result
}

#' @export
#' @importFrom vctrs vec_data
`[[.haven_labelled_defined` <- function(x, i, ...) {
  defined(vec_data(x)[[i]],
    label = var_label(x),
    unit = var_unit(x),
    definition = var_definition(x),
    namespace = var_namespace(x),
    labels = attr(x, "labels")
  )
}


#' @export
#' @importFrom vctrs vec_data
Ops.haven_labelled_defined <- function(e1, e2) {
  # Comparisons work as expected
  lhs <- if (inherits(e1, "haven_labelled_defined")) vec_data(e1) else e1
  rhs <- if (inherits(e2, "haven_labelled_defined")) vec_data(e2) else e2
  .Generic <- .Generic
  do.call(.Generic, list(lhs, rhs))
}

#' @export
#' @importFrom vctrs vec_data
length.haven_labelled_defined <- function(x) {
  length(vec_data(x))
}

#' @export
#' @importFrom vctrs vec_data
head.haven_labelled_defined <- function(x, n = 6L, ...) {
  x[seq_len(min(n, length(x)))]
}

#' @export
#' @importFrom vctrs vec_data
tail.haven_labelled_defined <- function(x, n = 6L, ...) {
  x[seq.int(to = length(x), length.out = min(n, length(x)))]
}

#' @export
print.haven_labelled_defined <- function(x, ...) {
  has_def <- !is.null(var_definition(x)) && !is.na(var_definition(x)) && nzchar(var_definition(x))
  has_unit <- !is.null(var_unit(x)) && !is.na(var_unit(x)) && nzchar(var_unit(x))

  if (has_def && has_unit) {
    msg <- paste0("Defined as ", var_definition(x), ", measured in ", var_unit(x))
  } else if (has_def) {
    msg <- paste0("Defined as ", var_definition(x))
  } else if (has_unit) {
    msg <- paste0("Measured in ", var_unit(x))
  } else {
    msg <- "Defined vector"
  }

  cat(msg, "\n")
  print(vec_data(x), ...)
  invisible(x)
}


#' @export
format.haven_labelled_defined <- function(x, ...) {
  base <- format(vec_data(x), ...)
  unit <- var_unit(x)
  def <- var_definition(x)

  if (!is.null(unit) && nzchar(unit)) {
    suffix <- paste0(" (", unit, ")")
  } else if (!is.null(def) && nzchar(def) && nchar(def) < 30) {
    suffix <- paste0(" [", def, "]")
  } else {
    suffix <- ""
  }

  paste0(base, suffix)
}

#' @export
as.list.haven_labelled_defined <- function(x, ...) {
  lapply(seq_along(x), function(i) x[[i]])
}

#' @export
as.vector.haven_labelled_defined <- function(x, mode = "any") {
  as.vector(vec_data(x), mode = mode)
}
#' @rdname defined
#' @param object An R object to be summarised.
#' @export
summary.haven_labelled_defined <- function(object, ...) {
  label <- var_label(object)
  unit <- var_unit(object)

  if (!is.null(label) && nzchar(label)) {
    if (!is.null(unit) && nzchar(unit)) {
      cat(paste0(label, " (", unit, ")\n"))
    } else {
      cat(paste0(label, "\n"))
    }
  }
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
#' x <- defined(c("a", "b", "c"), label = "Letter code")
#' as_character(x)
#'
#' y <- defined(1:3, label = "Index")
#' as_character(y)
#' @export
as_character <- function(x) {
  UseMethod("as_character", x)
}


#' @title Coerce to factor vector
#' @param x A vector created with \code{\link{defined}}.
#' @return A factor vector.
#' @examples
#' sex <- defined(
#'   c(0, 1, 1, 0),
#'   label = "Sex",
#'   labels = c("Female" = 0, "Male" = 1)
#' )
#' as_factor(sex)
#' @export
as_factor <- function(x) {
  UseMethod("as_factor", x)
}

#' @rdname as_numeric
#' @importFrom vctrs vec_data
#' @examples
#' gdp <- defined(c(3897L, 7365L), label = "GDP", unit = "million dollars")
#' as_numeric(gdp)
#' @export
as_numeric.haven_labelled_defined <- function(x) {
  x_data <- vec_data(x)
  if (!is.numeric(x_data)) {
    stop("as_numeric.haven_labelled_defined(x): underlying data is not numeric")
  }
  x_data
}

#' @rdname as_character
#' @importFrom vctrs vec_data
#' @export
as_character.haven_labelled_defined <- function(x) {
  as.character(vec_data(x))
}

#' @export
#' @importFrom haven as_factor
#' @importFrom vctrs vec_data


as_factor.haven_labelled_defined <- function(x, ...) {
  haven::as_factor(haven::labelled(vec_data(x),
    labels = attr(x, "labels")
  ), ...)
}

#' @title Combine Values into a defined Vector
#' @description
#' The c() method with the haven_labelled_defined class requires a strict
#' matching of the var_label, unit, definiton, and namespace attributes (if
#' they exist and do not have a \code{NULL} value)
#' @param ... objects to be concatenated.
#' @return A haven_labelled_defined vector.
#' @examples
#' a <- defined(1:3, label = "Length", unit = "meter")
#' b <- defined(4:6, label = "Length", unit = "meter")
#' c(a, b)
#' @seealso [defined()]
#' @export
c.haven_labelled_defined <- function(...) {
  dots <- list(...)

  var_labels <- unlist(lapply(dots, var_label))
  val_labels <- lapply(dots, function(x) attr(x, "labels"))
  units <- unlist(lapply(dots, var_unit))
  definitions <- unlist(lapply(dots, definition_attribute))
  namespaces <- unlist(lapply(dots, namespace_attribute))

  all.identical <- function(l) all(mapply(identical, head(l, 1), tail(l, -1)))

  if (length(unique(as.character(var_labels))) > 1) {
    stop("c.haven_labelled_defined(x,y): x,y must have no var_label or the same var_label.")
  }

  if (length(unique(as.character(units))) > 1) {
    stop("c.haven_labelled_defined(x,y): x,y must have no unit or the same unit.")
  }

  if (length(unique(as.character(definitions))) > 1) {
    stop("c.haven_labelled_defined(x,y): x,y must have no definition or the same definition.")
  }

  if (length(unique(as.character(namespaces))) > 1) {
    stop("c.haven_labelled_defined(x,y): x,y must have no namespace or the same namespace.")
  }

  if (!all.identical(val_labels)) {
    stop("c.haven_labelled_defined(x,y): x,y must have no value labels or the same value labels.")
  }

  defined(unname(do.call(c, lapply(dots, function(x) vec_data(x)))),
    label = var_labels[[1]],
    labels = val_labels[[1]],
    definition = definitions[[1]],
    namespace = namespaces[[1]],
    unit = units[[1]]
  )
  # NextMethod()
}
