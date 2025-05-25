#' @title Create a semantically well-defined, labelled vector
#' @description
#' Creates a semantically well-defined vector enriched with metadata.
#' `defined()` is an S3 constructor that extends numeric or character vectors
#' with a human-readable label, unit of measurement, linked concept,
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
#' - A **concept** (`concept`): a URI or textual reference
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
#' @param concept A character string of length one containing a linked
#'   concept or `NULL`.
#' @param namespace A namespace for individual observations or categories or
#'   `NULL`.
#' @param ... Further parameters for inheritance, not in use.
#' @family defined metadata methods and functions
#' @return The constructor \code{defined} returns a vector with defined value
#'   labels, a variable label, an optional unit of measurement and linked
#'   concept.\cr \code{is.defined} returns a logical value, stating if the
#'   object is of class \code{defined}.
#' @importFrom haven labelled
#' @importFrom labelled to_labelled is.labelled
#' @import vctrs
#' @importFrom utils head tail
#' @examples
#'
#' gdp_vector <- defined(
#'   c(3897, 7365, 6753),
#'   label = "Gross Domestic Product",
#'   unit = "million dollars",
#'   concept = "http://data.europa.eu/83i/aa/GDP"
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
                    concept = NULL,
                    namespace = NULL,
                    ...) {
  dots <- list(...)
  if (!is.null(dots$definition)) {
    warning("`definition` is deprecated; please use `concept` instead.", call. = FALSE)
    if (is.null(concept)) {
      concept <- dots$definition
    }
  }

  if (!is.null(unit) && (!is.character(unit) || length(unit) != 1)) {
    stop("`unit` must be a single character string or NULL", call. = FALSE)
  }
  if (!is.null(namespace) && (!is.character(namespace) || is.null(names(namespace)) && length(namespace) != 1)) {
    stop("`namespace` must be a named character vector or a single string", call. = FALSE)
  }

  if (is.numeric(x)) {
    x <- vctrs::vec_data(x)
    labels <- vec_cast_named(labels, x, x_arg = "labels", to_arg = "x")
    return(new_labelled_defined(x,
      labels = labels,
      label = label,
      unit = unit,
      concept = concept,
      namespace = namespace
    ))
  }

  if (is.character(x)) {
    return(new_labelled_defined(x,
      labels = labels,
      label = label,
      unit = unit,
      concept = concept,
      namespace = namespace
    ))
  }

  if (inherits(x, "Date")) {
    return(new_datetime_defined(x,
      label = label,
      unit = unit,
      concept = concept,
      namespace = namespace
    ))
  }

  if (is.factor(x)) {
    labelled_x <- to_labelled(x)
    var_unit(labelled_x) <- unit
    var_concept(labelled_x) <- concept
    var_namespace(labelled_x) <- namespace
    class(labelled_x) <- c("haven_labelled_defined", class(labelled_x))
    return(labelled_x)
  }

  if (is.labelled(x)) {
    var_unit(x) <- unit
    var_concept(x) <- concept
    var_namespace(x) <- namespace
    class(x) <- c("haven_labelled_defined", class(x))
    return(x)
  }

  stop("Unsupported input type to defined().", call. = FALSE)
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
vec_cast.double.haven_labelled_defined <- function(x, to, ...) vctrs::vec_data(x)

#' @keywords internal
vec_cast.character.haven_labelled_defined <- function(x, to, ...) vctrs::vec_data(x)

#' From haven
#' @keywords internal
#' @importFrom vctrs vec_cast
vec_cast_named <- function(x, to, ...) {
  stats::setNames(vctrs::vec_cast(x, to, ...), names(x))
}

#' @importFrom tibble new_tibble
#' @importFrom haven labelled
#' @keywords internal
new_labelled_defined <- function(x = double(),
                                 labels = NULL,
                                 label = NULL,
                                 unit = NULL,
                                 concept = NULL,
                                 namespace = NULL) {
  if (!is.null(unit) && (!is.character(unit) || length(unit) != 1)) {
    stop("defined(..., unit): 'unit' must be a character vector of length one.")
  }

  if (!is.null(concept) && (!is.character(concept) || length(concept) != 1)) {
    stop("defined(..., defintion): 'concept' must be a character vector of length one or NULL.")
  }

  if (!is.null(label) && (!is.character(label) || length(label) != 1)) {
    stop("defined(..., label): 'label' must be a character vector of length one or NULL.")
  }

  if (!is.null(namespace) && (!is.character(namespace) || length(namespace) != 1)) {
    stop("defined(..., namespace): 'namespace' must be a character vector of length one or NULL.")
  }

  tmp <- haven::labelled(x, labels = labels, label = label)

  attr(tmp, "unit") <- unit
  attr(tmp, "concept") <- concept
  attr(tmp, "namespace") <- namespace
  attr(tmp, "class") <- c("haven_labelled_defined", class(tmp))

  tmp
}

#' @importFrom tibble new_tibble
#' @keywords internal
new_datetime_defined <- function(x,
                                 label = NULL,
                                 unit = NULL,
                                 concept = NULL,
                                 namespace = NULL) {
  if (!is.null(unit) && (!is.character(unit) || length(unit) != 1)) {
    stop("defined(..., unit): 'unit' must be a character vector of length one.")
  }

  if (!is.null(concept) && (!is.character(concept) || length(concept) != 1)) {
    stop("defined(..., defintion): 'concept' must be a character vector of length one.")
  }

  if (!is.null(label) && (!is.character(label) || length(label) != 1)) {
    stop("defined(..., label): 'label' must be a character vector of length one.")
  }

  if (!is.null(namespace) && (!is.character(namespace) || length(namespace) != 1)) {
    stop("defined(..., namespace): 'namespace' must be a character vector of length one or NULL.")
  }

  tmp <- x

  attr(tmp, "unit") <- unit
  attr(tmp, "concept") <- concept
  attr(tmp, "namespace") <- namespace
  attr(tmp, "class") <- c("datetime_defined", class(tmp))

  tmp
}


## Subsetting ------------------------------------------------------

#' @export
`[.haven_labelled_defined` <- function(x, i, ...) {
  result <- NextMethod("[")
  most_attrs <- c("label", "unit", "concept", "namespace", "labels")
  for (attr_name in most_attrs) {
    attr(result, attr_name) <- attr(x, attr_name)
  }
  class(result) <- class(x)
  result
}

#' @export
#' @importFrom vctrs vec_data
`[[.haven_labelled_defined` <- function(x, i, ...) {
  defined(vctrs::vec_data(x)[[i]],
    label = var_label(x),
    unit = var_unit(x),
    concept = var_concept(x),
    namespace = var_namespace(x),
    labels = attr(x, "labels")
  )
}


#' @export
#' @importFrom vctrs vec_data
Ops.haven_labelled_defined <- function(e1, e2) {
  # Comparisons work as expected
  lhs <- if (inherits(e1, "haven_labelled_defined")) vctrs::vec_data(e1) else e1
  rhs <- if (inherits(e2, "haven_labelled_defined")) vctrs::vec_data(e2) else e2
  .Generic <- .Generic
  do.call(.Generic, list(lhs, rhs))
}

#' @export
#' @importFrom vctrs vec_data
length.haven_labelled_defined <- function(x) {
  length(vctrs::vec_data(x))
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


## Print & Summary --------------------------------------------------

#' @export
print.haven_labelled_defined <- function(x, ...) {
  has_def <- !is.null(var_concept(x)) && !is.na(var_concept(x)) && nzchar(var_concept(x))
  has_unit <- !is.null(var_unit(x)) && !is.na(var_unit(x)) && nzchar(var_unit(x))
  has_label <- !is.null(var_label(x))

  cat(deparse(substitute(x)))

  if(has_label) cat(paste0(": ", var_label(x)))
  cat("\n")

  if (has_def && has_unit) {
    msg <- paste0("Defined as ", var_concept(x), ", measured in ", var_unit(x))
  } else if (has_def) {
    msg <- paste0("Defined as ", var_concept(x))
  } else if (has_unit) {
    msg <- paste0("Measured in ", var_unit(x))
  } else {
    msg <- "Defined vector"
  }

  cat(msg, "\n")
  print(vctrs::vec_data(x), ...)
  invisible(x)
}


#' @export
format.haven_labelled_defined <- function(x, ...) {
  base <- format(vec_data(x), ...)
  unit <- var_unit(x)
  def <- var_concept(x)

  if (!is.null(unit) && nzchar(unit)) {
    suffix <- paste0(" (", unit, ")")
  } else if (!is.null(def) && nzchar(def) && nchar(def) < 30) {
    suffix <- paste0(" [", def, "]")
  } else {
    suffix <- ""
  }

  paste0(base, suffix)
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


## Coercion ----------------------------------------------------------

#' @export
as.list.haven_labelled_defined <- function(x, ...) {
  lapply(seq_along(x), function(i) x[[i]])
}

#' @export
as.vector.haven_labelled_defined <- function(x, mode = "any") {
  as.vector(vctrs::vec_data(x), mode = mode)
}

#' @title Strip the class from a defined vector
#' @description Converts a `defined` vector to a base R numeric or character,
#' retaining metadata as passive attributes.
#' @param x A `defined` vector.
#' @return A base R vector with attributes (`label`, `unit`, etc.) intact.
#' @seealso [as_numeric()], [as_character()]
#' @examples
#' gdp <- defined(c(3897L, 7365L), label = "GDP", unit = "million dollars")
#' strip_defined(gdp)
#'
#' fruits <- defined(c("apple", "avocado", "kiwi"),
#'   label = "Fruit", unit = "kg"
#' )
#' strip_defined(fruits)
#' @export
strip_defined <- function(x) {
  if (!inherits(x, "haven_labelled_defined")) {
    return(x)
  }

  base_class <- typeof(vctrs::vec_data(x)) # typically "double" or "integer"
  class(x) <- base_class
  x
}


## Numeric vectors --------------------------------------------------------
as.numeric <- function(x, ...) {
  UseMethod("as.numeric")
}

#' @rdname as_numeric
#' @description Base R's `as.numeric()` does not support custom classes like
#'   `defined`. Calling `as.numeric()` on a `defined` vector will drop all
#'   metadata and class information, which equals to
#'   `as_numeric(x, preserve_attributes = FALSE)`.
#' @export
#' @seealso \code{\link{as_numeric}}
#' @exportS3Method
as.numeric.haven_labelled_defined <- function(x, ...) {
  unclass(vctrs::vec_data(x))
}

#' @title Coerce a defined vector to numeric
#' @param x A vector created with \code{\link{defined}}.
#' @description `as_numeric()` is the recommended method to convert a `defined`
#' vector to numeric. It is metadata-aware and ensures that the underlying data
#' is numeric before coercion.
#' @details `as_numeric()` allows `preserve_attributes = TRUE` when the
#'   resulting vector will retain relevant metadata such as the `unit`,
#'   `concept`, and `namespace` attributes, but it will no longer be of class
#'   `defined`. If `preserve_attributes = FALSE` (default), a plain numeric
#'   vector is returned with all metadata and class dropped.\cr For
#'   character-based `defined` vectors, `as_numeric()` will throw an informative
#'   error to prevent accidental coercion of non-numeric data.
#' @return A numeric vector.
#' @examples
#' as_numeric(orange_df$age, preserve_attributes = TRUE)
#' @export
as_numeric <- function(x, ...) {
  UseMethod("as_numeric", x)
}

#' @rdname as_numeric
#' @param preserve_attributes Defaults to \code{FALSE}, in which case the
#' \code{unit}, \code{concept} and \code{namespace} attributes will be
#' preserved, but the returned value will otherwise become a base R integer,
#' double or numeric value. If false, then the effect will be similar to
#' \code{\link{strip_defined}}.
#' @param ... Further arguments passed to internal methods (not used).
#' @importFrom vctrs vec_data
#' @seealso [strip_defined()]
#' @examples
#' gdp <- defined(c(3897L, 7365L), label = "GDP", unit = "million dollars")
#' gdp_numbers <- as_numeric(gdp)
#' gdp_numbers
#' attributes(gdp_numbers)
#'
#' gdp_striped <- as_numeric(gdp, preserve_attributes = FALSE)
#' attributes(gdp_striped)
#' @export
as_numeric.haven_labelled_defined <- function(
    x, preserve_attributes = FALSE,
    ...) {
  if (!is.numeric(vctrs::vec_data(x))) {
    stop("as_numeric(): underlying data is not numeric.")
  }

  if (preserve_attributes) {
    strip_defined(x)
  } else {
    vec_data(x)
  }
}

## Character vectors --------------------------------------------------
#' @rdname as_character
as.character <- function(x, ...) {
  UseMethod("as.character")
}

#' @rdname as_character
#' @description Base R's `as.character()` does not support custom classes like
#'   `defined`. Calling `as.character()` on a `defined` vector will drop all
#'   metadata and class information, which equals to
#'   `as_character(x, preserve_attributes = FALSE)`.
#' @importFrom haven as_factor
#' @importFrom vctrs vec_data
#' @examples
#' as.character(defined(c("a", "b", "c"), label = "Letter code"))
#' @export
as.character.haven_labelled_defined <- function(x, ...) {
  unclass(vctrs::vec_data(x))
}

#' @title Coerce to character vector
#' @param x A vector created with \code{\link{defined}}.
#' @return A character vector.
#' @examples
#' as_character(defined(c("a", "b", "c"), label = "Letter code"))
#' @export

as_character <- function(x, ...) {
  UseMethod("as_character", x)
}

#' @rdname as_character
#' @title Coerce a defined vector to character
#' @param x A vector created with \code{\link{defined}}.
#' @description `as_character()` is the recommended method to convert a `defined`
#' vector to character. It is metadata-aware and ensures that the underlying data
#' is character before coercion.
#' @details `as_character()` uses `preserve_attributes = TRUE`, the resulting
#'   vector will retain relevant metadata such as the `unit`, `concept`, and
#'   `namespace` attributes, but it will no longer be of class `defined`. If
#'   `preserve_attributes = FALSE` (default), a plain character vector is
#'   returned with all metadata and class dropped.\cr\cr For numeric-based
#'   `defined` vectors, `as_character()` will throw an informative error to
#'   prevent accidental coercion of non-numeric data. \cr\cr
#'   `as.character()` will give a warning that `as_character()` is the
#'   preferred method.
#' @param preserve_attributes Defaults to \code{FALSE}. If set to \code{TRUE},
#'   in which case the \code{unit}, \code{concept} and \code{namespace}
#'   attributes will be preserved, but the returned value will otherwise become
#'   a base R character vector. If false, then the effect will be similar to
#'   \code{\link{strip_defined}}.
#' @param ... Further arguments passed to internal methods (not used).
#' @importFrom vctrs vec_data
#' @seealso [strip_defined()]
#' @examples
#' fruits <- defined(c("apple", "avocado", "kiwi"), label = "Fruit", unit = "kg")
#' # Keep the metadata, but revert to base R character type:
#' as_character(fruits, preserve_attributes = TRUE)
#'
#' # Revert back to base R character type, and do not keep the metadata:
#' as_character(fruits, preserve_attributes = FALSE)
#' @export
#' @export
as_character.haven_labelled_defined <- function(
    x,
    preserve_attributes = FALSE,
    ...) {

  tmp <- as.character(vctrs::vec_data(x))
  if (preserve_attributes) {
    attr(tmp, "unit") <- attr(x, "unit")
    attr(tmp, "concept") <- attr(x, "concept")
    attr(tmp, "namespace") <- attr(x, "namespace")
  }

  tmp
}

## Factor vectors ----------------------------------------------------------
#' @title Coerce to factor vector
#' @param x A vector created with \code{\link{defined}}.
#' @param ... Further arguments passed to internal methods (not used).
#' @return A factor vector.
#' @examples
#' sex <- defined(
#'   c(0, 1, 1, 0),
#'   label = "Sex",
#'   labels = c("Female" = 0, "Male" = 1)
#' )
#' as_factor(sex)
#' @export

as_factor <- function(x, ...) {
  UseMethod("as_factor", x)
}

#' @export
#' @importFrom haven as_factor
#' @importFrom vctrs vec_data
as_factor.haven_labelled_defined <- function(x, ...) {
  haven::as_factor(haven::labelled(vctrs::vec_data(x),
                                   labels = attr(x, "labels")
  ), ...)
}


## Combinations -----------------------------------------------

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
  concepts <- unlist(lapply(dots, var_concept))
  namespaces <- unlist(lapply(dots, namespace_attribute))

  all.identical <- function(l) all(mapply(identical,
                                          head(l, 1),
                                          tail(l, -1)))

  if (length(unique(as.character(var_labels))) > 1) {
    stop("c.haven_labelled_defined(x,y): x,y must have no var_label or the same var_label.")
  }

  if (length(unique(as.character(units))) > 1) {
    stop("c.haven_labelled_defined(x,y): x,y must have no unit or the same unit.")
  }

  if (length(unique(as.character(concepts))) > 1) {
    stop("c.haven_labelled_defined(x,y): x,y must have no concept definition or the same concept definition.")
  }

  if (length(unique(as.character(namespaces))) > 1) {
    stop("c.haven_labelled_defined(x,y): x,y must have no namespace or the same namespace.")
  }

  if (!all.identical(val_labels)) {
    stop("c.haven_labelled_defined(x,y): x,y must have no value labels or the same value labels.")
  }

  defined(unname(do.call(c, lapply(dots, function(x) vctrs::vec_data(x)))),
    label = var_labels[[1]],
    labels = val_labels[[1]],
    concept = concepts[[1]],
    namespace = namespaces[[1]],
    unit = units[[1]]
  )
}

#' @importFrom pillar type_sum
#' @export
type_sum.haven_labelled_defined <- function(x) {
 "defined"
}
