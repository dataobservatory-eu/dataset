#' Create a semantically well-defined, labelled vector
#'
#' `defined()` constructs a vector enriched with semantic metadata such as a
#' label, unit of measurement, concept URI, and optional namespace.
#' These vectors behave like base R vectors but retain metadata during
#' subsetting, comparison, and printing.
#'
#' The resulting object inherits from [haven::labelled()] and integrates with
#' tidyverse workflows, enabling downstream conversion to RDF and other
#' standards.
#'
#' @param x A vector of type character, numeric, Date, factor, or a `labelled`
#'   object.
#' @param labels An optional named vector of value labels. Only a subset of
#'   values may be labelled.
#' @param label A short human-readable label (string of length 1).
#' @param unit Unit of measurement (e.g., "kg", "hours"). Must be a string of
#'   length 1 or `NULL`.
#' @param concept A URI or concept name representing the meaning of the
#'   variable.
#' @param namespace Optional string or named character vector, used for
#'   value-level URI expansion.
#' @param ... Reserved for future use.
#'
#' @return A vector of class `"defined"` (technically
#' `haven_labelled_defined`), which behaves like a standard vector with
#' additional semantic metadata and is inherited from [haven::labelled()].
#' @importFrom haven labelled
#' @importFrom labelled to_labelled is.labelled
#' @import vctrs
#' @importFrom utils head tail
#' @seealso `browseVignettes("dataset")`
#' @seealso [is.defined()], [as_numeric()], [as_character()], [as_factor()],
#'   [strip_defined()]
#'
#' @examples
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

  if (has_label) cat(paste0(": ", var_label(x)))
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

#' Combine defined vectors with metadata checks
#'
#' The `c()` method for `defined` vectors ensures that all semantic metadata
#' (label, unit, concept, namespace, and value labels) match exactly. This
#' prevents accidental loss or mixing of incompatible definitions during
#' concatenation.
#'
#' All input vectors must:
#' - Have identical `label` attributes
#' - Have identical `unit`, `concept`, and `namespace`
#' - Have identical value labels (or none)
#'
#' @param ... One or more vectors created with [defined()].
#'
#' @return A single `defined` vector with concatenated values and retained
#'   metadata.
#'
#' @examples
#' a <- defined(1:3, label = "Length", unit = "meter")
#' b <- defined(4:6, label = "Length", unit = "meter")
#' c(a, b)
#'
#' @seealso [defined()]
#' @export
c.haven_labelled_defined <- function(...) {
  dots <- list(...)

  var_labels <- unlist(lapply(dots, var_label))
  val_labels <- lapply(dots, function(x) attr(x, "labels"))
  units <- unlist(lapply(dots, var_unit))
  concepts <- unlist(lapply(dots, var_concept))
  namespaces <- unlist(lapply(dots, namespace_attribute))

  all_identical <- function(l) {
    all(mapply(identical, head(l, 1), tail(l, -1)))
  }

  if (length(unique(as.character(var_labels))) > 1) {
    stop("c(): var_label must be identical or NULL across inputs")
  }

  if (length(unique(as.character(units))) > 1) {
    stop("c(): unit must be identical or NULL across inputs")
  }

  if (length(unique(as.character(concepts))) > 1) {
    stop("c(): concept must be identical or NULL across inputs")
  }

  if (length(unique(as.character(namespaces))) > 1) {
    stop("c(): namespace must be identical or NULL across inputs")
  }

  if (!all_identical(val_labels)) {
    stop("c(): value labels must be identical or NULL across inputs")
  }

  defined(
    unname(do.call(c, lapply(dots, vctrs::vec_data))),
    label = var_labels[[1]],
    labels = val_labels[[1]],
    concept = concepts[[1]],
    namespace = namespaces[[1]],
    unit = units[[1]]
  )
}

## Coercion ----------------------------------------------------------
## as_numeric, as_character, as_factor in separate files

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
#'   retaining metadata as passive attributes.
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

#' @importFrom pillar type_sum tbl_sum
#' @export
type_sum.haven_labelled_defined <- function(x) {
  "defined"
}
