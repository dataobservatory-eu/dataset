#' @title Get / set a concept definition for a vector or a dataset
#' @param x a vector
#' @param value a character string or `NULL` to remove the concept definition of
#'   a measure.
#' @param ... Further parameters for inheritance, not in use.
#' @details `get_variable_concepts()` is identical to `var_concept()`.
#' @examples
#' small_country_dataset <- dataset_df(
#'   country_name = defined(c("Andorra", "Lichtenstein"), label = "Country"),
#'   gdp = defined(c(3897, 7365),
#'     label = "Gross Domestic Product",
#'     unit = "million dollars"
#'   )
#' )
#' var_concept(small_country_dataset$country_name) <- "http://data.europa.eu/bna/c_6c2bb82d"
#' var_concept(small_country_dataset$country_name)
#' # To remove a concept definition of variable
#' var_concept(small_country_dataset$country_name) <- NULL
#' @return The (linked) concept of the meaning of the data contained by a
#'   vector constructed with[defined()].
#' @export

var_concept <- function(x, ...) {
  # rlang::check_dots_used()
  UseMethod("var_concept")
}

#' @export
var_concept.default <- function(x, ...) {
  concept <- attr(x, "concept", exact = TRUE)

  # Fallback to deprecated 'definition' attribute
  if (is.null(concept)) {
    legacy <- attr(x, "definition", exact = TRUE)
    if (!is.null(legacy)) {
      warning("Attribute `definition` is deprecated; please rename to `concept`.", call. = FALSE)
      return(legacy)
    }
  }

  concept
}

#' @rdname var_concept
#' @export
`var_concept<-` <- function(x, value) {
  UseMethod("var_concept<-")
}

#' @rdname var_concept
#' @param x A vector to which the concept URI will be assigned.
#' @param value A character string with a concept URI or NULL to remove the concept.
#' @description Assigns a concept URI to a vector created with `defined()`. This
#' method updates the `concept` attribute and validates that the input is a single
#' character string or NULL.
#' @return The modified vector with updated `concept` metadata.
#' @export
#' @examples
#' x <- defined(c(1, 2, 3), label = "Example Variable")
#' var_concept(x) <- "http://example.org/concept/XYZ"
#' var_concept(x)
`var_concept<-.default` <- function(x, value) {
  if ((!is.character(value) && !is.null(value)) || length(value) > 1) {
    stop("`concept` must be a single character string or NULL", call. = FALSE)
  }
  attr(x, "concept") <- value
  x
}

#' @title Get concepts for all variables in a dataset_df
#' @description Returns a named list of concept URIs (or NULLs) for all
#'   variables.
#' @param x A `dataset_df` object.
#' @return A named list of concept URIs for each variable.
#' @examples get_variable_concepts(orange_df)
#' @export
get_variable_concepts <- function(x) {
  if (!is.dataset_df(x)) {
    stop("get_variable_concepts(x): x must be a dataset_df object.", call. = FALSE)
  }

  lapply(x, var_concept)
}
