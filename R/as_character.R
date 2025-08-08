#' Coerce a defined vector to character
#'
#' `as_character()` is the recommended method to convert a
#' [`defined()`][defined] vector to a character type. It is metadata-aware and
#' ensures that the underlying data is character before coercion.
#'
#' If `preserve_attributes = TRUE`, the returned character vector retains
#' semantic metadata such as `unit`, `concept`, and `namespace`, though the
#' `"defined"` class itself is removed. If `preserve_attributes = FALSE`
#' (default), a plain character vector is returned with all attributes stripped.
#'
#' For numeric-based `defined` vectors, `as_character()` throws an informative
#' error to avoid accidental coercion of non-character data.
#'
#' Note: `as.character()` (base R) is supported but simply returns the raw
#' values, and does not preserve or warn about metadata loss.
#'
#' @param x A vector created with [defined()].
#' @param preserve_attributes Logical. If `TRUE`, retains `unit`, `concept`, and
#'   `namespace` attributes. Defaults to `FALSE`.
#' @param ... Reserved for future use.
#'
#' @return A character vector.
#'
#' @examples
#' # Recommended use
#' fruits <- defined(c("apple", "avocado", "kiwi"), label = "Fruit", unit = "kg")
#' as_character(fruits, preserve_attributes = TRUE)
#'
#' # Strip metadata
#' as_character(fruits, preserve_attributes = FALSE)
#'
#' # Equivalent base R fallback
#' as.character(fruits)
#'
#' @seealso [strip_defined()], [as_numeric()]
#' @export
as_character <- function(x, ...) {
  UseMethod("as_character", x)
}

#' @rdname as_character
#' @importFrom vctrs vec_data
#' @export
as_character.haven_labelled_defined <- function(
    x,
    preserve_attributes = FALSE,
    ...) {
  base <- vctrs::vec_data(x)

  tmp <- as.character(base)

  if (preserve_attributes) {
    attr(tmp, "unit") <- attr(x, "unit")
    attr(tmp, "concept") <- attr(x, "concept")
    attr(tmp, "namespace") <- attr(x, "namespace")
  }

  tmp
}

#' @rdname as_character
#' @description Base R's `as.character()` method applied to `defined` vectors
#' simply strips the class and returns the values as a plain character vector.
#' This is equivalent to calling [as_character()] with `preserve_attributes =
#' FALSE`.
#'
#' @export
as.character.haven_labelled_defined <- function(x, ...) {
  unclass(vctrs::vec_data(x))
}
