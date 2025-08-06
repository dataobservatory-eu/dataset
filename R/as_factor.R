#' Coerce a defined vector to a factor
#'
#' Converts a [`defined()`][defined] vector with value labels into a
#' factor using [haven::as_factor()]. This allows categorical `defined`
#' vectors to behave like standard factors in models and plotting.
#'
#' @param x A vector created with [defined()].
#' @param ... Reserved for future extensions; not used.
#'
#' @return A factor vector with levels derived from the value labels.
#'
#' @examples
#' sex <- defined(
#'   c(0, 1, 1, 0),
#'   label = "Sex",
#'   labels = c("Female" = 0, "Male" = 1)
#' )
#' as_factor(sex)
#'
#' @export
as_factor <- function(x, ...) {
  UseMethod("as_factor")
}

#' @export
#' @importFrom haven as_factor labelled
#' @importFrom vctrs vec_data
as_factor.haven_labelled_defined <- function(x, ...) {
  haven::as_factor(
    haven::labelled(
      vctrs::vec_data(x),
      labels = attr(x, "labels")
    ),
    ...
  )
}
