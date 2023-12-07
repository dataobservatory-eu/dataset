#' @title Get / Set a variable labels in a dataset
#' @param x A dataset object created with \code{dataset::\link{dataset}}.
#' @export
var_labels <- function(x) {
  UseMethod("var_labels", x)
}

#' @rdname var_labels
#' @export
set_var_labels <- function(x, value) {
  UseMethod("set_var_labels")
}

#' @rdname var_labels
#' @exportS3Method
var_labels.dataset <- function(x) {
  dsd <- attr(x, "DataStructure", exact = TRUE)
  vapply (names(dsd), function(x) unlist(dsd[[x]]$label), character(1))
}

#' @rdname var_labels
#' @param value A character vector for labelling the variables with
#' \code{set_var_labels}.
#' @examples
#' relabelled <- set_var_labels(
#'                iris_dataset,
#'                 c(Sepal.Length="The sepal length measured in centimeters.",
#'                   Sepal.Width="The sepal width measured in centimeters.",
#'                   Species="The species of the iris observed.")
#'                  )
#' var_labels(relabelled)
#' @exportS3Method
set_var_labels.dataset <- function(x, value) {
  dsd <- attr(x, "DataStructure")

  original_labels <-  vapply (names(dsd), function(x) unlist(dsd[[x]]$label), character(1))
  to_change <- which(  names(dsd) %in% names(value) )
  new_labels <- original_labels

  new_labels <- vapply(seq_along(original_labels),
                       function(x) {
                         ifelse (x %in% to_change,
                                 yes = value[which(names(value)==names(original_labels)[x])],
                                 no = original_labels[x])
                       }, character(1))

  dsd2 <- dsd

  for ( i in seq_along(dsd)) {
    dsd2[[i]]$label <- new_labels[i]
  }

  attr(x, "DataStructure") <- dsd2

  invisible(x)
}
