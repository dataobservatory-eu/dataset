#' @title Get / Set a variable label
#' @description
#' Add a human readable, easier to understand label as a metadata attribute to a
#' variable or vector than the programmatic vector object name, or column name
#' in the data frame.
#' @details
#' See \code{\link[labelled:var_label]{labelled::var_label}} for details about
#' variable labels.
#' \cr
#' See \code{vignette("defined", package = "dataset")} to use comprehensively
#' with variable labels, namespaces, units of measures, and machine-independent
#' permanent variable identifiers.
#' @name var_label
#' @return \code{var_label()} returns returns the \code{label} attribute as a character
#' string.
#' The \code{var_label<-}
#' assignment method allows to add, remove, or overwrite this attribute on a vector
#' \code{x}. The assignment function returns the \code{x} vector invisibly.
#' @examples
#' # Retrieve the label attribute:
#' var_label(orange_df$circumference)
#'
#' # To (re)set the label attribute:
#' `var_label<-`(orange_df$circumference, "circumference (breast height)")
#' @export
#' @importFrom labelled var_label
#' @importFrom labelled `var_label<-`
#' @usage
#' var_label(x)
#' var_label(x) <- value
#' @param x a vector or a data.frame
#' @family defined metadata methods and functions
#' @param value a character string or `NULL` to remove the label
#'  For data frames, with `var_labels()`, it could also be a named list or a
#'  character vector of same length as the number of columns in `x`.
NULL

#' @rdname var_label
#' @importFrom labelled `var_label<-`
#' @export
var_label.defined <- function(x, ...) {
  NextMethod()
}

#' @rdname var_label
#' @param unlist for data frames, return a named vector instead of a list
#' @param null_action for data frames, by default `NULL` will be returned for
#' columns with no variable label. Use `"fill"` to populate with the column name
#' instead, `"skip"` to remove such values from the returned list, `"na"` to
#' populate with `NA` or `"empty"` to populate with an empty string (`""`).
#' @param recurse if `TRUE`, will apply `var_label()` on packed columns
#' (see [tidyr::pack()]) to return the variable labels of each sub-column;
#' otherwise, the label of the group of columns will be returned.
#' @param ... Further potential parameters reserved for inherited classes.
#' @export
var_label.dataset_df <- function(x,
                                 unlist = FALSE,
                                 null_action =
                                   c("keep", "fill", "skip", "na", "empty"),
                                 recurse = FALSE,
                                 ...) {
  NextMethod()
}


#' @keywords internal
set_var_labels <- function(dataset, var_labels) {
  var_label_list <- list()
  var_label_list <- lapply(colnames(dataset), function(i) i)
  names(var_label_list) <- colnames(dataset)

  for (rn in which(names(var_label_list) %in% names(var_labels))) {
    var_label_list[[rn]] <- var_labels[[which(names(var_label_list)[rn] == names(var_labels))]]
  }

  attr(dataset, "var_labels") <- var_label_list

  dataset
}

#' @rdname var_label
#' @importFrom labelled label_attribute
#' @export
label_attribute <- function(x) {
  attr(x, "label", exact = TRUE)
}

#' @export
`var_label<-` <- function(x, value) {
  UseMethod("var_label<-")
}

#' @rdname var_label
#' @importFrom labelled `var_label<-`
#' @export
`var_label<-.haven_labelled_defined` <- function(x, value) {
  attr(x, "label") <- value
  x
}

#' @rdname var_label
#' @export
`var_label<-.dataset_df` <- function(x, value) {
  NextMethod()
}
