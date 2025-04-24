#' @title Bind strictly defined rows
#' @description Add rows of dataset y to dataset x. Metadata (labels, units,
#' definitions, namespaces) must match exactly. The combined dataset inherits
#' x's citation by default, and creator roles are merged.
#' @details
#' This function combines two semantically enriched datasets created with \code{dataset_df()}.
#' It ensures that the structure and metadata of the datasets are compatible.
#'
#' All variable-level attributes — including variable labels, measurement units,
#' linked definitions, and namespaces — must match exactly in both datasets.
#'
#' If \code{strict = TRUE} (the default), the row identifier namespace
#' (used in the \code{rowid} column) must also be identical in both datasets.
#' If \code{strict = FALSE}, this requirement is relaxed: the resulting dataset
#' inherits the row ID structure from \code{x}, even if \code{y} used a different one.
#' @param strict Logical. If \code{TRUE} (default), all variables must match
#'   exactly in both datasets, including variable labels, units, definitions,
#'   namespaces, and the dataset-level row identifier namespace. If
#'   \code{FALSE}, only user-defined variable semantics are checked strictly,
#'   while the row identifier column is inherited from \code{x} without
#'   requiring an exact match.
#' @export
#' @examples
#' A <- dataset_df(
#'   length = defined(c(10, 15), label = "Length", unit = "cm", namespace = "http://example.org"),
#'   identifier = c(id = "http://example.org/dataset#"),
#'   dataset_bibentry = dublincore(title = "Dataset A", creator = person("Alice", "Smith"))
#' )
#'
#' B <- dataset_df(
#'   length = defined(c(20, 25), label = "Length", unit = "cm", namespace = "http://example.org"),
#'   identifier = c(id = "http://example.org/dataset#")
#' )
#'
#' # This works: same structure and rowid namespace
#' bind_defined_rows(A, B)
#'
#' C <- dataset_df(
#'   length = defined(c(30, 35), label = "Length", unit = "cm", namespace = "http://example.org"),
#'   identifier = c(id = "http://another.org/dataset#")
#' )
#'
#' # This throws an error because the rowid namespace differs
#' \dontrun{
#' bind_defined_rows(A, C, strict = TRUE)
#' }
#'
#' # This succeeds because rowid is inherited from A
#' bind_defined_rows(A, C, strict = FALSE)
bind_defined_rows <- function(x, y, ..., strict=FALSE) {
  dots <- list(...)

  if (!inherits(x, "dataset_df")) {
    stop("`x` must be a dataset_df object. Got: ",
         paste(class(x), collapse = ", "))
  }

  if (!inherits(y, "dataset_df")) {
    stop("`y` must be a dataset_df object. Got: ",
         paste(class(y), collapse = ", "))
  }


  # Strict mode: also validate rowid identifier
  if (strict) {
    ns_x <- namespace_attribute(x[["rowid"]])
    ns_y <- namespace_attribute(y[["rowid"]])

    if (!identical(ns_x, ns_y)) {
      stop("Row identifier namespaces must match in strict mode.")
    }
  }

  # Exclude rowid from semantic checks
  vars <- setdiff(names(x), "rowid")

  if (!identical(names(x)[vars], names(y)[vars])) {
    stop("Error: Column names must match between datasets.")
  }

  if (!identical(var_label(x[vars]), var_label(y[vars]))) {
    stop("Error: Variable labels must match in the two datasets.")
  }

  if (!identical(lapply(x[vars], var_unit), lapply(y[vars], var_unit))) {
    stop("Error: Variable units must match in the two datasets.")
  }

  if (!identical(
    lapply(x[vars], function(i) attr(i, "definition")),
    lapply(y[vars], function(j) attr(j, "definition"))
  )) {
    stop("Variable definitions must match in the two datasets.")
  }

  if (!identical(
    lapply(x[vars], namespace_attribute),
    lapply(y[vars], namespace_attribute))
  ) {
    stop("Variable namespaces must match in the two datasets.")
  }

  # Bind only variable columns (exclude rowid)
  new_data <- mapply(function(a, b) c(a, b), x[vars], y[vars], SIMPLIFY = FALSE)

  # Use the rowid namespace from x; allow override later if needed
  new_dataset <- do.call(dataset_df, c(new_data, list(identifier = namespace_attribute(x[["rowid"]]))))

  # Handle title/creator if supplied
  if (!is.null(dots$title)) dataset_title(new_dataset, overwrite = TRUE) <- dots$title
  if (!is.null(dots$creator)) {
    creator(new_dataset) <- dots$creator
  } else {
    creator(new_dataset) <- compare_creators(x, y)
  }

  attr(new_dataset, "dataset_bibentry") <- attr(x, "dataset_bibentry")
  new_dataset
}

