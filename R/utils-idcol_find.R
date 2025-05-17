#' @keywords internal
idcol_find <- function(x, idcol) {
  if (is.null(idcol)) {
    stop("idcol_find: 'idcol' must be specified.")
  }

  # Case: character column name
  if (is.character(idcol)) {
    if (!idcol %in% names(x)) {
      stop(sprintf("idcol_find: column '%s' not found in data frame.", idcol))
    }
    selected <- which(names(x) == idcol)

    # Case: numeric column index
  } else if (is.numeric(idcol)) {
    if (!idcol %in% seq_along(x)) {
      stop(sprintf("idcol_find: column index %s is out of bounds.", idcol))
    }
    selected <- idcol

    # Unsupported type
  } else {
    stop("idcol_find: 'idcol' must be a column name (character) or index (numeric).")
  }

  if (length(selected) != 1) {
    stop("idcol_find: 'idcol' must refer to exactly one column.")
  }

  selected
}
