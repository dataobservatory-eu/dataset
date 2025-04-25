#' @keywords internal
fix_publisher <- function(publishers) {
  if (is.null(publishers)) {
    return(":unas")
  }

  if (inherits(publishers, "person")) {
    full_names <- vapply(as.list(publishers), function(p) {
      if (!is.null(p$family) && nzchar(p$family)) {
        paste(p$given, p$family)
      } else {
        p$given
      }
    }, character(1))

    if (length(full_names) > 1) {
      return(paste0("{", paste(full_names, collapse = "} and {"), "}"))
    } else {
      return(full_names)
    }
  }

  if (is.character(publishers)) {
    return(publishers)
  }

  stop("fix_publisher(): Unsupported publisher type")
}
