#' @title Create a modern bibrecord-compatible metadata object
#' @description Create a `utils::bibentry`-compatible object extended with
#'   standard Dublin Core and DataCite-compatible fields. This serves as a
#'   unified metadata structure for use in both `dublincore()` and `datacite()`
#'   functions.
#'
#' @param title A character string, the dataset title.
#' @param author A list or vector of `utils::person` objects. Mapped to
#'   `creator` in DataCite and DCTERMS.
#' @param contributor Optional list/vector of `utils::person`. Contributor roles
#'   are merged if repeated.
#' @param publisher Character string or `person`. The publishing entity.
#' @param year Publication year. Derived from `date` if not explicitly provided.
#' @param date A character string or `Date` object.
#' @param identifier Unique identifier (e.g., DOI).
#' @param subject Optional keyword(s) or controlled vocabulary string.
#' @param ... Additional fields (e.g., language, format, rights, description).
#' @importFrom utils bibentry
#' @return An object of class `bibrecord` and `bibentry`. bibrecord(title =
#'   "Gross domestic product, volumes", author = person("Eurosat"), publisher =
#'   person("Eurostat"), identifier = "https://doi.org/10.2908/TEINA011", date =
#'   as.Date("2025-05-20"))
#' @export
bibrecord <- function(
    title,
    author,
    contributor = NULL,
    publisher = NULL,
    year = NULL,
    date = Sys.Date(),
    identifier = NULL,
    subject = NULL,
    ...) {

  stopifnot(inherits(author, "person") ||
              all(vapply(author, inherits, logical(1), "person"))
            )

  # Normalize author roles and ensure vector of class "person"
  author <- normalize_roles(as.list(author))
  author <- do.call("c", author)

  # Normalize contributor if present
  if (!is.null(contributor)) {
    if (inherits(contributor, "person")) {
      contributor <- list(contributor)
    }
    contributor <- normalize_roles(as.list(contributor))
    contributor <- do.call("c", contributor)
  }

  # Infer year from date if missing
  year <- year %||% substr(as.character(date), 1, 4)

  meta <- bibentry(
    bibtype = "Misc",
    title = title,
    author = author,
    year = year,
    date = as.character(date),
    identifier = identifier,
    publisher = publisher,
    subject = subject,
    ...
  )

  if (!is.null(contributor)) {
    attr(meta, "contributor") <- contributor
  }

  class(meta) <- c("bibrecord", class(meta))
  meta
}

#' @keywords internal
`%||%` <- function(a, b) {
  if (!is.null(a)) a else b
}

#' @export
print.bibrecord <- function(x, ...) {
  NextMethod("print")

  contributors <- attr(x, "contributor")
  if (!is.null(contributors)) {
    cat("\nContributors:\n")
    cat(fix_contributor(contributors), "\n")
  }

  invisible(x)
}
