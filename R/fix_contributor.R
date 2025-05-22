#' @title Format contributor list into citation-friendly string
#' @description Given a list of contributors (`utils::person` objects), this
#' function formats them into a LaTeX/BibLaTeX-compatible string, merging roles
#' per person and formatting names.
#'
#' Contributors without explicit roles are assumed to have the role `"ctb"`. If
#' the input is `NULL` or the special string `":unas"`, the function returns
#' `":unas"`.
#'
#' This formatting is intended for metadata serialization or citation exports.
#'
#' @param contributors A vector of `person` objects, or the character string
#'   `":unas"`, or `NULL`.
#' @return A character string like `{Jane Doe [dtm, ctb]} and {John Smith
#'   [ctb]}`.
#' @examples
#' \dontrun{
#' fix_contributor(person("Jane", "Doe", role = c("dtm", "ctb")))
#' fix_contributor(c(person("Jane", "Doe", role = "ctb"),
#'                   person("John", "Smith")))
#' fix_contributor(":unas")
#' }
#' @keywords internal
fix_contributor <- function(contributors = NULL) {
  if (is.null(contributors) || (is.character(contributors) && length(contributors) == 1 && contributors[[1]] == ":unas")) {
    return(":unas")
  }

  # Normalize single person to a list
  if (inherits(contributors, "person")) {
    contributors_list <- as.list(contributors)
  } else if (is.list(contributors) && all(vapply(contributors, function(p) inherits(p, "person"), logical(1)))) {
    contributors_list <- contributors
  } else {
    return(":unas")
  }

  # Build identity keys: "given|family"
  identity_keys <- vapply(contributors_list, function(p) {
    paste0(p$given %||% "", "|", p$family %||% "")
  }, character(1))

  unique_keys <- unique(identity_keys)
  merged <- vector("character", length(unique_keys))

  for (i in seq_along(unique_keys)) {
    key <- unique_keys[i]
    matches <- contributors_list[identity_keys == key]

    # Collect roles (default to "ctb")
    roles <- unique(unlist(lapply(matches, function(p) {
      if (is.null(p$role) || length(p$role) == 0 || all(!nzchar(p$role))) {
        "ctb"
      } else {
        p$role
      }
    })))

    base <- matches[[1]]
    name_parts <- c(base$given, base$family)
    name_parts <- name_parts[!is.null(name_parts) & nzchar(name_parts)]
    full_name <- paste(name_parts, collapse = " ")

    contributor_entry <- paste0("{", full_name, " [", paste(roles, collapse = ", "), "]}")
    merged[[i]] <- contributor_entry
  }

  paste(merged, collapse = " and ")
}
