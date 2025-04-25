#' @title Format contributor list into citation-friendly string
#' @description
#' Given a list of contributors (utils::person objects), formats them into
#' a LaTeX/BibLaTeX-like string, merging roles and safely formatting names.
#'
#' Contributors without explicit roles are assumed to have role "ctb".
#' If input is NULL or ":unas", ":unas" is returned.
#'
#' @param contributors A person object or character vector or NULL.
#' @return A character string like "{Jane Doe [dtm, ctb]} and {John Smith [ctb]}".
#' @keywords internal
fix_contributor <- function(contributors = NULL) {

  if (is.null(contributors) || (is.character(contributors) && contributors == ":unas")) {
    return(":unas")
  }

  if (!inherits(contributors, "person")) {
    stop("fix_contributor(): contributors must be of class 'person' or the special string ':unas'.")
  }

  contributors_list <- as.list(contributors)

  # Build identity keys: "given|family"
  identity_keys <- vapply(contributors_list, function(p) {
    paste0(p$given %||% "", "|", p$family %||% "")
  }, character(1))

  unique_keys <- unique(identity_keys)
  merged <- vector("character", length(unique_keys))

  for (i in seq_along(unique_keys)) {
    key <- unique_keys[i]
    matches <- contributors_list[identity_keys == key]

    # Collect roles (fill with "ctb" if missing)
    roles <- unique(unlist(lapply(matches, function(p) {
      if (is.null(p$role) || length(p$role) == 0 || all(!nzchar(p$role))) {
        "ctb"
      } else {
        p$role
      }
    })))

    base <- matches[[1]]

    # Build full name safely
    name_parts <- c(base$given, base$family)
    name_parts <- name_parts[!is.null(name_parts) & nzchar(name_parts)]
    full_name <- paste(name_parts, collapse = " ")

    # Final contributor formatting
    if (length(roles) > 0) {
      contributor_entry <- paste0("{", full_name, " [", paste(roles, collapse = ", "), "]}")
    } else {
      contributor_entry <- paste0("{", full_name, "}")
    }

    merged[[i]] <- contributor_entry
  }

  # Join all contributors with 'and'
  paste(merged, collapse = " and ")
}

