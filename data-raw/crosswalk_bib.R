#' Crosswalk bibliographic metadata to schema.org (dataspice-ready)
#'
#' @param x A `dublincore` or `datacite` metadata record
#' @param schema One of "schema.org" (default), "dataspice"
#' @return A named list that matches schema.org/Dataset JSON-LD
#' @keywords import
crosswalk_bib <- function(x, target = c("schema.org", "dataspice")) {
  target <- match.arg(target)

  if (!(inherits(x, "dublincore") || inherits(x, "datacite"))) {
    stop("Unsupported metadata class")
  }

  unified <- list(
    title       = x$title,
    description = x$description %||% NA_character_,
    identifier  = x$identifier %||% NA_character_,
    creator     = x$author %||% list(),
    contributor = attr(x, "contributor") %||% list(),
    publisher   = x$publisher %||% NA_character_,
    date        = x$date %||% x$year %||% NA_character_,
    language    = x$language %||% NA_character_,
    relation    = attr(x, "relation") %||% x$relation %||% NA_character_,
    subject     = attr(x, "subject") %||% x$subject %||% NA_character_
  )

  if (target == "schema.org") {
    sch <- list(
      "@context"   = "https://schema.org",
      "@type"      = "Dataset",
      "name"       = unified$title,
      "description"= unified$description,
      "identifier" = unified$identifier,
      "publisher"  = list("@type"="Organization","name"=unified$publisher),
      "datePublished" = unified$date,
      "inLanguage" = unified$language
    )

    # Subject → keywords
    if (is.list(unified$subject) && inherits(unified$subject, "subject")) {
      sch$keywords <- unified$subject$term
    } else if (!is.null(unified$subject) && nzchar(unified$subject)) {
      sch$keywords <- unified$subject
    }

    # Relation → isBasedOn
    if (is.list(unified$relation) && inherits(unified$relation, "related")) {
      sch$isBasedOn <- unified$relation$relatedIdentifier
    } else if (!is.null(unified$relation) && nzchar(unified$relation)) {
      sch$isBasedOn <- unified$relation
    }

    # Creators & contributors as before …
    # (Role objects omitted here for brevity)

    return(sch)
  }

  if (target == "dataspice") {
    ds <- list(
      title       = unified$title,
      description = unified$description,
      id          = unified$identifier,
      creator     = vapply(unified$creator, clean_person_name, character(1)),
      contributor = vapply(unified$contributor, clean_person_name, character(1)),
      publisher   = unified$publisher,
      date        = unified$date,
      language    = unified$language,
      relation    = if (is.list(unified$relation) && inherits(unified$relation,"related")) {
        unified$relation$relatedIdentifier
      } else {
        unified$relation
      },
      subject     = if (is.list(unified$subject) && inherits(unified$subject,"subject")) {
        unified$subject$term
      } else {
        unified$subject
      }
    )

    # wrap in a biblio data.frame to satisfy dataspice convention
    return(list(biblio = as.data.frame(ds, stringsAsFactors = FALSE)))
  }
}


#' @keywords internal
clean_person_name <- function(p) {
  if (inherits(p, "person")) {
    name_parts <- c(p$given, p$family)
    name_parts <- name_parts[nzchar(name_parts)]
    name <- paste(name_parts, collapse = " ")
  } else {
    name <- as.character(p)
  }
  sub("\\s*\\[[^]]+\\]$", "", name)
}


