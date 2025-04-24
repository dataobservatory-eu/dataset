#' @keywords internal
compare_creators <- function(x, y) {
  creators_x <- creator(x)
  creators_y <- creator(y)
  all_creators <- c(creators_x, creators_y)

  identity_keys <- vapply(all_creators, function(p) {
    paste(
      p$given %||% "",
      p$family %||% "",
      p$email %||% "",
      p$comment %||% "",
      sep = "|"
    )
  }, character(1))

  unique_keys <- unique(identity_keys)
  merged <- vector("list", length(unique_keys))

  for (i in seq_along(unique_keys)) {
    key <- unique_keys[i]
    matches <- all_creators[identity_keys == key]
    roles <- unique(unlist(lapply(matches, function(p) p$role)))
    base <- matches[[1]]

    merged[[i]] <- person(
      given = base$given,
      family = base$family,
      email = base$email,
      comment = base$comment,
      role = roles
    )
  }

  do.call(c, merged)
}
