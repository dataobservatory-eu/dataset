#' @title Bind strictly defined rows
#' @description
#' Add rows of the y dataset to the x dataset.
#' @details
#' By default, the dataset_bibentry bibliographical data and the
#' title is recycled from dataset x. You can give a new title with
#' ..., title="New Title". \cr
#' By default, the unique creators of dataset y, who are not present in
#' dataset x, are added to the creators of the new dataset.
#' @param x A dataset created with \code{\link{dataset_df}}.
#' @param y A dataset created with \code{\link{dataset_df}}.
#' @param ... Optional parameters: \code{dataset_title}, \code{creator}.
#' @return A dataset with the combined rows.
#' @export
#' @examples
#' A <- dataset_df(
#'   a = defined(c(11, 14, 16), label = "length", unit = "cm"),
#'   dataset_bibentry = dublincore(
#'     title = "Test", creator = person("Jane Doe"),
#'     dataset_date = Sys.Date()
#'   )
#' )
#'
#' B <- dataset_df(
#'   a = defined(c(12, 17, 19), label = "length", unit = "cm"),
#'   dataset_bibentry = dublincore(
#'     title = "Test", creator = person("Jane Doe")
#'   )
#' )
#' bind_defined_rows(x = A, y = B)
#'
bind_defined_rows <- function(x, y, ...) {
  dots <- list(...)

  if (!inherits(x, "dataset_df")) {
    stop("Error: bind_defined_rows(x,y): x must be a dataset_df.")
  }

  if (!inherits(y, "dataset_df")) {
    stop("Error: bind_defined_rows(x,y): y must be a dataset_df.")
  }

  if (any(names(x) != names(y))) {
    stop("Error: bind_defined_rows(x,y): x,y must have the same names.")
  }

  if (any(as.character(var_label(x)) != as.character(var_label(y)))) {
    unmatched <- as.character(var_label(x))[
      which(as.character(var_label(x)) != as.character(var_label(y)))
    ]
    stop("Error: bind_defined_rows(x,y): ", unmatched, " has different labels.")
  }

  if (!identical(lapply(x, namespace_attribute), lapply(y, namespace_attribute))) {
    a <- unlist(lapply(x, namespace_attribute))
    b <- unlist(lapply(x, namespace_attribute))
    unmatched <- a[a != b]
    stop("Error: bind_defined_rows(x,y): ", unmatched, " has different namespace.")
  }

  if (dim(x)[2] != dim(y)[2]) {
    error_msg <- paste0(
      "bind_defined_rows(x,y): x has ",
      dim(x)[2], ", but y has ",
      dim(y)[2], " columns."
    )
    stop(error_msg)
  }

  for (i in seq_along(x)) {
    if (i == 1) next
    if (i == 2) {
      new_dataset <- dataset_df(c(x[[i]], y[[i]]))
      names(new_dataset)[2] <- names(x)[2]
    } else {
      new_col <- c(x[[i]], y[[i]])
      new_dataset$new_col <- new_col
      names(new_dataset)[i] <- names(x)[i]
    }
  }

  attr(new_dataset, "dataset_bibentry") <- attr(x, "dataset_bibentry")

  if (!is.null(dots$creator)) {
    # Otherwise the x dataset bibentry lives in the join
    creator(new_dataset) <- dots$creator
  }

  if (!is.null(dots$title)) dataset_title(new_dataset, overwrite = TRUE) <- dots$title

  namespace_attribute(new_dataset$rowid) <- namespace_attribute(x$rowid)
  new_dataset
}

#' @keywords internal
compare_creators <- function(x, y) {
  given_names <- c(creator(x)$given, creator(y)$given)
  family_names <- c(creator(x)$family, creator(y)$family)
  comments <- c(creator(x)$comment, creator(y)$comment)
  roles <- c(creator(x)$role, creator(y)$role)
  emails <- c(creator(x)$email, creator(y)$email)

  for (i in seq_along(given_names)) {
    for (j in seq_along(given_names)) {
      if (i == j) next
    }

    all_the_same <- all(
      given_names[i] == given_names[j] &&
        family_names[i] == family_names[j] &&
        comments[i] == comments[j]
    )

    if (is.na(all_the_same)) next

    if (all_the_same) {
      given_names <- given_names[-j]
      family_names <- family_names[-j]
      comments <- comments[-j]
      roles <- roles[-j]
      emails <- emails[-j]
    }
  }

  for (i in seq_along(given_names)) {
    if (i == 1) {
      persons <- person(
        given = given_names[i],
        family = family_names[i],
        comment = comments[i],
        role = roles[i],
        email = emails[i]
      )
      next
    }
    persons <- c(
      persons,
      person(
        given = given_names[i],
        family = family_names[i],
        comment = comments[i],
        role = roles[i],
        email = emails[i]
      )
    )
  }
}
