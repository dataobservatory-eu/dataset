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
    creator(new_dataset) <- dots$creator
  }

  if (is.null((dots$creator))) {
    creators <- c(creator(x), creator(y))

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
    creator(new_dataset) <- persons
  }

  if (!is.null(dots$title)) dataset_title(new_dataset) <- dots$title
  new_dataset
}
