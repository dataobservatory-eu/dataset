#' @keywords internal
`agent<-` <- function(x, value) {
  return_type <- NULL

  if ("dataset_bibentry" %in% names(attributes(x))) {
    dataset_bibentry <- get_bibentry(x)
    return_type <- "dataset"
  } else if (inherits(x, "dublincore")) {
    dataset_bibentry <- x
    return_type <- "dublincore"
  } else if (inherits(x, "datacite")) {
    dataset_bibentry <- x
    return_type <- "datacite"
  } else {
    stop("Error: agent(x)<- x must be a dataset_df, a dublincore or a datacite object.")
  }

  assertthat::assert_that(all(inherits(value, "person")),
    msg = "Error: agent(x) <- value: value must be a vector of utils::persons."
  )

  creators <- ifelse(is.null(dataset_bibentry$author), ":tba", dataset_bibentry$author)
  publishers <- ifelse(is.null(dataset_bibentry$publisher), ":unas", dataset_bibentry$publisher)
  contributors <- ifelse(is.null(dataset_bibentry$contributor), ":unas", dataset_bibentry$contributor)

  get_creator <- function(x) {
    if (!is.null(x$role)) ifelse("cre" %in% x$role, TRUE, FALSE) else FALSE
  }

  get_author <- function(x) {
    if (!is.null(x$role)) ifelse("aut" %in% x$role, TRUE, FALSE) else FALSE
  }

  get_publisher <- function(x) {
    if (!is.null(x$role)) ifelse("pbl" %in% x$role, TRUE, FALSE) else FALSE
  }

  is_creator <- vapply(value, get_creator, logical(1))
  is_author <- vapply(value, get_author, logical(1))
  is_publisher <- vapply(value, get_publisher, logical(1))


  new_creators <- c(value[is_creator], value[is_author[!is_creator]])
  new_publishers <- c(value[is_publisher])
  new_contributors <- c(value[!value %in% c(creators, publishers)])

  creators <- ifelse(length(new_creators) > 0, new_creators, creators)
  publishers <- ifelse(length(new_publishers) > 0, new_publishers, publishers)
  contributors <- ifelse(length(new_creators) > 0,
                         new_contributors,
                         contributors)

  dataset_bibentry$author <- ifelse(length(new_creators) > 0,
                                    new_creators,
                                    dataset_bibentry[[1]]$author)
  dataset_bibentry$contributor <- contributors
  dataset_bibentry$publisher <- publishers

  if (return_type %in% c("datacite", "dublincore")) {
    dataset_bibentry
  } else if (return_type == "dataset") {
    attr(x, "dataset_bibentry") <- dataset_bibentry
    invisible(x)
  }
}

#' @keywords internal
agent <- function(x) {
  if (inherits(x, "dataset_df")) {
    dataset_bibentry <- get_bibentry(x)
  } else if (inherits(x, "datacite")) {
    dataset_bibentry <- x
    creators <- ifelse(is.null(dataset_bibentry$author), ":tba", dataset_bibentry$author)
    publishers <- ifelse(is.null(dataset_bibentry$publisher), ":unas", dataset_bibentry$publisher)
    contributors <- ifelse(is.null(dataset_bibentry$contributor), ":unas", dataset_bibentry$contributor)
  } else if (inherits(x, "dublincore")) {
    dataset_bibentry <- x
    creators <- ifelse(is.null(dataset_bibentry$author), ":tba", dataset_bibentry$author)
    publishers <- ifelse(is.null(dataset_bibentry$publisher), ":unas", dataset_bibentry$publisher)
    contributors <- ifelse(is.null(dataset_bibentry$contributor), ":unas", dataset_bibentry$contributor)
  } else if (all(inherits(x, "person"))) {
    contributors <- x
    publishers <- x
    creators <- x
    return_type <- "persons_vector"
  } else {
    stop("Error: agent(x)<- x must be a dataset_df, a vector of persons, a dublincore or datacite object.")
  }

  get_creator <- function(x) {
    if (!is.null(x$role)) ifelse("cre" %in% x$role, TRUE, FALSE) else FALSE
  }

  get_author <- function(x) {
    if (!is.null(x$role)) ifelse("aut" %in% x$role, TRUE, FALSE) else FALSE
  }

  get_contributor <- function(x) {
    if (!is.null(x$role)) ifelse("ctb" %in% x$role, TRUE, FALSE) else FALSE
  }

  get_publisher <- function(x) {
    if (!is.null(x$role)) ifelse("pbl" %in% x$role, TRUE, FALSE) else FALSE
  }

  is_creator <- vapply(creators, get_creator, logical(1))
  is_author <- vapply(creators, get_author, logical(1))
  is_publisher <- vapply(creators, get_publisher, logical(1))
  is_contributor <- vapply(creators, get_contributor, logical(1))

  new_creators <- c(creators[is_creator], creators[is_author[!is_creator]])
  new_contributors <- c(contributors[!contributors %in% c(creators, publishers)])
  new_publishers <- publishers[is_publisher]
  new_contributors <- contributors[is_contributor]

  creators <- if (length(new_creators) > 0) creators <- new_creators
  contributors <- if (length(new_contributors) > 0) contributors <- new_contributors
  publishers <- if (length(new_publishers) > 0) publishers <- new_publishers

  list(
    creators = creators,
    contributors = contributors,
    publishers = publishers
  )
}
