#' @title Create a dataset
#'
#' @param x An R object that contains the data of the dataset (a data.frame or
#' inherited from data.frame, for example, tibble, tsibble, data.table).
#' @param author A single person or a vector of persons as authors, declared with
#' \code{\link[utils:person]{person}}.
#' @param title The title of the dataset.
#' @param identifier The permanent identifier, for example, the DOI of the dataset.
#' If left empty (NULL), receives the DataCite standard value \code{':tba'}, or to
#' be announced later.
#' @param publisher The organisation or person that publishes the dataset.
#' If left empty (NULL), receives the DataCite standard value \code{':tba'}, or to
#' be announced later.
#' @param year The year of the creation of the dataset. If left empty (NULL),
#' the current year.
#' @param version The version of the dataset. If left emtpy (NULL), defaults to
#' \code{'0.1.0'}
#' @param Subject The subject of the data frame, as a subject type.
#' @param description The optional \code{Description} property as an attribute to
#' an R object.
#' @param language The primary language of the dataset, for example \code{'eng'}.
#' Defaults to \code{NULL} that sets it to the unassigned value \code{':unas'}.
#' @param datasource The source of the dataset,
#' \href{https://www.dublincore.org/specifications/dublin-core/dcmi-terms/elements11/source/}{DCMI: Source},
#' which corresponds to a \code{relatedItem} in the DataCite vocabulary. We use
#' \code{datasource} instead of \code{source} to avoid naming conflicts with the
#' base R  \code{source()} function.
#' @param rights Any rights information for this resource. The property may be
#' repeated to record complex rights characteristics.
#' Free text, defaults to \code{":unas"} for unassigned values. See \code{\link{rights}}.
#' @return A dataset object, which is a data.frame or inherited object with rich
#' metadata.
#' @examples
#' ds <- dataset(iris,
#'         title = "The iris Dataset",
#'         author = c(
#'            person(family ="Anderson",
#'            given ="Edgar",
#'            role = "aut")
#'            ),
#'          identifier = "https://doi.org/10.1111/j.1469-1809.1936.tb02137.x",
#'          year = "1935",
#'          version = "1.0",
#'          description = "The famous dataset that is distributed with R.",
#'          url = "https://en.wikipedia.org/wiki/Iris_flower_data_set",
#'          resourceType = "Dataset"
#'          )
#' @export

dataset  <- function(x,
                     author,
                     title,
                     identifier = NULL,
                     publisher = NULL,
                     year = NULL,
                     version = NULL,
                     subject = NULL,
                     description = NULL,
                     language = NULL,
                     datasource = NULL,
                     rights = NULL,
                     ... ) {

  arguments <- list(...)

  as_dataset(x,
             author=author,
             title=title,
             publisher=publisher,
             identifier=identifier,
             year=year,
             version=version,
             subject=subject,
             language=language,
             datasource=datasource,
             rights=rights)
}

#' @keywords internal
new_dataset <- function (x,
                         DataBibentry,
                         subject) {

  validate_dataset(x = x,
                   DataBibentry = DataBibentry,
                   subject = subject)


  DataStructureNest <- lapply (1:ncol(x), function(col) initialise_dsd(df = x, col) )

  DataStructure <- DataStructureNest[[1]]
  if (ncol(x)>1) {
    for ( i in 2:ncol(x)) DataStructure <- c(DataStructure,  DataStructureNest[[i]])
  }

  attr(x, "DataBibentry") <- DataBibentry
  attr(x, "Subject") <- subject
  attr(x, "DataStructure") <- DataStructure
  class(x) <- c("dataset", class(x))
  x
}

#' @keywords internal
validate_dataset <- function(x,
                             DataBibentry,
                             subject) {

  if (! inherits(x, "data.frame"))   {
    wrong_class <- class(x)
    stop("dataset(x=...) must be inherited from a data.frame (like data.frame, tibble, data.table, tsibble ...), not ", wrong_class)
  }

  if (! inherits(DataBibentry, "bibentry"))   {
    wrong_class <- class(DataBibentry)
    stop("dataset(Bibentry=...) must be inherited from a bibentry, not ", wrong_class)
  }

  if (! inherits(subject, "subject") )   {
    wrong_class <- class(subject)
    stop("dataset(subject=...) must be inherited from a subject [created by dataset::subject()], not ", wrong_class)
  }

}

#' @rdname dataset
#' @export
is.dataset <- function(x) inherits(x, "dataset")


#' @rdname dataset
#' @export
print.dataset <- function(x, ...) {
  print(attr(x, "DataBibentry"))
  NextMethod()
  cat(paste0("Further metadata: describe(", deparse(substitute(x)), ")\n"))
}

#' @rdname dataset
#' @param object an object for which a summary is desired.
#' @param ... Further arguments passed on to generic methods
#' like \code{summary(x, ...)}.
#' @export
summary.dataset <- function(object, ...) {
  args <- list(...)
  print(attr(object, "DataBibentry"))
  cat(paste0("Further metadata: describe(", deparse(substitute(object)), ")\n"))
  NextMethod()
 }


#' @title Get / Set a variable labels in a dataset
#' @param x A dataset.
var_labels <- function(x) {
  UseMethod("var_labels", x)
}

set_var_labels <- function(x, value) {
  UseMethod("set_var_labels")
}

#' @rdname var_labels
#' @exportS3Method
var_labels.dataset <- function(x) {
  dsd <- attr(x, "DataStructure", exact = TRUE)
  vapply (names(dsd), function(x) unlist(dsd[[x]]$label), character(1))
}

#' @rdname var_labels
#' @examples
#' relabelled <- set_var_labels(
#'                iris_dataset,
#'                 c(Sepal.Length="The sepal length measured in centimeters.",
#'                   Sepal.Width="The sepal width measured in centimeters.",
#'                   Species="The species of the iris observed.")
#'                  )
#' var_labels(relabelled)
#'
#' @exportS3Method
`set_var_labels.dataset` <- function(x, value) {
  dsd <- attr(x, "DataStructure")

  original_labels <-  vapply (names(dsd), function(x) unlist(dsd[[x]]$label), character(1))
  to_change <- which(  names(dsd) %in% names(value) )
  new_labels <- original_labels

  new_labels <- vapply(seq_along(original_labels),
                       function(x) {
                         ifelse (x %in% to_change,
                                 yes = value[which(names(value)==names(original_labels)[x])],
                                 no = original_labels[x])
                       }, character(1))

  dsd2 <- dsd

  for ( i in seq_along(dsd)) {
    dsd2[[i]]$label <- new_labels[i]
  }

  attr(x, "DataStructure") <- dsd2

  invisible(x)
}
