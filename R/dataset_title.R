#' @title Get/set title(s) of a dataset
#' @description Add one or more \code{Title(s)} to the dataset's metadata.
#' @details In the DataCite definition, several titles can be used.
#' @param Title The name(s) or title(s) by which a resource is known, including \code{Title},
#'  \code{AlternativeTitle},  \code{Subtitle},  \code{TranslatedTitle},  \code{OtherTitle}.
#' May be the title of a dataset
#' or the name of a piece of software.
#' Similar to \href{https://purl.org/dc/elements/1.1/title}{dct:title}.\cr
#' Use \code{\link{dataset_title_create}} to create a several title entries.
#' @param x An R object
#' @param value The name(s) or title(s) by which a resource is known. A character string or
#' a Title object created by \code{\link{dataset_title_create}}.Similar to
#' \href{https://purl.org/dc/elements/1.1/title}{dct:title}.
#' @param titleType In DataCite, the controlled values are
#' \code{AlternativeTitle}, \code{Subtitle}, \code{TranslatedTitle}, \code{Other}. When no titleType is given (as in
#' Dublin Core), the titleType is set to \code{Title}.
#' @param overwrite Defaults to \code{FALSE}.
#' @return The titles as a data.frame with a titleTypes column.
#' @examples
#' my_iris <- iris
#'
#' dataset_title(my_iris) <- dataset_title_create(
#'     Title = c("Iris Dataset",
#'     "The famous iris dataset of the R examples"),
#'     titleType = c("Title", "Subtitle")
#'     )
#' dataset_title(my_iris)
#'
#'  y <- data.frame()
#'  dataset_title(y) <- "R (Computer program language)"
#'  dataset_title(y) <- "Questionnaires--Computer programs"
#'  dataset_title(y)
#' @export

dataset_title <- function(x) {
  assert_that(is.dataset(x),
              msg = "dataset_title(x) must be a dataset object created with dataset() or as_dataset().")

  DataBibentry <- dataset_bibentry(x)
  DataBibentry$dataset_title
}

#' @rdname dataset_title
#' @export
`dataset_title<-` <- function(x, overwrite = FALSE, value) {

  assert_that(is.dataset(x),
              msg = "In dataset_title(x) <- x must be a dataset object created with dataset() or as_dataset().")

  DataBibentry <- invisible(dataset_bibentry(x))

  if (is.null(value)) {
    DataBibentry$title <- ":tba"
    attr(x, "DataBibentry") <- DataBibentry
    return(x)
  }

  if ( any(c("character", "factor") %in% class(value)) ) {
    if (length(value)>1) {
      stop("title(x) <- value: if you have multiple titles, use dataset_title_create()")
    } else {
      value <- dataset_title_create(Title = value,
                                    titleType = "Title")
    }
  }

  #if (! inherits(value, 'list')) {
  #  stop("title(x) <- value: value must be a character, a factor, or a list object.")
  #}

  #if (! all(names(value) %in%  c("Title", "AlternativeTitle", "Subtitle", "TranslatedTitle", "Other"))) {
  #  stop("title(x) <- value: value must be a list object with a'Title' and optional 'AlternativeTitle', `Subtitle`, `TranslatedTitle` and `Other` columns.")
  #}


  if (is.null(DataBibentry$title) | overwrite ) {
    DataBibentry$title <- value
  }

  attr(x, "DataBibentry") <- DataBibentry
  x
}

#' @rdname dataset_title
#' @importFrom stats setNames
#' @keywords internal
dataset_title_create <- function (Title,
                                  titleType = "Title") {

  old_function <- function() {

    if ( any(! titleType %in% c("Title", "AlternativeTitle", "Subtitle", "TranslatedTitle", "Other"))) {
      stop("title_create(Title, titleType): all titleType(s) must be one of 'Title', 'AlternativeTitle', 'Subtitle', 'TranslatedTitle', 'Other'")
    }

    if(!all.equal(length(Title), length(titleType))) {
      stop("title_create(Title, titleType): you must input the same number of Titles, titleTypes values.")
    }

    as.list(stats::setNames(Title, titleType))

  }

}
