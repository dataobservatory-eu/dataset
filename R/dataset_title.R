#' @title Get/set title(s) of a dataset
#' @description Add one or more \code{Title(s)} to the dataset's metadata.
#' @details In the DataCite definition, several titles can be used.
#' @param Title The name(s) or title(s) by which a resource is known, includeing \code{Title},
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

  attr(x, "Title")
}

#' @rdname dataset_title
#' @export
`dataset_title<-` <- function(x, overwrite = FALSE, value) {

  if (is.null(value)) {
    attr(x, "Title") <- NULL
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

  if (! inherits(value, 'data.frame')) {
    stop("title(x) <- value: value must be a character, a factor, or a data.frame object.")
  }

  if (! all(names(value) %in% c("Title", "titleType"))) {
    stop("title(x) <- value: value must be a data.frame object with 'Title', 'titleType' columns.")
  }

  if ((is.null(attr(x, "Title"))) | overwrite ) {
    attr(x, "Title") <- value
  } else {
    attr(x, "Title") <- rbind(attr(x, "Title"), value)
  }
  x
}

#' @rdname dataset_title
#' @export
dataset_title_create <- function (Title,
                                  titleType = "Title") {

  if ( any(! titleType %in% c("Title", "AlternativeTitle", "Subtitle", "TranslatedTitle", "Other"))) {
    stop("title_create(Title, titleType): all titleType(s) must be one of 'Title', 'AlternativeTitle', 'Subtitle', 'TranslatedTitle', 'Other'")
  }

  if(!all.equal(length(Title), length(titleType))) {
    stop("title_create(Title, titleType): you must input the same number of Titles, titleTypes values.")
  }

  Title <- data.frame(Title = Title,
                      titleType = titleType)

  Title
}
