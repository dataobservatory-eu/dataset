#' @title Get/set title of a dataset
#' @description Get or reset the dataset's main title.
#' @details In the DataCite definition, several titles can be used; it is not
#' yet implemented.
#' @param x A dataset object created with \code{dataset::\link{dataset}}.
#' @param value The name(s) or title(s) by which a resource is known. See:
#' \href{https://purl.org/dc/elements/1.1/title}{dct:title}.
#' @return A string with the dataset's title; \code{set_dataset_title} returns
#' a dataset object with the changed (main) title.
#' @examples
#' dataset_title(iris_dataset)
#' dataset_title(iris_dataset, overwrite = TRUE) <-"The Famous Iris Dataset"
#' dataset_title(iris_dataset)
#' @export

dataset_title <- function(x) {
  if(!is.dataset(x)) {
    stop("dataset_title(x) must be a dataset object created with dataset() or as_dataset().")
  }

  DataBibentry <- dataset_bibentry(x)
  DataBibentry$title
}

#' @rdname dataset_title
#' @param overwrite If the attributes should be overwritten. In case it is set
#' to \code{FALSE},it gives a warning with the current \code{title}
#' property instead of overwriting it. Defaults to \code{FALSE}.
#' @importFrom stats setNames
#' @export
`dataset_title<-` <- function(x,  overwrite = FALSE, value) {

  if(!is.dataset(x)) {
   stop("title(x) <- x must be a dataset object created with dataset() or as_dataset().")
  }

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
      #value <- dataset_title_create(Title = value,
       #                             titleType = "Title")
    }
  }

  #if (! inherits(value, 'list')) {
  #  stop("title(x) <- value: value must be a character, a factor, or a list object.")
  #}

  #if (! all(names(value) %in%  c("Title", "AlternativeTitle", "Subtitle", "TranslatedTitle", "Other"))) {
  #  stop("title(x) <- value: value must be a list object with a'Title' and optional 'AlternativeTitle', `Subtitle`, `TranslatedTitle` and `Other` columns.")
  #}


  if (! DataBibentry$title %in% c(":unas", ":tba", "") & ! overwrite ) {
      warning("The dataset already has a title: ", DataBibentry$title)
  } else {
    DataBibentry$title <- value
  }

  attr(x, "DataBibentry") <- DataBibentry
  x
}

#' @importFrom stats setNames
#' @keywords internal
dataset_title_create <- function (Title,
                                  titleType = "Title") {

  old_function <- function() {

    if ( any(! titleType %in% c("Title", "AlternativeTitle", "Subtitle", "TranslatedTitle", "Other"))) {
      stop("title_create(Title, titleType): all titleType(s) must be one of 'Title', 'AlternativeTitle', 'Subtitle', 'TranslatedTitle', 'Other'")
    }

    if(!all.equal(length(Title), length(titleType))) {
      stop("title_create(Title, titleType): you must input the same number of Titles and titleType values.")
    }

    as.list(stats::setNames(Title, titleType))

  }

}
