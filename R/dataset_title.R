#' @title Get/set the title of a dataset
#' @description Get or reset the dataset's main title.
#' @details In the DataCite definition, several titles can be used; it is not
#' yet implemented.
#' @param x A dataset object created with [dataset_df()] or [as_dataset_df()].
#' @param value The name(s) or title(s) by which a resource is known. See:
#' \href{https://www.dublincore.org/specifications/dublin-core/dcmi-terms/elements11/title/}{dct:title}.
#' @return A string with the dataset's title; \code{set_dataset_title} returns
#' a dataset object with the changed (main) title.
#' @family Bibliographic reference functions
#' @examples
#' dataset_title(iris_dataset)
#' dataset_title(iris_dataset, overwrite = TRUE) <-"The Famous Iris Dataset"
#' dataset_title(iris_dataset)
#' @export

dataset_title <- function(x) {
  if(!is.dataset_df(x)) {
    stop("dataset_title(x) must be a dataset object created with dataset() or as_dataset_df().")
  }

  ds_bibentry <- get_bibentry(x)
  ds_bibentry$title
}

#' @rdname dataset_title
#' @param overwrite If the attributes should be overwritten. In case it is set
#' to \code{FALSE},it gives a warning with the current \code{title}
#' property instead of overwriting it. Defaults to \code{FALSE}.
#' @importFrom stats setNames
#' @export
`dataset_title<-` <- function(x,  overwrite = FALSE, value) {

  if(!is.dataset_df(x)) {
    stop("title(x) <- x must be a dataset object created with dataset() or as_dataset_df().")
  }

  ds_bibentry <- invisible(get_bibentry(x))

  if (is.null(value)) {
    ds_bibentry$title <- ":tba"
    attr(x, "dataset_bibentry") <- ds_bibentry
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


  if (! ds_bibentry$title %in% c(":unas", ":tba", "") & ! overwrite ) {
    warning("The dataset already has a title: ", ds_bibentry$title)
  } else {
    ds_bibentry$title <- value
  }

  attr(x, "dataset_bibentry") <- ds_bibentry

  invisible(x)
}
