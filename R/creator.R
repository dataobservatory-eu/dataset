#' @title Get/set the Creator of the object.
#' @description Add the optional \code{Creator} property as an attribute to a
#' dataset object.
#' @details The \code{Creator}
#' corresponds to \href{https://www.dublincore.org/specifications/dublin-core/dcmi-terms/elements11/creator/}{dct:creator}
#' in Dublin Core and Creator in DataCite.
#' The name of the entity that holds, archives, publishes prints, distributes,
#' releases, issues, or produces the dataset. This property will be used to
#' formulate the citation, so consider the prominence of the role.
#' @param x A dataset object created by \code{\link{dataset}}.
#' @param value The \code{Creator} as a \code{\link[utils:person]{utils::person}} object.
#' @param overwrite If the attributes should be overwritten. In case it is set to \code{FALSE},
#' it gives a message with the current \code{Creator} property instead of overwriting it.
#' Defaults to \code{TRUE} when the attribute is set to \code{value} regardless of previous
#' setting.
#' @return The Creator attribute as a character of length one is added to \code{x}.
#' @importFrom utils person
#' @examples
#' creator(iris_dataset)
#' @family Reference metadata functions
#' @export
creator<- function(x) {
  assert_that(is.dataset(x),
              msg = "creator(x): x must be a dataset object created with dataset() or as_dataset().")

  ds_bibentry <- dataset_bibentry(x)
  ds_bibentry$author
}

#' @rdname creator
#' @export
`creator<-` <- function(x, overwrite = TRUE, value) {
  assert_that(is.dataset(x),
              msg = "creator(x) <- value: x must be a dataset object created with dataset() or as_dataset().")

  if (is.null(value)) {
    return(x)
  }

  if (!inherits(value, "person")) {
    stop("creator <- value: value must be a utils::person object.")
  }

  ds_creator <- dataset_bibentry(x)$author


  if ( overwrite ) {
    ds_creator <- value
  } else {
    ds_creator <- c(ds_creator, value)
  }

  databibentry <- attr(x, "DataBibentry")
  databibentry$author <- ds_creator
  attr(x, "DataBibentry") <- databibentry
  invisible(x)
}


