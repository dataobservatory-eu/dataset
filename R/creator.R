#' @title Get/set the Creator of the object.
#' @description Add the optional \code{Creator} property as an attribute to a
#' dataset object.
#' @details The \code{Creator}
#' corresponds to \href{https://www.dublincore.org/specifications/dublin-core/dcmi-terms/elements11/creator/}{dct:creator}
#' in Dublin Core and Creator in DataCite.
#' The name of the entity that holds, archives, publishes prints, distributes,
#' releases, issues, or produces the datast. This property will be used to
#' formulate the citation, so consider the prominence of the role.
#' @param ds A dataset object created by \code{\link{dataset}}.
#' @param value The \code{Creator} as a \code{utils::\link[utils]{person}} object.
#' @param overwrite If the attributes should be overwritten. In case it is set to \code{FALSE},
#' it gives a message with the current \code{Creator} property instead of overwriting it.
#' Defaults to \code{TRUE} when the attribute is set to \code{value} regardless of previous
#' setting.
#' @return The Creator attribute as a character of length one is added to \code{x}.
#' @importFrom utils person
#' @examples
#' ds <- dataset(iris,
#'               title = "The iris Dataset",
#'               author = c(
#'                 person(family ="Anderson",
#'                        given ="Edgar",
#'                        role = "aut")
#'               ),
#'               identifier = "https://doi.org/10.1111/j.1469-1809.1936.tb02137.x",
#'               year = "1935",
#'               version = "1.0",
#'               description = "The famous dataset that is distributed with R.")
#' @family Reference metadata functions
#' @export
creator<- function(ds) {

  ds_bibentry <- dataset_bibentry(ds)
  ds_bibentry$author
}

#' @rdname creator
#' @export
`creator<-` <- function(ds, overwrite = TRUE, value) {

  if (is.null(value)) {
    return(ds)
  }

  if (!inherits(value, "person")) {
    stop("creator <- value: value must be a utils::person object.")
  }

  ds_creator <- dataset_bibentry(ds)$author


  if ( overwrite ) {
    ds_creator <- value
  } else {
    ds_creator <- c(ds_creator, value)
  }

  databibentry <- attr(ds, "DataBibentry")
  databibentry$author <- ds_creator
  attr(ds, "DataBibentry") <- databibentry
  ds
}


