#' @title Get/set the resource type of the dataset
#' @description Get/set \code{Type} property to the dataset.
#' @details The DataCite resourceType definition refers back to
#' \href{https://www.dublincore.org/specifications/dublin-core/dcmi-terms/#https://purl.org/dc/elements/1.1/type}{dcm:type}.
#' The \code{Type$resourceTypeGeneral} is set to  \code{Dataset}, while the user can set a more
#' specific \code{Type$resourceType} value. (See examples.)
#' @param x An R dataset object inherited from data.frame, tibble, or data.table.
#' @param value The \code{Type$resourceTypeGeneral} is set to  \code{Dataset},
#' while the user can set a more specific \code{Type$resourceType} value with the \code{value}
#' argument. To initialize a \code{Type} parameter use \code{resource_type(x) <- "Dataset"}.
#' @return Returns the \code{x} object with the \code{Type} attribute.
#' The Type$resourceTypeGeneral is set to \code{Dataset}.
#' @examples
#' x <- data.frame()
#' resource_type(x) <- "Dataset"
#' resource_type(x)
#'
#' y <- data.frame()
#' resource_type(y) <- "Census Data"
#' resource_type(y)
#' @export

resource_type <- function(x) {

  attr(x, "Type")
}

#' @rdname resource_type
#' @export
`resource_type<-` <- function(x, value) {

  if ( value == "Dataset") {
    value <- data.frame(
      resourceType = "Dataset",
      resourceTypeGeneral = "Dataset"
    )
  } else {
    value <- data.frame(
      resourceType = value,
      resourceTypeGeneral = "Dataset"
    )
  }

  attr(x, "Type") <- value

  x
}
