#' @title Get/set the Creator of the object.
#' @description Add the optional \code{Creator} property as an attribute to an R object.
#' @details The \code{Creator}
#' corresponds to \href{https://www.dublincore.org/specifications/dublin-core/dcmi-terms/#https://purl.org/dc/elements/1.1/creator}{dct:creator} and Creator in DataCite.
#' The name of the entity that holds, archives, publishes prints, distributes, releases,
#' issues, or produces the resource. This property will be used to formulate the citation,
#' so consider the prominence of the role. For software, use \code{Creator} for the
#' code repository. If there is an entity other than a code repository, that
#' "holds, archives, publishes, prints, distributes, releases, issues, or produces" the
#' code, use the property Contributor/contributorType/hostingInstitution for the code
#' repository.
#' @param x An R object, such as a data.frame, a tibble, or a data.table.
#' @param value The \code{Creator} as a \code{utils::\link[utils]{person}} object.
#' @param overwrite If the attributes should be overwritten. In case it is set to \code{FALSE},
#' it gives a message with the current \code{Creator} property instead of overwriting it.
#' Defaults to \code{TRUE} when the attribute is set to \code{value} regardless of previous
#' setting.
#' @return The Creator attribute as a character of length 1 is added to \code{x}.
#' @importFrom utils person
#' @examples
#' iris_dataset <- iris
#' creator(iris_dataset) <- person("Anderson", "Edgar", role = "aut")
#' creator(iris_dataset)
#' @family Reference metadata functions
#' @export
creator<- function(x) {
  attr(x, "Creator")
}

#' @rdname creator
#' @export
`creator<-` <- function(x, overwrite = TRUE, value) {

  if (is.null(value)) {
    attr(x, "Creator") <- NULL
    return(x)
  }

  if (!inherits(value, "person")) {
    stop("creator <- value: value must be a utils::person object.")
  }

  if (is.null(attr(x, "Creator"))) {
    if (is.null(value)) {
      attr(x, "Creator") <- NA_character_
    } else {
      attr(x, "Creator") <- value
    }
  } else if ( overwrite ) {
    attr(x, "Creator") <- value
  } else {
    message ("The dataset has already an Creator: ",  creator(x) )
  }
  x
}
