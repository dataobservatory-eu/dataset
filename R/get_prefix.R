#' @title Get prefix/resource identifier from CURIE
#' @description
#' Separate the `eg`: prefix and the \code{refArea} resource identifier
#' from the Compact URI \code{`eg:refArea`} or \code{`[eg:refArea]`}
#' @param curie A Compact URI, for example, \code{'eg:refArea'}.
#' @return The prefix, for example, \code{'eg'} from \code{'eg:refArea'}.
#' @examples
#' get_prefix("eg:refArea")
#' @export

get_prefix <- function(curie) {
  paste0(gsub(":.*","",gsub("\\[|\\]", "", curie)), ":")
}

#' @rdname get_prefix
#' @export
#' @examples
#' get_resource_identifier("eg:refArea")
#'
get_resource_identifier <- function(curie) {
  gsub(".*:","",gsub("\\[|\\]", "", curie))
  }


