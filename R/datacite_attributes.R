#' @keywords internal
add_relitem <- function( RelatedIdentifier, relItem, relationType ) {

  pkg_citation <- citation(package = relItem )
  related_item <- list ( RelatedIdentifier = pkg_citation$url,
                         relatedIdentifierType = "URI",
                         relationType = relationType)
  if ( is.null( RelatedIdentifier)) {
    RelatedIdentifier <- related_item
  } else if ( any (RelatedIdentifier == related_item) ) {
    RelatedIdentifier
  } else  {
    RelatedIdentifier <-  list(RelatedIdentifier, related_item)
  }
  RelatedIdentifier
}

#' @keywords internal
add_date <- function( Date = NULL, time, dateType, dateInformation) {

  date_item  <- list ( Date = time, dateType = dateType, dateInformation = dateInformation)
  if ( is.null(Date) )  { date_item } else {
    list(Date, date_item)
  }
}
