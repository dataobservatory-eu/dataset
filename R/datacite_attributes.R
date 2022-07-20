#' @keywords internal
add_relitem <- function( RelatedIdentifier, relItem, relationType ) {

  if (!is.null(RelatedIdentifier)) {
    existing_identifiers <- RelatedIdentifier$RelatedIdentifier
  } else { existing_identifiers  <-  ""}


  pkg_citation <- citation(package = relItem )
  related_item <- data.frame ( RelatedIdentifier = pkg_citation$url,
                               relatedIdentifierType = "URI",
                               relationType = relationType)
  if ( is.null( RelatedIdentifier)) {
    RelatedIdentifier <- related_item
  } else if (existing_identifiers %in% related_item$RelatedIdentifier ) {
    RelatedIdentifier
  } else  {
    RelatedIdentifier <-  rbind(RelatedIdentifier, related_item)
  }
  RelatedIdentifier
}

#' @keywords internal
add_date <- function( Date = NULL, time, dateType, dateInformation) {

  date_item  <- data.frame ( Date = time,
                             dateType = dateType,
                             dateInformation = dateInformation)
  if ( is.null(Date) )  { date_item } else {
    rbind(Date, date_item)
  }
}






