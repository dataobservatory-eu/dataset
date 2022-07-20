#' @title Create a related item identifer
#' @return A list
#' @param identifier The unique identifier of the resource.
#' @param relation_type Seee
#' @param resource_type Seee
#' @param scheme See, recommended to use \code{"URL"} or \code{"DOI"}.
#' @family metadata functions
#' @examples
#' related_item_identifer
#' @export

related_item_identifier <- function(scheme, identifier, relation_type, resource_type) {
  list ( scheme = scheme,
         Identifier = identifier,
         relationType = relation,
         resource_type = resouce_type)
}


#' @title Create a related item
#' @inheritParams datacite
#' @param related_item_identifier A list created with \code{\link{related_item_identifier}}.
#' @return a related item.
#' @family metadata functions
#' @export

related_item <- function(related_item_identifier,
                         creator, title,
                         publication_year,
                         volume = NULL,
                         issue = NULL,
                         number = NULL, number_type = NULL,
                         first_page = NULL, last_page = NULL,
                         publisher = NULL,
                         edition = NULL,
                         contributor = NULL){

  rel_item <- list (
    relatedItemIdentifier = related_item_identifier,
    Creator = creator,
    Title = title,
    PublicationYear = publication_year,
    Publisher = publisher)

  if (!is.null(publisher)) rel_item <-c(related_items, Publisher = publisher)
  if (!is.null(contributor)) rel_item <-c(related_items, Contributor = contributor)
  if (!is.null(number)) rel_item <-c(related_items, number = number)
  if (!is.null(edition)) rel_item <-c(related_items, edition = edition)
  if (!is.null(volume)) rel_item <-c(related_items, volume = volume)
  if (!is.null(issue)) rel_item <-c(related_items, issue = issue)
  if (!is.null(first_page)) rel_item <-c(related_items, firstPage = first_page)
  if (!is.null(last_page)) rel_item <-c(related_items, lastPage = last_page)

  rel_item
}

