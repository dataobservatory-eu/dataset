#' @title Create a related item
#' @description Create a
#' \href{https://support.datacite.org/docs/datacite-metadata-schema-v44-recommended-and-optional-properties#12-relatedidentifier}{RelatedIdentifier},
#' attribute, which is recommended for discovery in \code{DataCite}.
#' @inheritParams datacite
#' @param relatedIdentifierType See \href{https://support.datacite.org/docs/datacite-metadata-schema-v44-recommended-and-optional-properties#12a-relatedidentifiertype}{relatedIdentifierType}.
#' @param relationType See \href{https://support.datacite.org/docs/datacite-metadata-schema-v44-recommended-and-optional-properties#12b-relationtype}{relationType}.
#' @param schemeURI  See \href{https://support.datacite.org/docs/datacite-metadata-schema-v44-recommended-and-optional-properties#12d-schemeuri}{schemeURI}.
#' @param schemeType  See \href{https://support.datacite.org/docs/datacite-metadata-schema-v44-recommended-and-optional-properties#12e-schemetype}{schemeType}.
#' @param resourceTypeGeneral The general type of a resource or file.
#' @param Volume The volume of the related item (optional).
#' @param Issue The issue number of the related item (optional).
#' @param Edition The edition of the related item (optional).
#' @param Number The number of the related item (optional).
#' @param numberType The type of the number (optional).
#' @param firstPage The first page of the related item (optional).
#' @param lastPage The first page of the related item (optional).
#' @return a related item.
#' @family metadata functions
#' @examples
#' my_item <- related_item (Identifier = "https://zenodo.org/record/5703222#.YZYkm2DMLIU",
#'                          Creator = person ("Daniel", "Antal", role = "aut"),
#'                          Publisher = "Zenodo",
#'                          PublicationYear = 2022,
#'                          relatedIdentifierType = "DOI",
#'                          relationType = "CompiledBy",
#'                          schemeURI = "URI",
#'                          resourceTypeGeneral = "Dataset")
#' @export

related_item <- function(Identifier,
                         Creator,
                         Title,
                         relatedIdentifierType,
                         relationType,
                         schemeURI = NA_character_,
                         schemeType = NA_character_,
                         resourceTypeGeneral = NA_character_,
                         PublicationYear = NULL,
                         Volume = NULL,
                         Issue = NULL,
                         Number = NULL,
                         numberType = NULL,
                         firstPage = NULL, lastPage = NULL,
                         Publisher = NULL,
                         Edition = NULL,
                         Contributor = NULL){

  rel_item <- related_item_identifier (Identifier = Identifier,
                                       relatedIdentifierType = relatedIdentifierType,
                                       relationType = relationType,
                                       schemeURI = schemeURI,
                                       schemeType = schemeType,
                                       resourceTypeGeneral = resourceTypeGeneral)

  if (!is.null(Publisher)) rel_item$Publisher <- Publisher
  if (!is.null(PublicationYear)) rel_item$PublicationYear <- PublicationYear
  if (!is.null(Contributor)) rel_item$Contributor <- Contributor
  if (!is.null(Number)) rel_item$Number <- Number
  if (!is.null(numberType)) rel_item$numberType <- numberType
  if (!is.null(Edition)) rel_item$Edition <- Edition
  if (!is.null(Volume)) rel_item$Volume <- Volume
  if (!is.null(Issue)) rel_item$Issue <- Issue
  if (!is.null(firstPage)) rel_item$firstPage <- firstPage
  if (!is.null(lastPage)) rel_item$lastPage <- lastPage

  rel_item
}

#' @title Create a related item identifier
#' @return A data.frame
#' @inheritParams datacite
#' @inheritParams related_item
#' @keywords internal
related_item_identifier <- function(Identifier,
                                    relatedIdentifierType,
                                    relationType,
                                    schemeURI = NA_character_,
                                    schemeType = NA_character_,
                                    resourceTypeGeneral = NA_character_) {

  relitem_identifier <- data.frame ( Identifier = Identifier,
                                     relatedIdentifierType = relatedIdentifierType,
                                     relationType = relationType,
                                     schemeURI = schemeURI,
                                     schemeType = schemeType)

  relitem_identifier

}
