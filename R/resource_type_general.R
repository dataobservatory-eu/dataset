#' @title Get/set the resourceTypeGeneral property of a (related) item
#' @description The general type of a resource (file), see
#' \href{https://support.datacite.org/docs/datacite-metadata-schema-v44-mandatory-properties#101-resourcetypegeneral}{DataCite 4.4 10.1 resourceTypeGeneral}.
#' @details Use \code{resource_type_general_allowed} to get the allowed controlled list of
#' resourcetypes from \code{DataCite 4.4}.\cr
#' \code{\link{resource_type_general_verify}} verifies if your property is among the allowed
#' values in the DataCite 4.4 definition.
#' @param relitem An object created by \code{\link{related_item_identifier}}.
#' @return Get or set the resourceTypeGeneral property of a related item created with
#'  \code{\link{related_item}}.
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
#' @seealso related_item

resource_type_general <- function(relitem) {

  relitem$resourceTypeGeneral

}

#' @rdname resource_type_general
#' @param value The general type of a resource (file), see
#' \href{https://support.datacite.org/docs/datacite-metadata-schema-v44-mandatory-properties#101-resourcetypegeneral}{DataCite 4.4 10.1 resourceTypeGeneral}.
#' @export
`resource_type_general<-`  <- function(relitem, value) {

  resource_type_general_verify(value)
  relitem$resourceTypeGeneral <- value

  relitem

}

#' @rdname resource_type_general
#' @export
resource_type_general_allowed <- function() {
  c("Audiovisual",
    "Book",
    "BookChapter",
    "Collection",
    "ComputationalNotebook",
    "ConferencePaper",
    "ConferenceProceeding",
    "DataPaper",
    "Dataset",
    "Dissertation",
    "Event",
    "Image",
    "InteractiveResource",
    "Journal",
    "JournalArticle",
    "Model",
    "OutputManagementPlan",
    "PeerReview",
    "PhysicalObject",
    "Preprint",
    "Report",
    "Service",
    "Software",
    "Sound",
    "Standard",
    "Text",
    "Workflow",
    "Other")
}

#' @rdname resource_type_general
#' @inheritParams related_item_identifier
#' @export
resource_type_general_verify <- function(resourceTypeGeneral) {

  GeneralResourceType <- resource_type_general_allowed()

  if ( resourceTypeGeneral %in% GeneralResourceType ) {
    return(TRUE)
  } else {
    stop("resourceTypeGeneral=", resourceTypeGeneral , " is not one of '",
         paste(GeneralResourceType, collapse = "', '"), "'."  )
  }
}
