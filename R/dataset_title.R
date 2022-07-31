#' @title Read / Add DataCite metadata to an object
#' @description Read/Add a Title attribute to a dataset or R object
#' @examples
#' my_iris <- iris
#' my_iris <- title_add(my_iris,
#'                      Title = c("The Famous Dataset", "Iris Dataset"),
#'                       titleType = c("AlternativeTitle", "Title")
#'                      )
#' dataset_title(my_iris)
#' @export
dataset_title <- function(x) {
  attr(x, "Title")
}

#' @title Add a Title attribute
#' @details
#' For Dublin Core metadata, use Title as a character vector of length 1, with
#' titleType = \code{NULL}.\cr
#' For DataCite, provide a vector of titles and an equal length of titleType parameters.
#' @inheritParams datacite
#' @rdname dataset_title


title_add <- function(x, Title, titleType = NULL) {

  controlled_titleType <- c("Title",
                            "AlternativeTitle",
                            "Subtitle",
                            "TranslatedTitle",
                            "Other")

  if (is.null(Title)) return(x)

  if (length(Title)==1 & is.null(titleType)) {
    attr(x, "Title") <- Title
    return(x)
  } else if ( ! any(titleType %in% controlled_titleType) )  {
    {
      stop(paste0("tile_add(Title, titleType): ", titleType[1], " is not one of Title, AlternativeTitle, Subtitle, TranslatedTitle, or Other."))
    }
  }

  if (length(Title)!= length(titleType)) {
    stop ("title_add(Title, titleType):  Title (length=", length(Title), ") and titleType (length=", length(titleType),") must be of same length.")
  }

  if ("Title" %in% titleType) {
    title <- list (Title = Title[which(titleType == "Title")])
  } else {
    title <- list (Title = as.character(unlist(dataset_title(x))[1]))
  }

  if ("Subtitle" %in% titleType ) {
    title  <- c(title, list ( Subtitle =  Title[which(titleType == "Subtitle")]))
  }

  if ("AlternativeTitle" %in% titleType) {
    title <- c(title, list ( AlternativeTitle =  Title[which(titleType == "AlternativeTitle")]))
  }

  if ("Other" %in% titleType) {
    title <- c(title, list ( Other =  Title[which(titleType == "Other")]))
  }

  attr(x, "Title") <- title

  dataset_title(x)
  x
}
