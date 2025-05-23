#' @title Get/set the Identifier of the object.
#' @description Add the optional Identifier property as an attribute to an R
#'   object.
#' @details The \code{Identifier} is an unambiguous reference to the resource
#'   within a given context. Recommended practice is to identify the resource by
#'   means of a string conforming to an identification system. Examples include
#'   International Standard Book Number (ISBN), Digital Object Identifier (DOI),
#'   and Uniform Resource Name (URN). Select and identifier scheme from
#'   \href{https://www.ukoln.ac.uk/metadata/dcmi-ieee/identifiers/index.html}{registered
#'   URI schemes maintained by IANA}. More details:
#'   \href{https://www.ukoln.ac.uk/metadata/dcmi-ieee/identifiers/}{Guidelines
#'   for using resource identifiers in Dublin Core metadata and IEEE LOM}.
#'   Similar to \code{Identifier} in \code{\link{datacite}}.
#'   \href{https://support.datacite.org/docs/datacite-metadata-schema-v44-mandatory-properties#1-identifier}{DataCite
#'   4.4}.\cr It is not part of the "core" Dublin Core terms, but we always add
#'   it to the metadata attributes of a dataset (in case you use a strict Dublin
#'   Core property sheet you can omit it.)
#'   \href{https://www.dublincore.org/specifications/dublin-core/dcmi-terms/}{Dublin
#'   Core metadata terms}.
#' @param x An \code{\link{dataset_df}} object or a
#'   \code{\link[utils:bibentry]{utils::bibentry}} object, including possibly an
#'   instance of its \code{\link{dublincore}} or \code{\link{datacite}}
#'   subclass.
#' @param value The  \code{Identifier} as a character string.
#' @param overwrite If the attributes should be overwritten. In case it is set
#'   to \code{FALSE}, it gives a message with the current \code{Identifier}
#'   property instead of overwriting it. Defaults to \code{TRUE} when the
#'   attribute is set to \code{value} regardless of previous setting.
#' @return The \code{Identifier} attribute as a character of length 1 is added
#'   to \code{x}.
#' @examples
#' identifier(orange_df)
#' orange_copy <- orange_df
#' identifier(orange_copy) <- "https://doi.org/99999/9999999"
#' @family Reference metadata functions
#' @export

identifier <- function(x) {
  if (!(inherits(x, "bibentry") || inherits(x, "dataset_df"))) {
    stop("identifier(x): x must be a dataset_df or a bibentry object.")
  }

  if (is.dataset_df(x)) {
    ds_bibentry <- get_bibentry(x)
  } else {
    ds_bibentry <- x
  }

  ds_bibentry$identifier
}

#' @rdname identifier
#' @export
`identifier<-` <- function(x, overwrite = TRUE, value) {
  if (is.numeric(value)) value <- as.character(value)

  if (!(inherits(x, "bibentry") || inherits(x, "dataset_df"))) {
    stop("identifier(x): x must be a dataset_df or a bibentry object.")
  }

  if (!(is.null(value) || inherits(value, "character"))) {
    stop("identifier(x) <- value: value must be a named or not named character string of length 1.")
  }

  if (is.dataset_df(x)) {
    ds_bibentry <- get_bibentry(x)
  } else {
    ds_bibentry <- x
  }

  old_identifier <- ds_bibentry$identifier

  if (is.null(value)) {
    value <- ":unas"
  }

  is_doi <- function(i) {
    if (!is.null(names(i))) {
      if (tolower(names(i)) == "doi") {
        return(TRUE)
      }
    } else {
      ifelse(grepl("https://doi.org", i), TRUE, FALSE)
    }
  }


  if (overwrite || old_identifier %in% c(":unas", ":tba")) {
    ds_bibentry$identifier <- value
    if (is_doi(value)) {
      doi <- gsub("https://doi.org/", "", value)
      doi <- gsub("/$", "", doi)
      ds_bibentry$doi <- doi
    }
  } else {
    warning(
      "The dataset has already an identifier: ",
      old_identifier, ".\nYou can overwrite this message with identifier(x, overwrite = TRUE) <- value"
    )
  }


  if (inherits(x, "bibentry")) {
    ds_bibentry
  } else {
    if (is_doi(value)) attr(x, "doi") <- gsub("https://doi.org/", "", value)
    attr(x, "dataset_bibentry") <- ds_bibentry
    invisible(x)
  }
}
