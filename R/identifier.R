#' @title Get/set the Identifier of the object.
#' @description Add the optional Identifier property as an attribute to an R object.
#' @details The \code{Identifier} is an unambiguous reference to the resource within a given context.
#' Recommended practice is to identify the resource by means of a string conforming to an
#' identification system. Examples include International Standard Book Number (ISBN),
#' Digital Object Identifier (DOI), and Uniform Resource Name (URN).
#' Select and identifier scheme from
#' \href{https://www.ukoln.ac.uk/metadata/dcmi-ieee/identifiers/index.html}{registered URI schemes maintained by IANA}.
#' More details: \href{https://www.ukoln.ac.uk/metadata/dcmi-ieee/identifiers/}{Guidelines for using resource identifiers in Dublin Core metadata and IEEE LOM}.
#' Similar to \code{Identifier} in \code{\link{datacite}}.
#' \href{https://support.datacite.org/docs/datacite-metadata-schema-v44-mandatory-properties#1-identifier}{DataCite 4.4}.\cr
#' It is not part of the "core" Dublin Core terms, but we always add it to the metadata attributes
#' of a dataset (in case you use a strict Dublin Core property sheet you can omit it.)
#' \href{https://www.dublincore.org/specifications/dublin-core/dcmi-terms/}{Dublin Core metadata terms}.
#' @param x An R object, such as a data.frame, a tibble, or a data.table.
#' @param value The  \code{Identifier} as a character string.
#' @param overwrite If the attributes should be overwritten. In case it is set to \code{FALSE},
#' it gives a message with the current \code{Identifier} property instead of overwriting it.
#' Defaults to \code{TRUE} when the attribute is set to \code{value} regardless of previous
#' setting.
#' @return The \code{Identifier} attribute as a character of length 1 is added to \code{x}.
#' @examples
#' identifier(iris_dataset) <- "https://doi.org/10.1111/j.1469-1809.1936.tb02137.x"
#' identifier(iris_dataset)
#' @family Reference metadata functions
#' @export

#' @export
identifier <- function(x) {
  assert_that(is.dataset_df(x),
              msg = "identifier(x): x must be a dataset object created with dataset_df() or as_dataset_df().")

  ds_bibentry <- get_bibentry(x)
  ds_bibentry$identifier
}

#' @rdname identifier
#' @export
`identifier<-`  <- function(x,  overwrite = TRUE, value) {
  assert_that(is.dataset_df(x),
              msg = "identifier(x) <- value: x must be a dataset object created with dataset_df() or as_dataset_df().")

  ds_bibentry <- get_bibentry(x)
  old_identifier <- ds_bibentry$identifier

  if (is.null(value)) {
    value <- ":unas"
  }

  if ( overwrite | old_identifier %in% c(":unas", ":tba")) {
    ds_bibentry$identifier <- as.character(value)
    attr(x, "dataset_bibentry") <- ds_bibentry
  } else {
    warning ("The dataset has already an identifier: ",
             old_identifier, ".\nYou can overwrite this message with identifier(x, overwrite = TRUE) <- value" )
  }
  invisible(x)
}

