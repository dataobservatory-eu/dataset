#' @title Get/Set the primary language of the dataset
#' @description Add the optional Language property as an attribute to an R object.
#' @details Language is an optional property in DataCite 4.4; see:
#' \href{https://support.datacite.org/docs/datacite-metadata-schema-v44-recommended-and-optional-properties#9-language}{datacite:Language}\cr
#' It is a part of the "core" of the
#' \href{https://www.dublincore.org/specifications/dublin-core/dcmi-terms/}{Dublin Core metadata terms}.
#' The language parameter is validated against the \code{[ISOcodes]{ISO_639_2}} table.\cr
#' The attribute \code{language} is added to the object. It will be exported into DataCite
#' applications in a capitalized \code{Lanugage} format.
#' @param x A semantically rich data frame object created by \code{\link{dataset_df}} or
#' \code{\link{as_dataset_df}}.
#' @param value The language to be added to the object attributes, added by name, or
#' as a 2- or 3-character code for the language. You can add a language code or language name,
#' and the parameter is normalized to \code{tolower(language)}. (The ISO 639 standard capitalizes
#' language names and uses lower case for the codes.)
#' @param iso_639_code Defaults to \code{ISO 639-3}, alternative is \code{ISO 639-1}.
#' @return The Language is added to the \code{x} as
#' \code{ISO 639-1}, the Datacite recommendation, or \code{ISO 639-3} used by the
#' Zenodo data repository.
#' @examples
#' myiris <- iris_dataset
#' language(myiris) <- "English"
#' language(myiris)
#' language(myiris) <- "fr"
#' language(myiris)
#' @family Reference metadata functions
#' @export
language <- function (x) {
  assert_that(is.dataset_df(x),
              msg = "language(x): x must be a dataset object created with dataset() or as_dataset_df().")

  ds_bibentry <- get_bibentry(x)
  as.character(ds_bibentry$language)

}

#' @rdname language
#' @export
`language<-` <- function(x, iso_639_code = "639-3", value ) {

  assert_that(is.dataset_df(x),
              msg = "language(x)<- value: x must be a dataset object created with dataset() or as_dataset_df().")

  ds_bibentry <- get_bibentry(x)
  if (is.null(value)) {
    ds_bibentry$language <- ":unas"
    attr(x, "dataset_bibentry") <- ds_bibentry
    return(x)
  }

  ISO_639 <- ISOcodes::ISO_639_2

  if (nchar(value)==2) {
    value <- tolower(value)
    lang_entry <- ISO_639[which(value == ISO_639$Alpha_2),]
  } else if ( nchar(value)== 3) {
    value <- tolower(value)
    lang_entry <- ISO_639[which(value == ISO_639$Alpha_3),]
  } else {
    value <- tolower(value)
    lang_entry <-ISO_639[which(value == tolower(ISO_639$Name)),]
  }

  if (nrow(lang_entry)==0) {
    stop(paste0("Language=", value, " is not a valid ISO 639 language code."))
  }

  if (iso_639_code == "639-1") {
    ds_bibentry$language <- lang_entry$Alpha_2
    attr(x, "dataset_bibentry") <- ds_bibentry
    return(invisible(x))
  } else {
    ds_bibentry$language <- lang_entry$Alpha_3_T
    attr(x, "dataset_bibentry") <- ds_bibentry
  }
  invisible(x)
}
