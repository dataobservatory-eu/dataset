#' @title Get/Add the primary language of the dataset
#' @description Add the optional Language property as an attribute to an R object.
#' @details Language is an optional property in
#' \href{https://support.datacite.org/docs/schema-optional-properties-v43#13-size}{DataCite 4.3} and
#' it is part of the "core" of the
#' \href{https://www.dublincore.org/specifications/dublin-core/dcmi-terms/}{Dublin Core metadata terms}.
#' The language parameter is validated against the \code{[ISOcodes]{ISO_639_2}} table.\cr
#' The attribute \code{language} is added to the object. It will be exported into DataCite
#' applications in a capitalized \code{Lanugage} format.
#' @param x An R object, such as a data.frame, a tibble, or a character vector.
#' @param Language The language to be added to the object attributes, added by name, or
#' as a 2- or 3-character code for the language. You can add a language code or language name,
#' and the parameter is normalised to \code{tolower(language)}. (The ISO 639 standard capitalizes
#' language names and uses lower case for the codes.)
#' @param iso_639_code Defaults to \code{ISO 639-3}, alternative is \code{ISO 639-1}.
#' @return The Language is added to the \code{x} as
#' \code{ISO 639-1}, the Datacite recommendation, or \code{ISO 639-3} used by the
#' Zenodo data repository.
#' @examples
#' iris_dataset <- language_add(x = iris, Language= "English")
#' attr(iris_dataset, "Language")
#' @family Metadata functions
#' @export
language <- function (x) {
  attr(x, "Language")
}

#' @rdname language
#' @export
language_add <- function(x, Language, iso_639_code = "639-3" ) {

  ISO_639 <- ISOcodes::ISO_639_2

  if (nchar(Language)==2) {
    Language <- tolower(Language)
    lang_entry <- ISO_639[which(Language == ISO_639$Alpha_2),]
  } else if ( nchar(Language)== 3) {
    Language <- tolower(Language)
    lang_entry <- ISO_639[which(Language == ISO_639$Alpha_3),]
  } else {
    Language <- tolower(Language)
    lang_entry <-ISO_639[which(Language == tolower(ISO_639$Name)),]
  }

  if (nrow(lang_entry)==0) {
    stop(paste0("Language=", Language, " is not a valid ISO 639 language code."))
  }

  if (iso_639_code == "639-1") {
    attr(x, "Language") <- lang_entry$Alpha_2
  } else {
    attr(x, "Language") <- lang_entry$Alpha_3_T
  }
  x
}
