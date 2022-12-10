#' @title Structure a data frame to dataset
#' @description A DataSet is a collection of statistical data that corresponds to a defined structure.
#' @details Loosely follows the \href{https://www.w3.org/TR/vocab-data-cube/}{The RDF Data Cube Vocabulary},
#' but without the definition of data slices.\cr
#' \code{\link{bibentry_dataset}} is a wrapper around \code{\link[utils:bibentry]{bibentry}} to correctly turn the
#' metadata of the dataset into a bibentry object.
#' @param x A data.frame or inherited tibble, data.frame, or a structured list.
#' @param Dimensions The name or column number of the dimensions within the dataset.
#' @param Measures The name or column number of the measures within the dataset.
#' @param Attributes The name or column number of the attributes within the dataset.
#' @param sdmx_attributes The optional dimensions and attributes that conform with
#' SDMX. \code{c("time", "geo")} will mark the "time" and "geo" attributes as conforming to
#' sdmx. See \href{https://raw.githubusercontent.com/UKGovLD/publishing-statistical-data/master/specs/src/main/vocab/sdmx-attribute.ttl}{sdmx-attribute}.
#' @param Label may be used to provide a human-readable version of the dataset's name.
#' A text description (optionally with a language tag) as defined by
#' \href{https://www.w3.org/TR/rdf-schema/#ch_label}{rdfs:label}.
#' @inheritParams dublincore_add
#' @param Subject Recommended for discovery in DataCite. Subject, keyword, classification code, or key
#' phrase describing the resource. Similar to \href{https://purl.org/dc/elements/1.1/subject}{dct:subject}. \cr
#' Use \code{\link{subject}} to properly add a key phrase from a controlled vocabulary
#' and create structured Subject objects with \code{\link{subject_create}}.
#' @param Type It is set by default to \href{https://purl.org/dc/dcmitype/Dataset}{DCMITYPE:Dataset}.
#' @param Issued Corresponds to \href{https://purl.org/dc/elements/1.1/date}{dct:date}.
#' @param ... Other parameters for the \code{print} and \code{summary} methods.
#' @importFrom utils toBibtex
#' @return A data frame-like object with structural and referential metadata.
#' @family dataset functions
#' @examples
#' my_dataset <- dataset (
#'     x = data.frame (time = rep(c(2019:2022),2),
#'                     geo = c(rep("NL",4), rep("BE",4)),
#'                     value = c(1,3,2,4,2,3,1,5),
#'                     unit = rep("NR",8),
#'                     freq = rep("A",8)),
#'     Dimensions = c(1,2),
#'     Measures = 3,
#'     Attributes = c(4,5),
#'     sdmx_attributes = c("time", "freq"),
#'     Title = "Example dataset",
#'     Creator = person("Jane", "Doe"),
#'     Publisher = "Publishing Co.",
#'     Issued = as.Date("2022-07-14")
#' )
#' @export

dataset <- function(x,
                    Dimensions = NULL,
                    Measures = NULL,
                    Attributes = NULL,
                    sdmx_attributes = NULL,
                    Title = NULL,
                    Label = NULL,
                    Creator = NULL,
                    Publisher = NULL,
                    Issued = NULL,
                    Identifier = NULL,
                    Subject = NULL,
                    Type = "DCMITYPE:Dataset"
                    ) {

  tmp_ds <- subset(x, select = dimensions(x)$name)
  tmp_ds <- cbind(tmp_ds, subset(x, select = measures(x)$name))
  tmp_ds <- cbind(tmp_ds, subset(x, select = attributes_measures(x)$name))
  tmp_ds <- cbind(tmp_ds, subset(x, select = names(x)[!names(x) %in% names(tmp_ds)]))


  dimensions(tmp_ds, sdmx_attributes = sdmx_attributes)          <- Dimensions
  measures(tmp_ds)                                               <- Measures
  attributes_measures(tmp_ds, sdmx_attributes = sdmx_attributes) <- Attributes
  resource_type(tmp_ds) <- Type


  tmp_ds <- dublincore_add(tmp_ds,
                           Title = Title,
                           Creator = Creator,
                           Identifier = Identifier,
                           Publisher = Publisher,
                           Subject = Subject,
                           Type = Type)

  if (is.null(Issued)) Issued <- Sys.Date()
  attr(tmp_ds, "Date") <- Issued

  attr(tmp_ds, "class") <- c("dataset", class(tmp_ds))
  tmp_ds
}

#' @rdname dataset
#' @export
is.dataset <- function(x) inherits(x, "dataset")


#' @keywords internal
new_dataset <- function(x,
                        Dimensions = NULL,
                        Measures = NULL,
                        Attributes = NULL,
                        sdmx_attributes = NULL,
                        Title = NULL,
                        Label = NULL,
                        Creator = NULL,
                        Publisher = NULL,
                        Issued = NULL,
                        Identifier = NULL,
                        Subject = NULL,
                        Source = NULL,
                        Language = NULL,
                        Format = NULL,
                        Relation = NULL,
                        Rights = NULL,
                        Description = NULL,
                        Type = "DCMITYPE:Dataset") {

  stopifnot(inherits(x, "data.frame"))

  tmp_ds <- dublincore_add(x,
                           Title = Title,
                           Creator = Creator,
                           Identifier = Identifier,
                           Publisher = Publisher,
                           Subject = Subject,
                           Language = Language,
                           Format = Format,
                           Rights = Rights,
                           Relation = Relation,
                           Description = Description,
                           Type = Type,
                           overwrite = TRUE)

  if (is.null(Issued)) Issued <- Sys.Date()
  attr(tmp_ds, "Date") <- Issued

  if (!inherits(tmp_ds, "dataset")) {
    attr(tmp_ds, "class") <- c("dataset", class(tmp_ds))
  }

  tmp_ds
}

#' @rdname dataset
#' @export
subset.dataset <- function(x, ...) {

  subset_title <- paste0(dataset::dataset_title(x)$Title, " (subset)")
  y <- NextMethod()
  new_dataset(y,
              Dimensions = attr(x, "Dimensions"),
              Measures = attr(x, "Dimensions"),
              Attributes = attr(x, "Dimensions"),
              sdmx_attributes = attr(x, "Dimensions"),
              Title = dataset_title_create(subset_title),
              Label = attr(x, "Label"),
              Creator = attr(x, "Creator"),
              Publisher = attr(x, "Publisher"),
              Issued = attr(x, "Issued"),
              Identifier = attr(x, "Identifier"),
              Subject = attr(x, "Subject"),
              Type = "DCMITYPE:Dataset" )
}


#' @rdname dataset
#' @param i elements to extract or replace: numeric, character, empty or logical.
#' @param j elements to extract or replace: numeric, character, empty or logical.
#' @export
`[.dataset` <- function(x, i, j, ...) {
  y <- NextMethod()
  if (inherits(y, "data.frame")) {
    subset_title <- paste0(dataset::dataset_title(x)$Title, " (subset)")
    new_dataset(y,
                Dimensions = attr(x, "Dimensions"),
                Measures = attr(x, "Dimensions"),
                Attributes = attr(x, "Dimensions"),
                sdmx_attributes = attr(x, "Dimensions"),
                Title = dataset_title_create(subset_title),
                Label = attr(x, "Label"),
                Creator = attr(x, "Creator"),
                Publisher = attr(x, "Publisher"),
                Issued = attr(x, "Issued"),
                Identifier = attr(x, "Identifier"),
                Subject = attr(x, "Subject"),
                Type = "DCMITYPE:Dataset" )
  } else {
    y
  }

}

#' @rdname dataset
#' @param object an object for which a summary is desired.
#' @export
summary.dataset <- function(object, ...) {
  print_header(object)

  if( ! is.null(attr(object, "Source"))) {
    Source  <- paste0("Source: ", attr(object, "Source"), ".\n")
  } else if (is.na(attr(object, "Source"))) {
    Source <- NA_character_
  } else {
    Source <- NA_character_
  }

  y <- NextMethod()
  dataset::dataset_title(y, overwrite = T) <- dataset::dataset_title_create(paste0("Summary: ", attr(object, "Title")$Title[1]))
  print(y)

  if (!is.null(attr(object, "unit"))) {
    unit_list <- attr(object, "unit")
    cat(paste0(" in unit=", unit_list$code, " (", unit_list$label, ")"))
  }

  if (!is.na(Source)) {
    cat(Source)
  }

  invisible(y)
}

#' @rdname dataset
#' @export
print.dataset <- function(x, ...) {
  print_header(x)
  source_attribute <- attr(x, "Source")

  if( ! is.null(source_attribute) ) {
    Source <- "Source:"
    if (length(attr(x, "Source")>0)) {
      Source  <- paste0(Source, attr(x, "Source"), ".\n")
    }
  } else if (is.na(source_attribute)) {
    Source <- NA_character_
  } else {
    Source <- NA_character_
  }

  n_row <- nrow(x)

  if(n_row>10) {
    x <- x[1:10,]
  }

  NextMethod()

  if (!is.null(attr(x, "unit"))) {
    unit_list <- attr(x, "unit")
    cat(paste0(" in unit=", unit_list$code, " (", unit_list$label, ")"))
  }
  if (n_row>10) {
    cat(paste0("\n... ", n_row-10, " further observations.\n"))
  }

  if (!is.na(Source)) {
    cat(Source)
  }
}

#' @keywords internal
print_header <- function(x){

  if( is.null(attr(x, "Title"))) {
    Title <-  "Untitled"
  } else {
    Title <- attributes(x)$Title$Title
  }

  if( is.null(attr(x, "Identifier"))) {
    Identifier <-  ""
  } else if (is.na(attr(x, "Identifier"))) {
    Identifier <- ""
  } else  {
    Identifier <- paste0(" [", attributes(x)$Identifier, "] ")
  }

  if( is.null(attr(x, "Creator"))) {
    Creator <-  ""
  } else if (is.na(attr(x, "Creator"))) {
    Creator <- ""
  } else {
    Creator <- paste0(" by ", paste(attr(x, "Creator")$given, attr(x, "Creator")$family, sep = " "))
  }

  cat(paste0(Title, Identifier, Creator, "\n"))

  if( is.null(attr(x, "Publisher"))) {
    Publisher <-  ""
  } else if (is.na(attr(x, "Publisher"))) {
    Publisher <- ""
  } else {
    Publisher <- paste0("Published by ", attr(x, "Publisher"))

    if ( !is.null(attr(x, "PublicationYear")) ) {
      cat(Publisher, " (", attr(x, "PublicationYear"), ")", "\n")
    } else { cat (paste0(Publisher, "\n"))
    }
  }
}

#' @title Dimensions of a dataset
#' @details Do not confuse with \code{base::dim}. The \href{https://www.w3.org/TR/vocab-data-cube/#dsd-dimensions}{dimension} in the definition
#' of the DataSet is different from the 'dimension' definition of the R language.
#' @inheritParams dataset
#' @return A data frame of the names, class, isDefinedBy, and codeList properties of the
#' dimensions columns of the dataset following the datacube model.
#' @export
#' @examples
#' df <- data.frame ( sex = c("M", "F"), value = c(1,2), unit = c("NR", "NR"))
#' dimensions(df, sdmx_attributes = "sex") <- "sex"
#' measures(df) <- "value"
#' attributes_measures(df) <- "unit"
#' dimensions(df)
dimensions <- function(x) attr(x, "dimensions")

#' @inheritParams dataset
#' @rdname dimensions
#' @param value The name or column number of the within the dataset.
#' @export
`dimensions<-` <- function(x, sdmx_attributes = NULL, value) {

  if ( any(is.null(value) | is.na(value) | length(value)==0) ) {
    attr(x, "dimensions") <- NULL
    return(x)}

  if (is.numeric(value)) {
    selection <- names(x)[value]
  } else {
    selection <- names(x)[which(names(x) %in% value)]
  }

  dimensions_subset <- subset(x, select = selection)

  dimensions_df <- data.frame(
    names  = names(dimensions_subset),
    class = vapply(dimensions_subset, function(x) paste(class(x), collapse="|"), character(1)),
    isDefinedBy = rep("https://purl.org/linked-data/cube|https://raw.githubusercontent.com/UKGovLD/publishing-statistical-data/master/specs/src/main/vocab/sdmx-attribute.ttl", length(names(dimensions_subset))),
    codeList = rep("not yet defined", length(names(dimensions_subset)))
  )

  attr(x, "dimensions") <-  dimensions_df

  invisible(x)
}

#' @title Measures of a dataset
#' @details See the W3C and SDMX definition of a \href{https://www.w3.org/TR/vocab-data-cube/#dsd-dimensions}{measure}.
#' @inheritParams dataset
#' @return A data frame of the names, class, isDefinedBy, and codeList properties of the measurement columns of a
#' dataset following the datacube model.
#' @examples
#' df <- data.frame ( sex = c("M", "F"), value = c(1,2), unit = c("NR", "NR"))
#' dimensions(df, sdmx_attributes = "sex") <- "sex"
#' measures(df) <- "value"
#' attributes_measures(df) <- "unit"
#' measures(df)
#' @export
measures <- function(x) attr(x, "measures")

#' @inheritParams measures
#' @rdname measures
#' @param value The name or column number of the within the dataset.
#' @export
#' @examples
#' df <- data.frame ( sex = c("M", "F"), value = c(1,2))
#' measures(df) <- "value"
#' measures(df)

`measures<-` <- function(x, value) {

  if ( any(is.null(value) | is.na(value) | length(value)==0) ) return(x)

  if (is.numeric(value)) {
    selection <- names(x)[value]
  } else {
    selection <- names(x)[which(names(x) %in% value)]
  }

  subset_measures <- subset(x, select = selection)

  measures_df <- data.frame(
    names  = names(subset_measures),
    class = vapply(subset_measures, function(x) paste(class(x), collapse="|"), character(1)),
    isDefinedBy = rep("https://purl.org/linked-data/cube", length(names(subset_measures))),
    codeListe = rep("not yet defined", length(names(subset_measures)))
  )

  attr(x, "measures") <- measures_df

  invisible(x)
}

#' @title Attributes of a dataset
#' @details Do not confuse with \code{base::\link{attributes}}, which applies to the attributes
#' of the entire dataset, and not each observation (measurement) row.
#' See the W3C and SDMX definition of a \href{https://www.w3.org/TR/vocab-data-cube/#dsd-dimensions}{attribute}.
#' @inheritParams dataset
#' @return A data frame of the names, class, isDefinedBy, and codeList properties of the attributes columns of a
#' dataset following the datacube model.
#' @examples
#' df <- data.frame ( sex = c("M", "F"), value = c(1,2), unit = c("NR", "NR"))
#' dimensions(df, sdmx_attributes = "sex") <- "sex"
#' measures(df) <- "value"
#' attributes_measures(df) <- "unit"
#' attributes_measures(df)
#' @export
attributes_measures <- function(x) attr(x, "attributes")

#' @inheritParams dataset
#' @param value The name or column number of the within the dataset.
#' @param sdmx_attributes The optional SDMX dimensions.
#' @rdname attributes_measures
#' @export
`attributes_measures<-` <- function(x, sdmx_attributes = NULL, value) {

  if ( any(is.null(value) | is.na(value) | length(value)==0) ) return(x)

  if (is.numeric(value)) {
    selection <- names(x)[value]
  } else {
    selection <- names(x)[which(names(x) %in% value)]
  }

  subset_attributes <- subset(x, select = selection)

  attributes_df <- data.frame(
    names  = names(subset_attributes),
    class = vapply(subset_attributes, function(x) paste(class(x), collapse="|"), character(1)),
    isDefinedBy = rep("https://purl.org/linked-data/cube|https://raw.githubusercontent.com/UKGovLD/publishing-statistical-data/master/specs/src/main/vocab/sdmx-attribute.ttl", length(names(subset_attributes))),
    codeListe = rep("not yet defined", length(names(subset_attributes)))
  )

  attr(x, "attributes") <-  attributes_df

  invisible(x)
}



