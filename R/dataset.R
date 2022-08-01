#' @title Structure a data frame to dataset
#' @description A DataSet is a collection of statistical data that corresponds to a defined structure.
#' @details Loosely follows the \href{https://www.w3.org/TR/vocab-data-cube/}{The RDF Data Cube Vocabulary},
#' but without the definition of data slices.\cr
#' \code{bibentry_dataset()} is a wrapper around \code{\link[utils:bibentry]{bibentry}} to correctly turn the
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
#' phrase describing the resource. Similar to \href{http://purl.org/dc/elements/1.1/subject}{dct:subject}. \cr
#' Use \code{\link{subject}} to properly add a key phrase from a controlled vocabulary
#' and create structured Subject objects with \code{\link{subject_create}}.
#' @param Type It is set by default to \href{http://purl.org/dc/dcmitype/Dataset}{DCMITYPE:Dataset}.
#' @param Issued Corresponds to \href{http://purl.org/dc/elements/1.1/date}{dct:date}.
#' @param ... Other parameters for the \code{print} and \code{summary} methods.
#' @param value The name or column number of the within the dataset.
#' @importFrom utils toBibtex
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
#'
#' utils::toBibtex(bibentry_dataset(my_dataset))
#'
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

  dimensions(x,  sdmx_attributes = sdmx_attributes) <- Dimensions
  measures(x) <- Measures
  attributes_measures(x, sdmx_attributes = sdmx_attributes) <- Attributes
  resource_type(x) <- Type

  x <- dublincore_add(x,
                      Title = Title,
                      Creator = Creator,
                      Identifier = Identifier,
                      Publisher = Publisher,
                      Subject = Subject,
                      Type = Type)

  if (is.null(Issued)) Issued <- Sys.Date()
  attr(x, "Date") <- Issued

  attr(x, "class") <- c("dataset", class(x))
  x
}

print   <- function(x, ...) { UseMethod("print")}
summary <- function(x, ...) { UseMethod("summary")}

#' @rdname dataset
summary.dataset <- function(x, ...) { NextMethod()}

#' @rdname dataset
print.dataset <- function(x, ...) {

  #cat(paste0(Title, " [", attr(x, "Identifier"), "] by ", paste(attr(x, "Creator")$given, attr(x, "Creator")$family, sep = " "), "\n"))
  #cat(paste0("Published by ", attr(x, "Publisher"), " (", attr(x, "PublicationYear"), ")", "\n"))

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
}


#' @rdname dataset
#' @export
is.dataset <- function(x) inherits(x, "dataset")


#' @title Dimensions of a dataset
#' @details Do not confuse with \code{base::dim}. The \href{https://www.w3.org/TR/vocab-data-cube/#dsd-dimensions}{dimension} in the definition
#' of the DataSet is different from the 'dimension' definition of the R language.
#' @inheritParams dataset
#' @export
#' @examples
#' df <- data.frame ( sex = c("M", "F"), value = c(1,2))
#' dimensions(df, sdmx_attributes = "sex") <- "sex"
dimensions <- function(x) attr(x, "dimensions")

#' @inheritParams dataset
#' @rdname dimensions
#' @export
`dimensions<-` <- function(x, value, sdmx_attributes = NULL) {

  if ( any(is.null(value) | is.na(value) | length(value)==0) ) {
    attr(x, "dimensions") <- NULL
    return(x)}

  if ( is.numeric(value)) {
    selection <- names(x)[value]
  } else {
    selection <- names(x)[which(names(x) %in% value)]
  }

  dimensions_subset <- subset(x, select = selection)

  dimensions_df <- data.frame(
    names  = names(dimensions_subset),
    class = sapply(dimensions_subset, function(x) paste(class(x), collapse="|")),
    isDefinedBy = rep("http://purl.org/linked-data/cube|https://raw.githubusercontent.com/UKGovLD/publishing-statistical-data/master/specs/src/main/vocab/sdmx-attribute.ttl", length(names(dimensions_subset))),
    codeListe = rep("not yet defined", length(names(dimensions_subset)))
  )


  attr(x, "dimensions") <-  dimensions_df

  x
}

#' @title Measures of a dataset
#' @details See the W3C and SDMX definition of a \href{https://www.w3.org/TR/vocab-data-cube/#dsd-dimensions}{measure}.
#' @inheritParams dataset
#' @export
measures <- function(x) attr(x, "measures")

#' @inheritParams measures
#' @rdname measures
#' @export
#' @examples
#' df <- data.frame ( sex = c("M", "F"), value = c(1,2))
#' measures(df) <- "value"
#' measures(df)

`measures<-` <- function(x, value) {

  if ( any(is.null(value) | is.na(value) | length(value)==0) ) return(x)

  if ( is.numeric(value)) {
    selection <- names(x)[value]
  } else {
    selection <- names(x)[which(names(x) %in% value)]
  }

  subset_measures <- subset(x, select = selection)

  measures_df <- data.frame(
    names  = names(subset_measures),
    class = sapply(subset_measures, function(x) paste(class(x), collapse="|")),
    isDefinedBy = rep("http://purl.org/linked-data/cube", length(names(subset_measures))),
    codeListe = rep("not yet defined", length(names(subset_measures)))
  )

  attr(x, "measures") <-   measures_df

  x
}

#' @title Attributes of a dataset
#' @details Do not confuse with \code{base::\link{attributes}}, which applies to the attributes
#' of the entire dataset, and not each observation (measurement) row.
#' See the W3C and SDMX definition of a \href{https://www.w3.org/TR/vocab-data-cube/#dsd-dimensions}{attribute}.
#' @inheritParams dataset
#' @export
attributes_measures <- function(x) attr(x, "attributes")

#' @inheritParams dataset
#' @param sdmx_attributes The optional SDMX dimensions.
#' @rdname attributes_measures
#' @export
`attributes_measures<-` <- function(x, value, sdmx_attributes = NULL) {

  if ( any(is.null(value) | is.na(value) | length(value)==0) ) return(x)

  if ( is.numeric(value)) {
    selection <- names(x)[value]
  } else {
    selection <- names(x)[which(names(x) %in% value)]
  }

  subset_attributes <- subset(x, select = selection)

  attributes_df <- data.frame(
    names  = names(subset_attributes),
    class = sapply(subset_attributes, function(x) paste(class(x), collapse="|")),
    isDefinedBy = rep("http://purl.org/linked-data/cube|https://raw.githubusercontent.com/UKGovLD/publishing-statistical-data/master/specs/src/main/vocab/sdmx-attribute.ttl", length(names(subset_attributes))),
    codeListe = rep("not yet defined", length(names(subset_attributes)))
  )

  attr(x, "attributes") <-  attributes_df

  x
}

#' @keywords internal
dimensions_names <- function(x, dimensions)   {

  if ( is.character(dimensions)) {
    dimensions <- which(names(x) %in% dimensions)
    not_in_dataset <- (! (which (dimensions %in% names(x))))
    if (length(not_in_dataset>0)) {
      warning("dimensions_get(x, dimensions, ...): ", paste(dimensions[not_in_dataset], collapse = ", "), " not in dataset.")
    }
  }

  if ( is.numeric(dimensions) ) {
    not_in_dataset <- dimensions[which (! dimensions %in% 1:ncol(x))]

    if (length(not_in_dataset>0)) {
      warning("dimensions_get(x, dimensions, ...): ", paste(not_in_dataset, collapse = ", "), " not in dataset.")
    }

    if ( all(dimensions %in% not_in_dataset)) {
      stop("dimensions_get(x, dimensions, ...): none of ", paste(dimensions, collapse = ", "), " in dataset.")
    }
    names(x)[as.integer(dimensions[! dimensions %in% not_in_dataset])]
  }
}

#' @keywords internal
measures_names <- function(x, measures)   {

  if ( is.character(measures)) {
    measures <- which(names(x) %in% measures)
    not_in_dataset <- (! (which (measures %in% names(x))))
    if (length(not_in_dataset>0)) {
      warning("measures_get(x, measures, ...): ", paste(measures[not_in_dataset], collapse = ", "), " not in dataset.")
    }
  }

  if ( is.numeric(measures) ) {
    not_in_dataset <- measures[which (! measures %in% 1:ncol(x))]

    if (length(not_in_dataset>0)) {
      warning("measures_get(x, measures, ...): ", paste(not_in_dataset, collapse = ", "), " not in dataset.")
    }

    if ( all(measures %in% not_in_dataset)) {
      stop("measures_get(x, measures, ...): none of ", paste(measures, collapse = ", "), " in dataset.")
    }
    names(x)[as.integer(measures[! measures %in% not_in_dataset])]
  }
}

#' @keywords internal
attributes_names <- function(x, attributes)   {

  if ( is.character(attributes)) {
    attributes <- which(names(x) %in% attributes)
    not_in_dataset <- (! (which (attributes %in% names(x))))
    if (length(not_in_dataset>0)) {
      warning("attributes_get(x, attributes, ...): ", paste(attributes[not_in_dataset], collapse = ", "), " not in dataset.")
    }
  }

  if ( is.numeric(attributes) ) {
    not_in_dataset <- attributes[which (! attributes %in% 1:ncol(x))]

    if (length(not_in_dataset>0)) {
      warning("attributes_get(x, attributes, ...): ", paste(not_in_dataset, collapse = ", "), " not in dataset.")
    }

    if ( all(attributes %in% not_in_dataset)) {
      stop("attributes_get(x, attributes, ...): none of ", paste(attributes, collapse = ", "), " in dataset.")
    }
    names(x)[as.integer(attributes[! attributes %in% not_in_dataset])]
  }
}

#' @keywords internal
#' @importFrom rlang get_expr
dot.names <- function (dots) {
  if (length (dots) > 0 && is.null (get_expr (dots [[1]]))) { return (NULL) }
  dot_names <- dots
  if (length (dot_names) == 0) { return (character(0)) }
  dot_names
}

#' @keywords internal
arg.names <- function (args) {
  if (deparse (args) == "c()") { return (character(0)) }
  if (deparse (args) == "list()") { return (character(0)) }
  if (is.null (args)) { return (NULL) }
  names <- sapply (args, deparse)
  if (length (names) > 1) { names <- names [-1] }
  return (names)
}
