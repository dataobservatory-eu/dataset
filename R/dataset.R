#' @title Structure a data frame to dataset
#' @description A DataSet is a collection of statistical data that corresponds to a defined structure.
#' @details Loosely follows the \href{https://www.w3.org/TR/vocab-data-cube/}{The RDF Data Cube Vocabulary},
#' but without the definition of data slices.\cr
#' \code{bibentry_dataset()} is a wrapper around \code{\link[utils:bibentry]{bibentry}} to correctly turn the
#' metadata of the dataset into a bibentry object.
#' @param x A data.frame or inherited tibble, data.frame, or a structured list.
#' @param dimensions The name or column number of the dimensions within the dataset.
#' @param measures The name or column number of the measures within the dataset.
#' @param attributes The name or column number of the attributes within the dataset.
#' @param sdmx_attributes The optional dimensions and attributes that conform with
#' SDMX. \code{c("time", "geo")} will mark the "time" and "geo" attributes as conforming to
#' sdmx. See \href{https://raw.githubusercontent.com/UKGovLD/publishing-statistical-data/master/specs/src/main/vocab/sdmx-attribute.ttl}{sdmx-attribute}.
#' @inheritParams dublincore_add
#' @param Type It is set by default to \href{http://purl.org/dc/dcmitype/Dataset}{DCMITYPE:Dataset}.
#' @param Issued Corresponds to \href{http://purl.org/dc/elements/1.1/date}{dct:date}.
#' @importFrom utils toBibtex
#' @examples
#' my_dataset <- dataset (x,
#' dimensions = c(1,2),
#' measures = 3,
#' attributes = c(4,5),
#' sdmx_attributes = c("time", "freq"),
#' title = "Example dataset",
#' creator = person("Jane", "Doe"),
#' publisher = "Publishing Co.",
#' issued = as.Date("2022-07-14")
#' )
#'
#' utils::toBibtex(bibentry_dataset(my_dataset))
#'
#' @export

dataset <- function(x,
                    dimensions = NULL,
                    measures = NULL,
                    attributes = NULL,
                    sdmx_attributes = NULL,
                    Title = NULL,
                    Label = NULL,
                    Creator = NULL,
                    Publisher = NULL,
                    Issued = NULL,
                    Type = list ( type = "Dataset",
                                  URI = "http://purl.org/dc/dcmitype/Dataset")
                    ) {

  x <- dimensions_add(x, dimensions, sdmx_attributes)
  x <- measures_add(x, measures)
  x <- attributes_add(x, attributes, sdmx_attributes)

  attr(x, "Title") <- Title
  attr(x, "Label") <- Label
  attr(x, "Creator") <- Creator
  attr(x, "Publisher") <- Publisher
  if (is.null(Issued)) Issued <- Sys.Date()
  attr(x, "Date") <- Issued
  attr(x, "Type") <- Type

  attr(x, "class") <- c("dataset", class(x))
  x
}

print <- function(x, ...) { UseMethod("print")}
summary <- function(x, ...) { UseMethod("summary")}

#' @rdname dataset
#' @export
summary.dataset <- function(x, ...) { NextMethod()}

#' @rdname dataset
#' @export
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


#' @inheritParams dataset
#' @importFrom utils bibentry
#' @rdname dataset
#' @family citation functions
#' @export
bibentry_dataset <- function(x) {

  extractyear <- function(date) {
    format(date, format="%Y")
  }

  if ( is.numeric(attr(x, "Issued"))) {
    year_nr <- attr(x, "Issued")
  } else if (inherits(attr(x, "Issued"), "POSIXt") | inherits(attr(x, "issued"), "Date")) {
    year_nr <- extractyear (attr(x, "Issued"))
  } else {
    year_nr = attr(x, "Issued")}

  bibentry(
    bibtype = "Misc",
    title = attr(x, "Title"),
    author = attr(x, "Creator"),
    publisher = attr(x, "Publisher"),
    size = attr(x, "Size"),
    year = year_nr)
}


#' @title Dimensions of a dataset
#' @details Do not confuse with \code{base::\link{dim}}. The \href{https://www.w3.org/TR/vocab-data-cube/#dsd-dimensions}{dimension} in the definition
#' of the DataSet is different from the 'dimension' definition of the R language.
#' @inheritParams dataset
#' @export
#' @seealso \code{\link{base::dim}}
#' @examples
#' df <- data.frame ( sex = c("M", "F"), value = c(1,2))
#' dimensions_add(df, "sex", sdmx_attributes = "sex")
dimensions <- function(x) attr(x, "dimensions")

#' @inheritParams dataset
#' @rdname dimensions

dimensions_add <- function(x, dimensions, sdmx_attributes = NULL) {

  if ( any(is.null(dimensions) | is.na(dimensions) | length(dimensions)==0) ) return(x)

  ss <- subset(x, select = dimensions)

  defined_by <- function(s) {
    isDefinedBy = ifelse (names(ss) %in% sdmx_attributes,
                          "https://raw.githubusercontent.com/UKGovLD/publishing-statistical-data/master/specs/src/main/vocab/sdmx-attribute.ttl",
                          "http://purl.org/linked-data/cube")
    names(isDefinedBy) <- names(ss)
    isDefinedBy
  }

  attr(x, "dimensions") <-  list ( names = names(ss),
         class = sapply (ss, class),
         isDefinedBy = defined_by(ss),
         codelist = sapply(ss, function(x) NULL))

  x
}

#' @title Measures of a dataset
#' @details See the W3C and SDMX definition of a \href{https://www.w3.org/TR/vocab-data-cube/#dsd-dimensions}{measure}.
#' @inheritParams dataset
#' @export
measures <- function(x) attr(x, "measures")

#' @inheritParams measures
#' @param sdmx_attributes The optional SDMX dimensions.
#' @rdname measures
#' @export
#' @examples
#' df <- data.frame ( sex = c("M", "F"), value = c(1,2))
#' df <- measures_add(df, "value")
#' measures(df)

measures_add <- function(x, measures) {

  if ( any(is.null(measures) | is.na(measures) | length(measures)==0) ) return(x)

  ss <- subset(x, select = measures)


  attr(x, "measures") <-  list ( names = names(ss),
                                   class = sapply (ss, class),
                                   isDefinedBy =   "http://purl.org/linked-data/cube",
                                   codelist = sapply(ss, function(x) NULL))

  x
}

#' @title Attributes of a dataset
#' @details Do not confuse with \code{base::\link{attributes}}.
#' See the W3C and SDMX definition of a \href{https://www.w3.org/TR/vocab-data-cube/#dsd-dimensions}{attribute}.
#' @inheritParams dataset
#' @export
attributes_dataset <- function(x) attr(x, "attributes")

#' @inheritParams dataset
#' @rdname attributes_dataset
#' @export

attributes_add <- function(x, attributes, sdmx_attributes = NULL) {

  if ( any(is.null(attributes) | is.na(attributes) | length(attributes)==0) ) return(x)

  ss <- subset(x, select = attributes)

  defined_by <- function(s) {
    isDefinedBy = ifelse (names(ss) %in% sdmx_attributes,
                          "https://raw.githubusercontent.com/UKGovLD/publishing-statistical-data/master/specs/src/main/vocab/sdmx-attribute.ttl",
                          "http://purl.org/linked-data/cube")
    names(isDefinedBy) <- names(ss)
    isDefinedBy
  }

  attr(x, "attributes") <-  list ( names = names(ss),
                                   class = sapply (ss, class),
                                   isDefinedBy = defined_by(ss),
                                   codelist = sapply(ss, function(x) NULL))

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
