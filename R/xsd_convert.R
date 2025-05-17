#' @title Convert to XML Schema Definition (XSD) types
#' @description Convert the numeric, boolean and Date/time columns of a dataset
#'   \code{xs:decimal}, \code{xsLboolean}, \code{xs:date} and
#'   \code{xs:dateTime}.
#' @param x An object to be coerced to an XLM Schema defined string format.
#' @param idcol The name or position of the column that contains the row
#'   (observation) identifiers. If \code{NULL}, it will make a new \code{idcol}
#'   from [row.names()].
#' @param ... Further optional parameters for generic method.
#' @return
#' A character vector of RDF-compatible typed literals. Each element
#' corresponds to an input value, serialized according to its type
#' (e.g., `xs:string`, `xs:integer`, `xs:dateTime`).
#' For data frames or tibbles, each row is converted into a set of
#' RDF triples, with columns mapped to predicates.
#' @export

xsd_convert <- function(x, idcol, ...) {
  UseMethod("xsd_convert", x)
}

#' @keywords internal
get_type <- function(t) {
  if (any(class(t) %in% c("numeric", "double"))) {
    type <- "xs:decimal"
  } else if (any(class(t) == "integer")) {
    type <- "xs:integer"
  } else if (any(class(t) %in% c("character", "factor"))) {
    type <- "xs:string"
  } else if (any(class(t) == "logical")) {
    type <- "xs:boolean"
  } else if (any(class(t) == "numeric")) {
    type <- "xs:decimal"
  } else if (any(class(t) == "Date")) {
    type <- "xs:date"
  } else if (any(class(t) == "POSIXct")) {
    type <- "xs:dateTime"
  } else if (any(class(t) == "difftime")) {
    type <- "xs:duration"
  }
  type
}

#' @rdname xsd_convert
#' @examples
#' # Convert data.frame to XML Schema Definition
#' xsd_convert(data.frame(a = 1:3, b = c("a", "b", "c")))
#' @exportS3Method
xsd_convert.data.frame <- function(x, idcol = NULL, ...) {
  # Identify ID column (or default to row names)
  if (!is.null(idcol)) {
    id_idx <- idcol_find(x, idcol)
    ids <- as.character(x[[id_idx]])
    id_name <- names(x)[id_idx]
  } else {
    ids <- row.names(x)
    id_name <- ".rowid"
  }

  convert_cols <- setdiff(seq_along(x),
                          if (!is.null(idcol)) id_idx else integer(0))

  # Apply xsd_convert to all non-ID columns
  xsd_list <- lapply(convert_cols, function(c) xsd_convert(x[[c]], ...))
  names(xsd_list) <- names(x)[convert_cols]

  # Assemble result: ID column first, then converted columns
  result <- c(
    setNames(list(ids), id_name),
    xsd_list
  )

  as.data.frame(result, stringsAsFactors = FALSE)
}

#' @rdname xsd_convert
#' @examples
#' # Convert dataset to XML Schema Definition
#' xsd_convert(head(dataset_df(orange_df)))
#' @export
#' @exportS3Method
xsd_convert.dataset_df <- function(x, idcol = "rowid", ...) {
  NextMethod()
}

#' @rdname xsd_convert
#' @export
#' @exportS3Method
xsd_convert.tbl_df <- function(x, idcol = NULL, ...) {
  if (!inherits(x, "data.frame")) {
    stop("xsd_convert.tbl_df: input must be a data frame or tibble.")
  }
  NextMethod("xsd_convert")
}

#' @rdname xsd_convert
#' @export
#' @examples
#' # Convert characters:
#' xsd_convert(c("apple", " banana ", "cherry"))
#'
#' # To handle whitespace:
#' xsd_convert(trimws(c("apple", " banana ", "cherry"), "both"))
#' @exportS3Method
xsd_convert.character <- function(x, idcol = NULL, ...) {
  var_type <- "xs:string"

  if (length(x) == 0) {
    return('""^^<xs:string>')
  }

  ifelse(is.na(x),
         NA_character_,
         paste0('"',x ,
                '"^^<', var_type, '>'))
}

#' @rdname xsd_convert
#' @export
#' @examples
#' # Convert integers or doubles, numbers:
#' xsd_convert(1:3)
#' @exportS3Method
xsd_convert.numeric <- function(x, idcol = NULL, ...) {
  var_type <- "xs:decimal"

  if (length(x) == 0) {
    return('""^^<xs:decimal>')
  }

  formatted_number <- ifelse(is.na(x), NA_character_, {
    str <- format(x, scientific = FALSE, trim = TRUE, justify = "none")
    # Remove .0 for whole numbers
    sub("\\.0$", "", str)
  })


  ifelse(is.na(formatted_number),
         NA_character_,
         paste0('"', formatted_number, '"^^<', var_type, '>'))
}

#' @rdname xsd_convert
#' @export
#' @exportS3Method
xsd_convert.haven_labelled_defined <- function(x, idcol = NULL, ...) {
  type <- get_type(x)
  if (type == "xs:decimal") {
    return(xsd_convert(as_numeric(x)))
  }
  if (type == "xs:integer") {
    return(xsd_convert(as_numeric(x)))
  }
  if (type == "xs:string") {
    return(xsd_convert(as_character(x)))
  }
  if (type == "xs:boolean") {
    return(xsd_convert(as.logical(as_numeric(x))))
  }
}

#' @rdname xsd_convert
#' @export
#' @exportS3Method
xsd_convert.integer <- function(x, idcol = NULL, ...) {
  var_type <- "xs:integer"

  if (length(x) == 0) {
    return('""^^<xs:integer>')
  }

  ifelse(is.na(x),
         NA_character_,
         paste0('"', as.character(x), '"^^<', var_type, '>'))
}

#' @rdname xsd_convert
#' @exportS3Method
#' @examples
#' # Convert logical values:
#' xsd_convert(TRUE)
#' @export
xsd_convert.logical <- function(x, idcol = NULL, ...) {
  var_type <- "xs:boolean"

  if (length(x) == 0) {
    return('""^^<xs:boolean>')
  }

  ifelse(is.na(x),
         NA_character_,
         paste0('"', tolower(as.character(x)), '"^^<', var_type, '>'))
}


#' @rdname xsd_convert
#' @export
#' @examples
#' xsd_convert(factor(c("apple", "banana", "cherry")))
#' @exportS3Method
xsd_convert.factor <- function(x, idcol = NULL, ...) {
  args <- list(...)
  codelist <- args$codelist %||% NULL

  if (length(x) == 0) {
    return('""^^<xs:string>')
  }

  if (is.null(codelist)) {
    var_type <- "xs:string"
    ifelse(is.na(x),
           NA_character_,
           paste0('"', as.character(x), '"^^<', var_type, '>'))
  } else {
    ifelse(is.na(x),
           NA_character_,
           paste0(codelist, ":", as.character(x)))
  }
}

#' @rdname xsd_convert
#' @export
#' @exportS3Method
xsd_convert.POSIXct <- function(x, idcol = NULL, ...) {
  var_type <- "<xs:dateTime>"
  if (length(x) == 0) return('""^^<xs:dateTime>')

  time_string <- strftime(x, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")

  ifelse(is.na(x),
         NA_character_,
         paste0('"', time_string, '"^^<xs:dateTime>'))
}

#' @rdname xsd_convert
#' @export
#' @exportS3Method
xsd_convert.Date <- function(x, idcol = NULL, ...) {
  var_type <- "xs:date"

  if (length(x) == 0) {
    return('""^^<xs:date>')
  }

  date_str <- format(x, format = "%Y-%m-%d")

  ifelse(is.na(x),
         NA_character_,
         paste0('"', date_str, '"^^<', var_type, '>'))
}

#' @rdname xsd_convert
#' @export
#' @examples
#' xsd_convert(as.difftime(c(3600, 5400), units = "secs"))
#' @exportS3Method
xsd_convert.difftime <- function(x, idcol = NULL, ...) {
  var_type <- "xs:duration"
  if (length(x) == 0) return('""^^<xs:duration>')

  seconds <- unclass(x)
  unit <- attr(x, "units")

  # Convert to seconds based on original units
  multiplier <- switch(unit,
                       "secs" = 1,
                       "mins" = 60,
                       "hours" = 3600,
                       "days" = 86400,
                       stop("Unsupported difftime unit: ", unit)
  )

  seconds <- seconds * multiplier

  convert_to_iso8601_duration <- function(s) {
    if (is.na(s)) return(NA_character_)
    h <- floor(s / 3600)
    m <- floor((s %% 3600) / 60)
    sec <- round(s %% 60)
    paste0("PT",
           if (h > 0) paste0(h, "H") else "",
           if (m > 0) paste0(m, "M") else "",
           if (sec > 0 || (h == 0 && m == 0)) paste0(sec, "S") else "")
  }

  iso_strs <- vapply(seconds, convert_to_iso8601_duration, character(1))
  ifelse(is.na(seconds),
         NA_character_,
         paste0('"', iso_strs, '"^^<', var_type, '>'))
}
