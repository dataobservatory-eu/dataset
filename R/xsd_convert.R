#' Convert to XML Schema Definition (XSD) Types
#'
#' Converts R vectors, data frames, and `dataset_df` objects to
#' [XML Schema Definition (XSD)](https://www.w3.org/TR/xmlschema11-2/)
#' compatible string representations such as `xs:decimal`, `xs:boolean`,
#' `xs:date`, and `xs:dateTime`.
#'
#' This is primarily used for generating RDF-compatible typed literals.
#'
#' @details
#' - For **vectors**, returns a character vector of typed literals.
#' - For **data frames** or tibbles, returns a data frame with the same
#'   structure but with all values converted to XSD strings.
#' - For `dataset_df` objects, behaves like the data frame method but
#'   preserves dataset-level attributes.
#'
#' @param x An object (vector, data frame, tibble, or `dataset_df`).
#' @param idcol Column name or position to use as row (observation) identifier.
#'   If `NULL`, row names are used.
#' @param ... Additional arguments passed to methods.
#'
#' @return
#' A character vector or data frame with values serialized as XSD-compatible
#' RDF literals.
#'
#' @examples
#' # Simple data frame with mixed types
#' df <- data.frame(
#'   id     = 1:2,
#'   value  = c(3.14, 2.71),
#'   active = c(TRUE, FALSE),
#'   date   = as.Date(c("2020-01-01", "2020-12-31"))
#' )
#'
#' @section Class-specific examples:
#' ```r
#' xsd_convert(42L)                   # integer -> xs:integer
#' xsd_convert(c(TRUE, FALSE, NA))    # logical -> xs:boolean
#' xsd_convert(Sys.Date())            # Date -> xs:date
#' xsd_convert(Sys.time())            # POSIXct -> xs:dateTime
#' xsd_convert(factor("apple"))       # factor -> xs:string
#' xsd_convert(c("apple", "banana"))  # character -> xs:string
#' ```
#' @family RDF and linked data helpers
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

  convert_cols <- setdiff(
    seq_along(x),
    if (!is.null(idcol)) id_idx else integer(0)
  )

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
#' @exportS3Method
xsd_convert.dataset_df <- function(x, idcol = "rowid", ...) {
  NextMethod()
}

#' @rdname xsd_convert
#' @exportS3Method
xsd_convert.tbl_df <- function(x, idcol = NULL, ...) {
  if (!inherits(x, "data.frame")) {
    stop("xsd_convert.tbl_df: input must be a data frame or tibble.")
  }
  NextMethod("xsd_convert")
}

#' @rdname xsd_convert
#' @exportS3Method
xsd_convert.character <- function(x, idcol = NULL, ...) {
  var_type <- "xs:string"

  if (length(x) == 0) {
    return('""^^<xs:string>')
  }

  ifelse(is.na(x),
    NA_character_,
    paste0(
      '"', x,
      '"^^<', var_type, ">"
    )
  )
}

#' @rdname xsd_convert
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
    paste0('"', formatted_number, '"^^<', var_type, ">")
  )
}

#' @rdname xsd_convert
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
#' @exportS3Method
xsd_convert.integer <- function(x, idcol = NULL, ...) {
  var_type <- "xs:integer"

  if (length(x) == 0) {
    return('""^^<xs:integer>')
  }

  ifelse(is.na(x),
    NA_character_,
    paste0('"', as.character(x), '"^^<', var_type, ">")
  )
}

#' @rdname xsd_convert
#' @exportS3Method
xsd_convert.logical <- function(x, idcol = NULL, ...) {
  var_type <- "xs:boolean"

  if (length(x) == 0) {
    return('""^^<xs:boolean>')
  }

  ifelse(is.na(x),
    NA_character_,
    paste0('"', tolower(as.character(x)), '"^^<', var_type, ">")
  )
}


#' @rdname xsd_convert
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
      paste0('"', as.character(x), '"^^<', var_type, ">")
    )
  } else {
    ifelse(is.na(x),
      NA_character_,
      paste0(codelist, ":", as.character(x))
    )
  }
}

#' @rdname xsd_convert
#' @exportS3Method
xsd_convert.POSIXct <- function(x, idcol = NULL, ...) {
  if (length(x) == 0) {
    return('""^^<xs:dateTime>')
  }

  time_string <- strftime(x,
    format = "%Y-%m-%dT%H:%M:%SZ",
    tz = "UTC"
  )

  ifelse(is.na(x),
    NA_character_,
    paste0('"', time_string, '"^^<xs:dateTime>')
  )
}

#' @rdname xsd_convert
#' @exportS3Method
xsd_convert.Date <- function(x, idcol = NULL, ...) {
  var_type <- "xs:date"

  if (length(x) == 0) {
    return('""^^<xs:date>')
  }

  date_str <- format(x, format = "%Y-%m-%d")

  ifelse(is.na(x),
    NA_character_,
    paste0('"', date_str, '"^^<', var_type, ">")
  )
}

#' @rdname xsd_convert
#' @exportS3Method
xsd_convert.difftime <- function(x, idcol = NULL, ...) {
  var_type <- "xs:duration"
  if (length(x) == 0) {
    return('""^^<xs:duration>')
  }

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
    if (is.na(s)) {
      return(NA_character_)
    }
    h <- floor(s / 3600)
    m <- floor((s %% 3600) / 60)
    sec <- round(s %% 60)
    paste0(
      "PT",
      if (h > 0) paste0(h, "H") else "",
      if (m > 0) paste0(m, "M") else "",
      if (sec > 0 || (h == 0 && m == 0)) paste0(sec, "S") else ""
    )
  }

  iso_strs <- vapply(seconds, convert_to_iso8601_duration, character(1))
  ifelse(is.na(seconds),
    NA_character_,
    paste0('"', iso_strs, '"^^<', var_type, ">")
  )
}
