#' @title Convert to XML Schema Definition (XSD) types
#' @description Convert the numeric, boolean and Date/time columns of a dataset
#'   \code{xs:decimal}, \code{xsLboolean}, \code{xs:date} and
#'   \code{xs:dateTime}.
#' @param x An object to be coerced to an XLM Schema defined string format.
#' @param idcol The name or position of the column that contains the row
#'   (observation) identifiers. If \code{NULL}, it will make a new \code{idcol}
#'   from [row.names()].
#' @param ... Further optional parameters for generic method.
#' @return A serialisation of an R vector or data frame (dataset) in XML.
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
  }

  type
}

#' @rdname xsd_convert
#' @examples
#'
#' # Convert data.frame to XML Schema Definition
#' xsd_convert(data.frame( a=1:3, b=c("a", "b", "c")))
#' @exportS3Method
#' @export
xsd_convert.data.frame <- function(x, idcol = NULL, ...) {
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
    }

    type
  }

  convert_cols <- seq_along(x)

  if (!is.null(idcol)) {
    ## See utils-idcol_find.R for the internal function
    convert_cols <- convert_cols[-idcol_find(x = x, idcol = idcol)]
  }

  convert_column <- function(c) {
    var_type <- get_type(t = x[[c]])
    if (!var_type %in% c("codelist", "literal")) {
      paste0('\"', as.character(x[[c]]), '\"', "^^<", var_type, ">")
    } else {
      as.character(x[[c]])
    }
  }

  xsd_list <- lapply(convert_cols, function(c) convert_column(c))
  xsd_dataframe <- as.data.frame(xsd_list)

  idcol <- which(!seq_along(x) %in% convert_cols)
  if (length(idcol) == 1) {
    xsd_dataframe <- cbind(x[, idcol], xsd_dataframe)
    names(xsd_dataframe) <- names(x)
  } else {
    names(xsd_dataframe) <- names(x)
  }

  tmp_df <- x
  for (j in seq_along(tmp_df)) {
    tmp_df[, j] <- as.character(tmp_df[, j])
    tmp_df[, j] <- xsd_dataframe[, j]
  }

  tmp_df
}

#' @rdname xsd_convert
#' @examples
#'
#' # Convert dataset to XML Schema Definition
#' xsd_convert(head(dataset_df(orange_df)))
#' @export

#' @exportS3Method
xsd_convert.dataset <- function(x, idcol = NULL, ...) {
  NextMethod()
}

#' @rdname xsd_convert
#' @export
#' @exportS3Method
xsd_convert.tibble <- function(x, idcol = NULL, ...) {
  NextMethod()
}

#' @rdname xsd_convert
#' @export
#' @exportS3Method
xsd_convert.character <- function(x, idcol = NULL, ...) {
  var_type <- "xs:string"
  paste0('\"', x, '\"', "^^<", var_type, ">")
}

#' @rdname xsd_convert
#' @export
#' @examples
#' # Convert integers or doubles, numbers:
#' xsd_convert(1:3)
#' @exportS3Method
xsd_convert.numeric <- function(x, idcol = NULL, ...) {
  var_type <- "xs:decimal"
  paste0('\"', as.character(x), '\"', "^^<", var_type, ">")
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
  paste0('\"', as.character(x), '\"', "^^<", var_type, ">")
}

#' @rdname xsd_convert
#' @exportS3Method
#' @examples
#' # Convert logical values:
#' xsd_convert(TRUE)
#' @export
xsd_convert.logical <- function(x, idcol = NULL, ...) {
  var_type <- "xs:boolean"
  paste0('\"', as.character(x), '\"', "^^<", var_type, ">")
}

#' @rdname xsd_convert
#' @export
#' @exportS3Method
xsd_convert.factor <- function(x, idcol = NULL, ...) {
  codelist <- NULL
  args <- list(...)

  if (codelist %in% names(args)) {
    codelist <- args$codelist
  }

  if (is.null(codelist)) {
    var_type <- "xs:string"
    paste0('\"', x, '\"', "^^<", var_type, ">")
  } else {
    paste0(codelist, ":", as.character(x))
  }
}

#' @rdname xsd_convert
#' @export
#' @exportS3Method
xsd_convert.POSIXct <- function(x, idcol = NULL, ...) {
  time_utc <- as.POSIXct(x, tz = "UTC")
  time_string <- paste0(
    as.character(as.Date(time_utc)), "T",
    strftime(time_utc, format = "%H:%M:%S"), "Z"
  )

  paste0('\"', time_string, '\"', "^^<xs:dateTime>")
}

#' @rdname xsd_convert
#' @export
#' @exportS3Method
xsd_convert.Date <- function(x, idcol = NULL, ...) {
  paste0('\"', paste0(as.character(as.Date(x))), '\"', "^^<xs:date>")
}
