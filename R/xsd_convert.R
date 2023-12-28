#' @title Convert to XSD types
#' @description Convert the numeric, boolean and Date/time columns of a dataset
#' \code{xs:decimal}, \code{xsLboolean}, \code{xs:date} and \code{xs:dateTime}.
#' @inheritParams dataset
#' @seealso [dataset()]
#' @param ... Further optional parameters for generic method.
#' @export
xsd_convert <- function(x, idcol, ...) {
  UseMethod("xsd_convert", x)
}

#' @rdname xsd_convert
#' @examples
#'
#' # Convert data.frame to XML Schema Definition
#' xsd_convert(head(iris))
#' @exportS3Method
#' @export
xsd_convert.data.frame <- function(x, idcol=NULL, ...) {
  get_type <- function(t) {

    type <- switch(class(t)[[1]],
                   "numeric"   = "xs:decimal",
                   "factor"    = "codelist",
                   "logical"   = "xs:boolean",
                   "integer"   = "xs:integer",
                   "Date"      = "xs:date",
                   "POSIXct"   = "xs:dateTime",
                   "character" = "xs:string"
    )

    type
  }

  convert_cols <- seq_along(x)

  if(!is.null(idcol)) {
    ## See utils-idcol_find.R for the internal function
    convert_cols <- convert_cols[-idcol_find(x=x, idcol=idcol)]
  }

  convert_column <- function(c) {

    var_type <- get_type(x[[c]])
    if ( ! var_type %in% c("codelist", "literal") ) {
      paste0('\"', as.character(x[[c]]),  '\"', "^^<", var_type, ">")
    } else {
      as.character(x[[c]])
    }
  }

  xsd_list <- lapply ( convert_cols, function(c) convert_column(c))
  xsd_dataframe <-  as.data.frame(xsd_list)

  idcol <- which(! seq_along(x) %in% convert_cols)
  if (length(idcol)==1) {
    xsd_dataframe <- cbind( x[, idcol], xsd_dataframe)
    names(xsd_dataframe) <- names(x)
  } else {
    names(xsd_dataframe) <- names(x)
  }

  tmp_df <- x
  for ( j in seq_along(tmp_df)) {
    tmp_df[,j] <- as.character(tmp_df[, j])
    tmp_df[,j] <- xsd_dataframe[,j]
  }

  tmp_df
}

#' @rdname xsd_convert
#' @examples
#'
#' # Convert dataset to XML Schema Definition
#' xsd_convert(head(iris_dataset))
#' @export
#' @exportS3Method
xsd_convert.dataset <- function(x, idcol=NULL, ...) {
  NextMethod()
}

#' @rdname xsd_convert
#' @export
#' @exportS3Method
xsd_convert.tibble <- function(x, idcol=NULL,...) {
  NextMethod()
}

xsd_convert.character <- function(x, idcol=NULL, ...) {
  var_type <-  "xs:string"
  paste0('\"', x,  '\"', "^^<", var_type, ">")
}

xsd_convert.numeric <- function(x, idcol=NULL, ...) {
  var_type <-  "xs:decimal"
  paste0('\"', as.character(x),  '\"', "^^<", var_type, ">")
}

xsd_convert.integer <- function(x, idcol=NULL, ...) {
  var_type <-  "xs:integer"
  paste0('\"', as.character(x),  '\"', "^^<", var_type, ">")
}

xsd_convert.boolean <- function(x, idcol=NULL, ...) {
  var_type <-  "xs:boolean"
  paste0('\"', as.character(x),  '\"', "^^<", var_type, ">")
}

xsd_convert.factor<- function(x, idcol=NULL, codelist=NULL ) {

  if (is.null(codelist)) {
    var_type <-  "xs:string"
    paste0('\"', x,  '\"', "^^<", var_type, ">")
  } else {
    paste0(codelist, ":", as.character(x))
  }
}
