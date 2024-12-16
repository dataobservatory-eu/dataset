#' @title Get / set a definition for a vector or a dataset
#' @param x a vector
#' @param value a character string or `NULL` to remove the definition of measure.
#' @param ... Further parameters for inheritance, not in use.
#' @details
#'   `get_variable_definitions()` is identical to `var_definition()`.
#' @examples
#' small_country_dataset <- dataset_df(
#'   country_name = defined(c("Andorra", "Lichtenstein"), label  = "Country"),
#'   gdp = defined(c(3897, 7365),
#'                       label = "Gross Domestic Product",
#'                       unit = "million dollars")
#' )
#' var_definition(small_country_dataset$country_name) <- "http://data.europa.eu/bna/c_6c2bb82d"
#' var_definition(small_country_dataset$country_name)
#' # To remove a definition of measure
#' var_definition(small_country_dataset$country_name) <- NULL
#' @export
var_definition <- function(x, ...) {
  #rlang::check_dots_used()
  UseMethod("var_definition")
}

#' @export
var_definition.default <- function(x, ...) {
  attr(x, "definition", exact = TRUE)
}

#' @rdname var_definition
#' @export
`var_definition<-` <- function(x, value) {
  UseMethod("var_definition<-")
}

#' @export
`var_definition<-.default` <- function(x, value) {
  definition_attribute(x) <- value
  x
}

#get_variable_definitions <- var_definition


#' @rdname var_definition
#' @export
definition_attribute <- function(x) {
  attr(x, "definition", exact = TRUE)
}


#' @rdname var_definition
#' @export
get_definition_attribute <- function(x) {
  definition_attribute(x)
}


#' @rdname var_definition
#' @export
set_definition_attribute <- function(x, value) {
  if ((!is.character(value) && !is.null(value)) || length(value) > 1) {
    stop(
      "`definition` should be a single character string or NULL",
      call. = FALSE,
      domain = "R-dataset"
    )
  }
  attr(x, "definition") <- value
  x
}


#' @rdname var_definition
#' @export
`definition_attribute<-` <- set_definition_attribute



#set_var_definitions <- function(dataset, definitions) {

#  var_definition_list <- list()
#  var_definition_list <- lapply(colnames(dataset), function(i) i)
#  names(var_definition_list) <- colnames(dataset)#

#  for (rn in which(names(var_definition_list) %in% names(var_labels))) {
#    var_definition_list[[rn]] <- var_labels[[which(names(var_definition_list)[rn]==names(var_labels))]]
#  }

#  attr(dataset, "var_definitions") <- var_definition_list

#  dataset
#}
