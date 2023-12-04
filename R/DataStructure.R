#' @title Data structure
#' @description Read or update the Data Structure Definition of a dataset.
#' @param x A dataset.
#' @return A dataset object, which is a data.frame or inherited object with rich
#' metadata.
#' @importFrom assertthat assert_that
#' @examples
#' DataStructure(iris_dataset)
#' @export

DataStructure <- function(x) {

  if (!is.dataset(x)) {
    stop("DataStructure(x): x must be a dataset created by dataset() or as_dataset().")
  }

  attr(x, "DataStructure")
}

#' @rdname DataStructure
#' @param value A DataStructure definition.
#' @examples
#' dsd_iris <- DataStructure(iris_dataset)
#' dsd_iris$Sepal.Length$label <- "The sepal length measured in centimeters."
#' dsd_iris$Sepal.Width$label  <- "The sepal width measured in centimeters."
#' dsd_iris$Petal.Length$label <- "The petal length measured in centimeters."
#' dsd_iris$Petal.Width$label  <- "The petal width measured in centimeters."
#' dsd_iris$Species$label      <- "The name of the Iris species in the observation."
#'
#' iris_dataset_labelled <- DataStructure_update(iris_dataset, dsd_iris)
#'
#' vapply(DataStructure(iris_dataset_labelled), function(x) x$label, character(1))
#' @export

DataStructure_update <- function(x, value) {

  if (!is.dataset(x)) {
    stop("DataStructure_update(x): x must be a dataset created by dataset() or as_dataset().")
  }
  attr(x, "DataStructure") <- value

  invisible(x)
}

#' @title Initialise a DataStructure (internal)
#' @param df An object inherited from data.frame.
#' @param col The column number that needs to be initialised.
#' @return A list containing the data structure definition.
#' @keywords internal
initialise_dsd <- function(df, col) {
  r_class   <- class(df[,col])
  col_class <- "xsd:string"
  col_class <- ifelse ("numeric" %in% r_class, "xsd:decimal", col_class)
  col_class <- ifelse ("logical" %in% r_class, "xsd:boolean", col_class)
  col_class <- ifelse ("integer" %in% r_class, "xsd:integer", col_class)
  col_class <- ifelse ("factor"  %in% r_class, "coded",       col_class)
  # find other labelled uses

  var <-  list ( var1 = list(name = names(df)[col],
                             label = list (""),
                             type = "",
                             range = col_class,
                             comment = "",
                             concept = list ( heading = "",
                                              schemeURI = "",
                                              valueURI = ""),
                             defintion = list ( schemeURI = "",
                                                valueURI = ""))
  )

  names(var) <- names(df)[col]
  var
}

#' @rdname initialise_dsd
initialize_dsd <- initialise_dsd
