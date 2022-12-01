#' @title Export a dataset
#' @description Export a dataset together with reference (DataCite and Dublin Core)
#' metadata.
#' @details This function is a wrapper around the exporting functions.It implements
#' file exports in a way that the resulting exported file contains reference metadata.\cr
#' \code{\link{dataset_export_csv}} is a wrapper around \code{utils::\link[utils:write.csv]{write.csv}}.
#' Use ... to pass on argument to that function.
#' @param ds A dataset object.
#' @param file A (path to) a file where to export the dataset object.
#' @param filetype Currently only \code{'csv'} is implemented.
#' @param ... Further parameters to be passed on to exporting functions. See details.
#' @family export functions
#' @return The function write a desired file on disc and does not return anything.
#' @importFrom assertthat assert_that
#' @seealso dataset
#' @export

dataset_export <- function(ds, file, filetype = 'csv', ...) {

  if (filetype != 'csv') {
    stop("dataset_export(.., filetype): Currently only filetype='csv' is implemented.")
  }

  dataset_export_csv(ds, file)
}

#' @rdname dataset_export
#' @importFrom utils write.table write.csv
#' @importFrom assertthat assert_that

#' @examples
#' my_iris_dataset <- dataset(
#'      x = iris,
#'      Dimensions = NULL,
#'      Measures = c("Sepal.Length", "Sepal.Width",  "Petal.Length", "Petal.Width" ),
#'      Attributes = "Species",
#'      Title = "Iris Dataset"
#' )
#'
#' my_iris_dataset <- dublincore_add(
#'      x = my_iris_dataset,
#'      Creator = person("Edgar", "Anderson", role = "aut"),
#'      Publisher = "American Iris Society",
#'      Source = "https://doi.org/10.1111/j.1469-1809.1936.tb02137.x",
#'      Date = 1935,
#'      Language = "en"
#' )
#'
#' dataset_export_csv(my_iris_dataset, file = file.path(tempdir(), "my_iris.csv"))
#'
#' \donttest{
#' read.csv(file.path(tempdir(), "my_iris.csv"), skip=20)
#' read.csv(file.path(tempdir(), "my_iris.csv"))
#' }
#' @export

dataset_export_csv <- function(ds, file) {

  if (! inherits(ds, "dataset")) {
    stop( "dataset_export_csv(ds, file): ds must be a dataset")
  }

  file_header        <- metadata_header(ds)
  real_header        <- as.data.frame(matrix(names(ds), ncol=ncol(ds)))
  names(real_header) <- names(file_header)
  add_ds             <- ds
  names(add_ds)      <- names(file_header)
  table_to_write     <- rbind(file_header, real_header, add_ds)

  write.csv(table_to_write, file, row.names = FALSE)
}

#' @keywords internal
metadata_header <- function(ds) {

  datacite_attributes  <- datacite(ds)
  datacite_attributes  <- datacite_attributes[which(names(datacite_attributes)!="names")]
  datacite_attributes$dimensions <- lapply ( datacite_attributes$dimensions, function(x)paste(x, collapse ="|") )$names
  datacite_attributes$measures   <- lapply ( datacite_attributes$measures, function(x)paste(x, collapse ="|") )$names
  datacite_attributes$attributes <- lapply ( datacite_attributes$attributes, function(x)paste(x, collapse ="|") )$names
  datacite_attributes$Date       <- as.character(datacite_attributes$Date)
  datacite_attributes$Issued     <- as.character(datacite_attributes$Issued)
  datacite_attributes$Creator    <- as.character(datacite_attributes$Creator)
  datacite_attributes$Type       <- paste0("resourceType=", unique(datacite_attributes$Type$resourceType), "|resourceTypeGeneral=", unique(datacite_attributes$Type$resourceTypeGeneral))
  datacite_attributes$Language   <- as.character(language(ds))
  as.data.frame(datacite_attributes)


  Property <- unlist(lapply (seq_along(datacite_attributes), function(x) names(datacite_attributes[x])))
  value <- unlist(lapply (seq_along(datacite_attributes), function(x) as.character(unlist(datacite_attributes[x]))))

  csv_header <- data.frame(Property = Property,
                           value = value )

  if ( ncol(ds) > 2 ) {
    cols_to_add <- ncol(ds)-2
    csv_header <- cbind(csv_header,
                        as.data.frame(matrix(rep(NA_character_, cols_to_add*nrow(csv_header)), nrow = nrow(csv_header)))
    )
  }

  empty_rows <- as.data.frame(matrix(rep(NA_character_, (19-nrow(csv_header))*ncol(csv_header)), ncol=ncol(csv_header)))
  names(empty_rows) <- names(csv_header)

  rbind(csv_header, empty_rows)
}
