#' @title Structure a data frame to dataset
#' @details Loosely follows the \href{https://www.w3.org/TR/vocab-data-cube/}{The RDF Data Cube Vocabulary},
#' but with far stricter definition.
#' @param x A data.frame or interited tibble, data.frame, or a structured list.
#' @param obs_id The observation identifier.
#' @param dim_names The columns in the data frame that are dimensions, and maybe used
#' for statistical aggregation.
#' @param measure_names The measurements in the data frame.
#' @param attribute_names Not measured attributes of the observations
#' @param unit The unit of the measurements
#' @param Title The title of the dataset, corresponds to datacite:Title, dct:title, rdfs:label.
#' @param Subject Corresponds to dct:subject, datacite:Subject.
#' @param Publisher Corresponds to dct:Publisher and datacite:Publisher.
#' @param Licence Corresponds to dct:License and datacite:License.
#' @importFrom assertthat assert_that
#' @family metadata functions
#' @examples
#' dataset (x = iris,
#'          title = "Iris Dataset",
#'          dataset_id = "iris_dataset", obs_id = NULL,
#'          dimensions = NULL,
#'          measurements = c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width"),
#'          attributes = c("Species"),
#'          unit = list(column="", code="MM", label = "milimeters")
#'           )
#' @export

dataset <- function(x,
                    dataset_id = "dataset_id",
                    obs_id = NULL,
                    dimensions = NULL,
                    measurements = NULL,
                    attributes = NULL,
                    unit = NULL,
                    Title = NULL,
                    Creator = NULL,
                    PublicationYear = NULL,
                    Publisher = NULL,
                    Subject = NULL,
                    License = NULL) {

  assertthat::assert_that(is.datacube(x),
                          msg = "in dataset(x, ...): x must be a datacube object.")


  arguments <- list ( dataset_id = dataset_id,
                      obs_id = obs_id, dimension=dimensions, measurements = measurements, attributes = attributes,
                      Title = Title, Subject = Subject, Publisher = Publisher, License = License )
  arguments_chr <- paste(paste0(names(arguments), "=", arguments), collapse = ", ")


  created_time <- Sys.time()
  dataset <- x
  attr(dataset, "class") <- c( "dataset", class(x))
  attr(dataset, "Title") <- Title
  attr(dataset, "Date") <- add_date ( NULL, time = created_time, dateType = "Created", dateInformation = paste0("dataset::dataset(",arguments_chr, ")"))

    assertthat::assert_that(is.datacube(x),
                          msg = "in dataset(x, ...): x must be a datacube object.")

  if(is.null(Creator)) {
    Creator <- person(given="unknown creator")
  } else {
    assertthat::assert_that(inherits(Creator, "person"),
                            msg = "in dataset(x, ..., Creator): Creator must be a person object.")
  }

  if(is.null(Title)) {
    Title <- "Untitled Dataset"
  } else {
    assertthat::assert_that(inherits(Title, "character"),
                            msg = "in dataset(x, ..., Title): Title must be a character string.")
  }

  if(is.null(Publisher)) {
    Publisher <- "<not yet published>"
  } else {
    assertthat::assert_that(inherits(Publisher, "character"),
                            msg = "in dataset(x, ..., Publisher): Publisher must be a character string.")
  }

  if(is.null(PublicationYear)) {
    PublicationYear <- as.integer(substr(as.character(Sys.Date()),1,4))
  } else {
    assertthat::assert_that(is.numeric(round(as.numeric(as.character(PublicationYear)),0)),
                            msg = "in dataset(x, ..., PublicationYear): PublicationYear must be an integer.")
  }

  ## DataCite mandatory
  attr(dataset, "Identifier") <- deparse(substitute(x))
  attr(dataset, "Creator") <- Creator
  attr(dataset, "Title") <- as.character(Title)
  attr(dataset, "Publisher") <- as.character(Publisher)
  attr(dataset, "PublicationYear") <- as.integer(round(as.numeric(as.character(PublicationYear)),0))
  attr(dataset, "ResourceType") <- "Dataset"

  new_date <- add_date ( NULL, time = created_time,
                         dateType = "Created",
                         dateInformation = paste0("call: dataset(...)"))
  attr(dataset, "Date") <-  new_date

  attr(dataset, "unit") <- unit
  attr(dataset, "Subject") <- Subject

  attr(dataset, "License") <- License

  dataset
}


#' @rdname dataset
#' @export
is.dataset <- function(x) inherits(x, "dataset")

print.dataset <- function(x, ...) {

  cat(paste0(Title, " [", attr(x, "Identifier"), "] by ", paste(attr(x, "Creator")$given, attr(x, "Creator")$family, sep = " "), "\n"))
  cat(paste0("Published by ", attr(x, "Publisher"), " (", attr(x, "PublicationYear"), ")", "\n"))

  n_row <- nrow(as.data.frame(x))

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

  #further_attributes <- ifelse (is.null(attr(x, "Subject")),
  #                              "", paste0("Subject: ", attr(x, "Subject")) )

  #cat (further_attributes)

}

summary.dataset <- function (x, ...) {
  summary(as.data.frame(x))
  #UseMethod("summary.dataset")
  }
#summary.dataset.datacube <- function(x, ...) { x <- as.data.frame(x) NextMethod()}

