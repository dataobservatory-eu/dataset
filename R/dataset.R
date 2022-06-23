#' @title Structure a data frame to dataset
#' @details Loosely follows the \href{https://www.w3.org/TR/vocab-data-cube/}{The RDF Data Cube Vocabulary},
#' but with far stricter definition.
#' @param x A data.frame or interited tibble, data.frame, or a structured list.
#' @param obs_id The observation identifier.
#' @param dimensions The columns in the data frame that are dimensions, and maybe used
#' for statistical aggregation.
#' @param measurements The measurements in the data frame.
#' @param attributes Not measured attributes of the observations
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
                    unit = NA_character_,
                    Title = NULL,
                    Subject = NULL,
                    Publisher = NULL,
                    License = NULL) {

  if (is.null(obs_id)) {
    obs_id <- "obs_id"
    x$obs_id <- row.names(x)
  }


  dataset <- subset(x, select = obs_id)

  if (!is.null(dimensions)) {
    assertthat::assert_that(all(dimensions %in% names(x)),
                            msg = "Not all dimenstions are present in x")

    dataset <- cbind(dataset, subset(x, select = dimensions))
  } else { dimensions = ""}

  if (!is.null(measurements)) {
    assertthat::assert_that(all(measurements %in% names(x)),
                            msg = "Not all measurements are present in x")

    dataset <- cbind(dataset,subset(x, select = measurements))
  } else { measurements = ""}

  if (!is.null(attributes)) {
    assertthat::assert_that(all(attributes %in% names(x)),
                            msg = "Not all attributes are present in x")

    dataset <- cbind(dataset, subset(x, select = attributes))
  } else { attributes = ""}

  attr(dataset, "class") <- c(class(dataset), "dataset")
  attr(dataset, "Modified") <- Sys.time()

  attr(dataset, "dataset_id") <- dataset_id
  attr(dataset, "obs_id") <- "obs_id"
  attr(dataset, "unit") <- unit
  attr(dataset, "dimensions") <- dimensions
  attr(dataset, "measurements") <- measurements
  attr(dataset, "attributes") <- attributes
  attr(dataset, "Subject") <- Subject
  attr(dataset, "Publisher") <- Publisher
  attr(dataset, "License") <- License

  dataset
}

