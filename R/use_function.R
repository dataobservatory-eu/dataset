#' @title Use a function on a dataset
#' @param dataset A dataset object.
#' @param .f A function (from an installed package)
#' @return A dataset with a recorded Time and RelatedItem attribute.
#' @export
#' @examples
#' my_dataset <- dataset (x = data.frame ( geo = c("NL", "NL", "BE", "BE"),
#'                        values = c(1:4)),
#'                        dataset_id = "reprex",
#'                        obs_id = NULL,
#'                        dimensions = "geo",
#'                        measurements = "values",
#'                        attributes = NULL,
#'                        Title =  "Sample Dataset")
#'
#'  attributes(use_function(my_dataset, .f="summary"))

use_function <- function(dataset, .f = "base::subset", ... ) {

  arguments <- list(...)

  loaded_pkgs <- utils::sessionInfo()
  my_packages <- loaded_pkgs$otherPkgs

  pkg_uri <- gsub(" ", "", paste("rpackage_base_R_", loaded_pkgs$R.version$major, "_",
                                 gsub("\\.", "_", loaded_pkgs$R.version$minor), "_on_platform_",
                                 loaded_pkgs$R.version$platform))
  if ( grepl("::", .f)) {
    f_name <- strsplit(.f, "::")
    if ( length(f_name[[1]]) == 2) {
      package_name <- f_name[[1]][1]
      function_name <- f_name[[1]][2]

      pkg_nr <- which(package_name == names(loaded_pkgs$loadedOnly))

      if (length(pkg_nr)==0) {
        pkg_version <- ""
      }  else {
        pkg_uri <- loaded_pkgs$loadedOnly[[pkg_nr]]$Version
        pkg_uri <- paste0(" (rpackage_", pkg_name, "_", pkg_uri, ")")
      }

    } else {
      package_name = "base"
      function_name = f_fname[[1]][1]
    }
  } else { package_name = "base"}

  return_dataset <- eval(parse(text = .f))(dataset, ...)

  if (is.null(return_dataset)) stop("Error")

  attr(return_dataset, "Date") <- add_date (attr(return_dataset, "Date"), Sys.time(), "Updated", dateInformation = paste0("call:" ,.f))

  new_item <-  add_relitem( RelatedIdentifier = attr(return_dataset, "RelatedIdentifier"),
                            package_name, "IsCompiledBy")

  attr(return_dataset, "RelatedIdentifier") <- new_item

  if ( ! "dataset" %in% class(return_dataset)) {
    class(return_dataset) <- c(class(return_dataset), "dataset")
  }

  return_dataset
}

#' @title Read a file into a dataset
#' @param .f A function (from an installed package)
#' @param ... Parameters to read the file.
#' @return A dataset with a recorded Time and RelatedItem attribute.
#' @inheritParams dataset
#' @inheritParams use_function
#' @export
#' @examples
#' temp_path <- file.path(tempdir(), "iris.csv")
#' write.csv(iris, file = temp_path, row.names = F)
#' iris_ds <- read_dataset(
#'   dataset_id = "iris_dataset",
#'   obs_id = NULL,
#'   dimensions = NULL,
#'   measurements = c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width"),
#'   attributes = "Species",
#'   Title = "Iris Dataset",
#'   unit = list(code = "MM", label = "millimeter"),
#'   .f = "utils::read.csv", temp_path )
#' attributes(iris_ds)

read_dataset <- function(dataset_id,
                         obs_id = NULL, Title, dimensions, measurements, attributes,
                         unit,
                         .f= "utils::read.csv", ...) {

  named_arguments = list (dataset_id, obs_id = obs_id, dimensions = dimensions,
                          measurements = measurements, attributes = attributes, Title = Title)
  arguments <- list(...)

  loaded_pkgs <- utils::sessionInfo()
  my_packages <- loaded_pkgs$otherPkgs

  pkg_uri <- gsub(" ", "", paste("rpackage_base_R_", loaded_pkgs$R.version$major, "_",
                                 gsub("\\.", "_", loaded_pkgs$R.version$minor), "_on_platform_",
                                 loaded_pkgs$R.version$platform))
  if ( grepl("::", .f)) {
    f_name <- strsplit(.f, "::")
    if ( length(f_name[[1]]) == 2) {
      package_name <- f_name[[1]][1]
      function_name <- f_name[[1]][2]

      pkg_nr <- which(package_name == names(loaded_pkgs$loadedOnly))

      if (length(pkg_nr)==0) {
        pkg_version <- ""
      }  else {
        pkg_uri <- loaded_pkgs$loadedOnly[[pkg_nr]]$Version
        pkg_uri <- paste0(" (rpackage_", pkg_name, "_", pkg_uri, ")")
      }

    } else {
      package_name = "base"
      function_name = f_fname[[1]][1]
    }
  } else { package_name = "base"}


  x <- eval(parse(text = .f))(...)
  ## x <- eval(parse(text = .f))(file)
  read_time <- Sys.time()
  tmp <- dataset (x = x,
                  dataset_id = "dataset_id", obs_id = NULL,
                  dimensions = dimensions,
                  measurements = measurements,
                  attributes = attributes,
                  unit = unit,
                  Title = Title)

  assertthat::assert_that(inherits(tmp, "dataset"))

  if ( is.null(arguments)  | length(arguments)==0) {
    argument_chr = ""
  } else {
    argument_chr <- paste(paste0(names(arguments), "=", arguments), collapse = ", ")
  }

  new_date <- add_date ( NULL, time = read_time, dateType = "Created", dateInformation = paste0("call:", .f, "(", argument_chr, ")", pkg_uri))
  attr(tmp, "Date") <-  new_date

  new_item <-  add_relitem( NULL, package_name, "IsCompiledBy")
  attr(tmp, "RelatedIdentifier") <- new_item
  tmp
}





