#' @title Download data into a dataset
#' @description A wrapper around \code{\link{dataset}} and
#' an import function.
#' @importFrom utils download.file read.csv
#' @inheritParams utils::download.file
#' @inheritParams dataset
#' @inheritParams dublincore
#' @param type A file type.  Currently only \code{csv} is implemented that
#' invokes \code{utils::download.file}.
#' @return A \code{\link{dataset}} with the downloaded dataset.
#' @examples
#'
#' dest_file <- file.path(tempdir(), "5813772.csv")
#'
#' dataset_download(
#'     Title = "Environmental Subsidies and Similar Transfers from Europe to the Rest of the World",
#'     dimensions = c("time", "geo"),
#'     measures = "value",
#'     attributes = c("unit", "obs_status", "method", "freq"),
#'     identifer = "https://doi.org/10.5281/zenodo.5813772",
#'     destfile = dest_file,
#'     url = "https://doi.org/10.5281/zenodo.5813772"
#' )
#' @export

dataset_download  <- function(Title,
                              dimensions = NULL,
                              measures = NULL,
                              attributes = NULL,
                              Identifier = NULL,
                              url,
                              type = "csv", ...) {

  dataset_download_csv(Title=Title,
                       dimensions=dimensions,
                       measures=measures,
                       attributes=attributes,
                       Identifier = Identifier,
                       type = "csv")
}


#' @rdname dataset_download
#' @keywords internal
dataset_download_csv  <- function(Title,
                                  dimensions = NULL,
                                  measures = NULL,
                                  attributes = NULL,
                                  Identifier = NULL,
                                  url,
                                  type = "csv",
                                  destfile = NULL,
                                  method =  'auto',
                                  quiet = TRUE,
                                  mode = 'wb',
                                  cacheOK = TRUE) {

  if (is.null(destfile)) destfile <- tempfile()
  download.file(url = url,
                destfile = destfile,
                method = method,
                quiet = quiet,
                mode = mode,
                cacheOK = cacheOK)

  tmp <- read.csv(destfile)
  ds <- dataset ( x = tmp,
                  dimensions = dimensions,
                  measures = measures,
                  attributes = attributes,
                  Title  = Title )

  dublincore_add (ds, Identifier = Identifier)
}

