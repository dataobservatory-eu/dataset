#' @title Download data into a dataset
#' @description A wrapper around \code{\link{dataset}} and
#' an import function.
#' @importFrom utils download.file
#' @inheritParams utils::download.file
#' @inheritParams dataset
#' @param type A file type.  Currently only \code{csv} is implemented that
#' invokes \code{utils::download.file}.
#' @param type A file type
#' @return A \code{\link{dataset}} with the downloaded dataset.
#' @examples
#' dataset_download(
#'     title = "Environmental Subsidies and Similar Transfers from Europe to the Rest of the World",
#'     dimensions = c("time", "geo"),
#'     measures = "value",
#'     attributes = c("unit", "obs_status", "method", "freq"),
#'     identifer = "https://doi.org/10.5281/zenodo.5813772",
#'     destfile = file.path(tempdir(), "5813772.csv")
#' )
#' @export

dataset_download  <- function(title,
                                  dimensions = NULL,
                                  measures = NULL,
                                  attributes = NULL,
                                  identifer = NULL,
                                  url,
                                  type = "csv", ...) {

  dataset_download_csv(title=title,
                       dimensions=dimensions,
                       measures=measures,
                       attributes=attributes,
                       type = "csv")
}


#' @rdname dataset_download
#' @keywords internal
dataset_download_csv  <- function(title,
                                  dimensions = NULL,
                                  measures = NULL,
                                  attributes = NULL,
                                  identifer = NULL,
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
            dimensions = dimensions, measures = measures, attributes = attributes,
            title  = title )

  dublincore_add (ds, identifier = identifer)
}

