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
#' \dontrun{
#' dataset_download(
#'     Title = "Environmental Subsidies and Similar Transfers from Europe to the Rest of the World",
#'     Dimensions = c("time", "geo"),
#'     Measures = "value",
#'     Attributes = c("unit", "obs_status", "method", "freq"),
#'     Identifer = "https://doi.org/10.5281/zenodo.5813772",
#'     destfile = dest_file,
#'     url = "https://doi.org/10.5281/zenodo.5813772"
#' )
#' }
#' @export

dataset_download  <- function(Title,
                              Dimensions = NULL,
                              Measures = NULL,
                              Attributes = NULL,
                              Identifier = NULL,
                              url,
                              type = "csv", ...) {

  dataset_download_csv(url = url,
                       Title = Title,
                       Dimensions = Dimensions,
                       Measures = Measures,
                       Attributes = Attributes,
                       Identifier = Identifier,
                       type = "csv")
}


#' @rdname dataset_download
#' @keywords internal
dataset_download_csv  <- function(url,
                                  Title,
                                  Dimensions = NULL,
                                  Measures = NULL,
                                  Attributes = NULL,
                                  Identifier = NULL,
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

  if(!all(Dimensions %in% names(tmp))) {
    stop("Not all Dimensions are present in ", destfile)
  }

  if(!all(Measures %in% names(tmp))) {
    stop("Not all Measures are present in ", destfile)
  }

  if(!all(Attributes %in% names(tmp))) {
    stop("Not all Attributes are present in ", destfile)
  }

  ds <- dataset ( x = tmp,
                  Dimensions = Dimensions,
                  Measures = Measures,
                  Attributes = Attributes,
                  Title  = Title )

  ds <- dublincore_add (ds,
                        Identifier = Identifier,
                        Source = url)

  ds
}

