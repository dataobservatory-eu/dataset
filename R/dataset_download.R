#' @title Download data into a dataset
#' @description A wrapper around \code{\link{dataset}} and
#' an import function.
#' @importFrom utils download.file read.csv
#' @param ... Various parameters to pass on to the downloading functions.
#' @inheritParams dataset
#' @param type A file type.  Currently only \code{csv} is implemented that
#'  \code{\link[utils:download.file]{utils::download.file}}.
#' @return A \code{\link{dataset}} with the downloaded dataset.
#' @examples
#' \donttest{
#' dest_file <- file.path(tempdir(), "iris.csv")
#' dataset_download(
#'    url = "https://zenodo.org/record/7421899/files/iris.csv?download=1",
#'    title = "Iris Dataset",
#'    author = person(given="Edgar", family="Anderson"),
#'    publisher = "American Iris Society",
#'    identifier = "https://doi.org/10.1111/j.1469-1809.1936.tb02137.x",
#'    destfile = dest_file)
#' }
#' @export
dataset_download  <- function(url,
                              title,
                              author,
                              destfile,
                              type = "csv", ...) {

  if (type == "csv") {
    dataset_download_csv(url = url,
                         title = title,
                         author = author,
                         destfile = destfile,
                         ...)
  } else {
    stop ('Currently only dataset_download(..., type = "csv" is implemented')
  }
}


#' @rdname dataset_download
#' @inheritParams utils::download.file
#' @keywords internal
dataset_download_csv  <- function(url,
                                  title,
                                  author,
                                  destfile = NULL,
                                  ...) {

  if (is.null(destfile)) destfile <- tempfile()

  download.file(url = url, destfile=destfile, ... = ...)

  tmp <- read.csv(destfile)

  ds <- dataset ( x = tmp,
                  title = title,
                  author = author,
                  ...)
  ds
}
