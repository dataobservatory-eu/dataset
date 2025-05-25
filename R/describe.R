#' @title Describe a dataset
#' @param x A dataset_df object.
#' @param con A connection, for example, \code{con=tempfile()}.
#' @return The description of the dataset_df object is written to the
#' connection in the N-Triples form. If \code{con=NULL}, then the serialisation
#' takes place in \code{tempfile()} and the contents are printed to the console;
#' if a file is given, than no output is returned.
#' @examples
#'
#' # See the serialisation on the screen:
#' describe(orange_df)
#'
#' # Save it to a connection:
#' temporary_connection <- tempfile()
#' describe(orange_df, con = temporary_connection)
#' @export

describe <- function(x, con=NULL) {

  if(!inherits(x, "dataset_df")) {
    stop("Error: describe(x, con) - x most be a a dataset_df object.")
  }

  ntriples_text <- provenance(x)
  ntriples_text <- c(ntriples_text, as_dublincore(x, "ntriples"))

  if (is.null(con)) {
    no_connection <- TRUE
    con <- tempfile()
  } else {
      no_connection <- FALSE
    }

  writeLines(ntriples_text, con = con)

  if (no_connection) {
    writeLines(ntriples_text, con = con)
    readLines(con = con)
  } else {
    writeLines(ntriples_text, con = con)
  }
}
