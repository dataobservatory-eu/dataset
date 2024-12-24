#' @title Describe a dataset
#' @param x A dataset_df object.
#' @param con A connection, for example, \code{con=tempfile()}.
#' @return The description of the dataset_df object is written to the connection
#' in the n-triples form, nothing is returned.
#' @examples
#' temp_prov <- tempfile()
#' describe(iris_dataset, con=temp_prov)
#' readLines(temp_prov)
#' @export

describe <- function(x,con) {
  assertthat::assert_that(is.dataset_df(x),
                          msg="Error: describe(x, con) - x most be a a dataset_df object.")
  ntriples_text <- provenance(x)
  ntriples_text <- c(ntriples_text, as_dublincore(x, "ntriples"))
  writeLines(ntriples_text, con=con)
}
