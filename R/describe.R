#' @title Describe a dataset in N-Triples format
#' @description Writes provenance and Dublin Core metadata of a dataset to a
#'   file or connection in N-Triples format.
#' @param x A `dataset_df` object.
#' @param con A connection or a character string path (e.g. from `tempfile()`).
#' @return Invisibly returns `x`. Writes N-Triples to `con`.
#' @export
describe <- function(x, con) {
  assertthat::assert_that(
    is.dataset_df(x),
    msg = "describe(x, con): x must be a dataset_df object."
  )

  # Collect provenance and DC metadata
  prov_text <- provenance(x)
  dc_text <- as_dublincore(x, type = "ntriples")

  prov_text <- if (is.null(prov_text)) character(0) else as.character(prov_text)
  dc_text <- if (is.null(dc_text)) character(0) else as.character(dc_text)

  ntriples_text <- c(prov_text, dc_text)

  if (!is.character(ntriples_text)) {
    stop("describe(): expected character vector of N-Triples.")
  }

  # Handle connection or file path
  if (is.character(con)) {
    con_file <- file(con, open = "w", encoding = "UTF-8")
    on.exit(close(con_file), add = TRUE)
    writeLines(ntriples_text, con = con_file)
  } else {
    writeLines(ntriples_text, con = con)
  }

  invisible(x)
}
