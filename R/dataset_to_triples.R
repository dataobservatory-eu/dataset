#' @title Dataset to triples (three columns)
#' @description The dataset is converted into a three-column long format with
#' columns \code{s} for subject, \code{p} for predicate and \code{o} for
#' object.
#' @param x An R object that contains the data of the dataset (a data.frame or
#' inherited from [`data.frame`][base::data.frame()]), for example, [dataset_df()]
#' [tibble::tibble()], [tsibble::tsibble()], [data.table::data.table()].
#' @param idcol The identifier column. If \code{idcol} is \code{NULL} it attempts to
#' use the \code{row.names(df)} as an \code{idcol}.
#' @return The long form version of the original dataset, retaining the attributes
#' and class.
#' @export

dataset_to_triples <- function(x, idcol=NULL) {

  is_dataset <- inherits(x, "dataset_df")

  if (is_dataset) {
    new_title = paste0(dataset_title(x), " [triple form]")
    DataBibentry <- get_bibentry(x)
    new_Subject <- subject(x)
  }

  if (is.null(idcol)) {
    x$new_id_col <- row.names(x)
    idcol <- which(names(x)=="new_id_col" )
    idcol_pos <- idcol_find(x, idcol)
    seq_along_cols <- seq_along(x)[-idcol_pos]
  } else {
    ## See utils-idcol_find.R for the internal function
    seq_along_cols <- seq_along(x)[-idcol_find(x, idcol)]
  }

  triple_list <- lapply (seq_along_cols, function(i) {
    data.frame(s = as.character(x[[idcol]]),
               p = names(x)[i],
               o = as.character(x[[i]]))
    })

  tmp <- do.call(rbind, triple_list)

  tmp

}

