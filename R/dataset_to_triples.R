#' @title Dataset to triples (three columns)
#' @description The dataset is converted into a three-column long format with
#' columns \code{s} for subject, \code{p} for predicate and \code{o} for
#' object.
#' @param x An R object that contains the data of the dataset (a data.frame or
#' inherited from [`data.frame`][base::data.frame()]), for example, [dataset()]
#' [tibble::tibble()], [tsibble::tsibble()], [data.table::data.table()].
#' @param idcol The identifier column. If \code{idcol} is \code{NULL} it attempts to
#' use the \code{row.names(df)} as an \code{idcol}.
#' @return The long form version of the original dataset, retaining the attributes
#' and class.
#' @export

dataset_to_triples <- function(x, idcol=NULL) {

  is_dataset <- inherits(x, "dataset")

  if (is_dataset) {
    new_title = paste0(dataset_title(x), " [triple form]")
    DataBibentry <- dataset_bibentry(x)
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
    data.frame(s = x[[idcol]],
               p = names(x)[i],
               o = x[[i]])
    })

  tmp <- do.call(rbind, triple_list)

  if (is_dataset) {
    tmp2 <- dataset(x=tmp, author=creator(x), title = new_title)
    tmp_DSD <- DataStructure(tmp2)
    tmp_DSD$s$label <- "Subject"
    tmp_DSD$s$label <- "Object"
    tmp_DSD$p$label <- "Predicate"
    DataStructure_update(tmp2, tmp_DSD)
  }
  tmp
}

