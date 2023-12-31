#' @title Dataset to triples (three columns)
#' @description The dataset is converted into a three-column long format with
#' columns \code{s} for subject, \code{p} for predicate and \code{o} for
#' object.
#' @param df A [data.frame] or similar object, or a [dataset].
#' @param idcol The identifier column. If idcol is \code{NULL} it attempts to
#' use the row.names(df) as an idcol.
#' @return The long form version of the original dataset, retaining the attributes
#' and class.
#' @export

dataset_to_triples <- function(df, idcol=NULL) {

  is_dataset <- inherits(df, "dataset")

  if (is_dataset) {
    new_title = paste0(dataset_title(df), " [triple form]")
    DataBibentry <- dataset_bibentry(df)
    new_Subject <- subject(df)
  }

  if (is.null(idcol)) {
    df$new_id_col <- row.names(df)
    idcol <- which(names(df)=="new_id_col" )
    idcol_pos <- dataset:::idcol_find(df, idcol)
    seq_along_cols <- seq_along(df)[-idcol_pos]
  } else {
    ## See utils-idcol_find.R for the internal function
    seq_along_cols <- seq_along(df)[-idcol_find(df, idcol)]
  }

  triple_list <- lapply ( seq_along_cols, function(x) {
    data.frame(s = df[[idcol]],
               p = names(df)[x],
               o = df[[x]]
    ) })

  tmp <- do.call(rbind, triple_list)

  if (is_dataset) {
    tmp2 <- dataset(x=tmp, author=creator(df), title = new_title)
    tmp_DSD <- DataStructure(tmp2)
    tmp_DSD$s$label <- "Subject"
    tmp_DSD$s$label <- "Object"
    tmp_DSD$p$label <- "Predicate"
    DataStructure_update(tmp2, tmp_DSD)
  }
  tmp
}

