#' @rdname dataset
describe <- function(x) {
  UseMethod("describe", x)
}

#' @rdname dataset
#' @exportS3Method
describe.dataset <- function(x){

  DataBibentry <- attr(x, "DataBibentry")

  title_row <- if("version" %in% names(DataBibentry[[1]])) {
    paste0(cat(DataBibentry[[1]]$title
               , " [ version:",
               DataBibentry[[1]]$version, "]\n"))
  } else {
    cat(DataBibentry[[1]]$title, "\n")
  }


  col_num <- dim(x)[2]
  row_num <- dim(x)[1]
  row_num <- ifelse (row_num>1, paste0(row_num, " observations (rows)"), paste0(row_num, " observation (row)"))
  col_num <- ifelse (col_num>1, paste0(col_num, " variables (columns)"), paste0(col_num, " variable (column)"))
  cat(paste0(DataBibentry[[1]]$resourceType, " with ", row_num, " and ", col_num, ".\n"  ))
  if(! is.null(DataBibentry[[1]]$description) ) {
    cat(paste0("Description: ", DataBibentry[[1]]$description, "\n"))
  }

  if(! is.null(DataBibentry[[1]]$author) ) {
    cat(paste0("Creator: ", as.character(DataBibentry[[1]]$author), "\n"))
  }


  if(! is.null(DataBibentry[[1]]$publisher) ) {
    cat(paste0("Publisher: ", as.character(DataBibentry[[1]]$publisher), "\n"))
  }

  #if(!is.null(attr(x, "Subject"))) {
  #  cat(paste0("Subject: ", names(attr(x, "Subject")), "\n"))
  #}


}
