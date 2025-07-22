#' @keywords internal
fix_contributor <- function(contributors=NULL) {

  if (is.null(contributors)) return(":unas")

  if ( all(inherits(contributors, "person")) ) {
    if( length(contributors)>1 ) {
      return_value <- paste0("{",
                             paste( vapply(contributors, function(x) {as.character(x)}, character(1)),
                                    collapse="} and {"),
                             "}" )
    } else {
      return_value <- as.character(contributors)
    }
  } else if ( all(inherits(contributors, "list")) ) {
    if( length(contributors)>1 ) {
      return_value <- paste0("{",
                             paste( lapply(contributors, function(x) x$given),
                                    collapse="} and {"),
                             "}" )
    } else {
      return_value <- paste(unlist(contributors[[1]]), collapse=" ")
    }
  } else if (length(contributors)>1) {
    # several character strings
    return_value <- paste0("{",
                           paste( vapply(contributors, function(x) x$given, character(1)),
                                  collapse="} and {"),
                           "}" )
  }  else {
    return_value <- contributors
  }

  assertthat::assert_that(is.character(return_value),
                          msg="Error: fix_contributor(contributors): not character but")
  assertthat::assert_that(length(return_value)==1, msg="Error: fix_contributor(contributors): not 1" )

  return_value <- gsub("* dtm", " [dtm]", return_value )
  return_value
}
