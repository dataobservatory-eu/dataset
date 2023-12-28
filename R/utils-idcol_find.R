
#' @keywords internal
idcol_find <- function(x, idcol) {

  if(!is.null(idcol)) {
    if(class(idcol)[1]=="character") {
      if(! idcol %in% names(x)) {
        stop("idcol_find(x, idcol, ...): idcol cannot be found in x.")
      }

      ## Return a unique number instead of a string
      selected_col <- which(names(x)==idcol)

      if (length(selected_col)!=1) {
        stop("idcol_find(x, idcol, ...): idcol must select a unique variable column in x.")
      }

      selected_col

    } else if (class(idcol)[1] %in% c("numeric", "integer")) {

      # Validate if the numeric idcol is in x
      if (! idcol %in% seq_along(x)) {
        stop("xsd_convert(x, idcol, ...): idcol cannot be found in x.")
      }

      # Return the validated idcol as a number
      idcol

    } else {
      # Throw an error
      stop("xsd_convert(x, idcol, ...): idcol must be a variable name or a variable position in x.")
    }
  }

}
