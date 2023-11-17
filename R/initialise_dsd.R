
#' @keywords internal
initialise_dsd <- function(df, col) {
  r_class <- class(df[,col])
  col_class <- "xsd:string"
  col_class <- ifelse ("numeric" %in% r_class, "xsd:decimal", col_class)
  col_class <- ifelse ("logical" %in% r_class, "xsd:boolean", col_class)
  col_class <- ifelse ("integer" %in% r_class, "xsd:integer", col_class)
  col_class <- ifelse ("factor" %in% r_class, "coded", col_class)
  # find other labelled uses

  var <-  list ( var1 = list(name = names(df)[col],
                             label = list (""),
                             type = "",
                             range = col_class,
                             comment = "",
                             concept = list ( heading = "",
                                              schemeURI = "",
                                              valueURI = ""),
                             defintion = list ( schemeURI = "",
                                                valueURI = ""))
  )

  names(var) <- names(df)[col]
  var
}
initialize_dsd <- initialise_dsd
