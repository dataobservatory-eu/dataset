#' @keywords internal

normalize_roles <- function(people, default_role = "ctb") {
  lapply(people, function(p) {
    if (!inherits(p, "person")) return(p)
    if (is.null(p$role) || !any(nzchar(p$role))) {
      p$role <- default_role
    }
    p
  })
}
