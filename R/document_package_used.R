
#' @keywords internal
#' @importFrom utils sessionInfo
document_package_used <- function(package_name) {


  loaded_pkgs <- utils::sessionInfo()
  my_packages <- loaded_pkgs$otherPkgs

  pkg_uri <- gsub(" ", "", paste("R_package:R_base_", loaded_pkgs$R.version$major, "_",
                                 gsub("\\.", "_", loaded_pkgs$R.version$minor), "_on_platform_",
                                 loaded_pkgs$R.version$platform))

  pkg_nr <- which(package_name == names(loaded_pkgs$loadedOnly))

  if (length(pkg_nr)==0) {
    pkg_version <- ""
  }  else {
    pkg_uri <- loaded_pkgs$loadedOnly[[pkg_nr]]$Version
    pkg_uri <- paste0("R_package:", package_name, "_", pkg_uri)
  }

  pkg_uri
}
