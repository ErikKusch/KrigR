### FILE EXISTENCE CHECKING ====================================================
#' Checking if a file already exists
#'
#' If a file already exists in a given place, load that file
#'
#' @param FName File name
#' @param Dir Directory where to look for file
#' @param loadFun function with which to load filetype of FName
#' @param load Logical. Whether to load the data or not
#' @param verbose Logical. Whether to print/message function progress in console or not.
#'
#' @return Either a data object or NULL
#'
#' @examples
#' KrigR::Check.File(
#'           FName = basename(system.file("extdata", "CentralNorway.nc", package="KrigR")),
#'           Dir = dirname(system.file("extdata", "CentralNorway.nc", package="KrigR")),
#'           loadFun = terra::rast
#'           )
#' @export
Check.File <- function(FName, Dir = getwd(), loadFun, load = TRUE, verbose = TRUE){
  FNAME <- file.path(Dir, FName)
  file <- NULL
  if(file.exists(FNAME)){
    if(verbose){print(paste0("A file with the name ", FName, " already exists in ", Dir,
                             ". Loading this file for you from the disk."))}
    if(load){
      file <- sapply(FNAME, loadFun)
    }else{
      file <- "Present. Not Loaded."
      }
  }
  return(file)
}
