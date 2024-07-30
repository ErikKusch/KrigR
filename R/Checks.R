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
                             "."))}
    if(load){
      if(verbose){print("Loading this file for you from the disk.")}
      file <- sapply(FNAME, loadFun)[[1]]
    }else{
      file <- "Present. Not Loaded."
      }
  }
  return(file)
}

### KRIGING SANITY CHECK =======================================================
#' Sanity checks before Kriging commences
#'
#' This function is called upon in the krigR function and performs sanity checks for some of the most common error sources in krigin thereby attempting to return more sensible error messages to the user than what is returned by default.
#'
#' @param Data A SpatRaster object containing the data to be kriged.
#' @param CovariatesCoarse A SpatRaster object containing covariates for kriging at training resolution.
#' @param CovariatesFine A SPatRaster object containing covariates for kriging at target resolution.
#' @param KrigingEquation A formula object obtained from a character vector via as.formula() specifying the covariates to be used in kriging. The covariates used have to be present and named as layers in CovariatesCoarse and CovariatesFine.
#'
#' @importFrom terra res
#' @importFrom terra ext
#' @importFrom terra nlyr
#' @importFrom terra values
#' @importFrom terra varnames
#' @importFrom base colSums
#'
#' @return A list containing a potentially altered KrigingEquation if needed as well as an identifier for data layers which need to be skipped when kriging due to a variety of reasons.
#'
Check.Krig <- function(Data, CovariatesCoarse, CovariatesFine, KrigingEquation){
  ## Resolutions ===============
  if(terra::res(CovariatesFine)[1] < terra::res(Data)[1]/10){
    warning("It is not recommended to use kriging for statistical downscaling of more than one order of magnitude. You are currently attempting this. Kriging will proceed.")
  }
  if(all.equal(terra::res(CovariatesCoarse)[1], terra::res(Data)[1]) != TRUE){
    stop(paste0("The resolution of your data (", terra::res(Data)[1], ") does not match the resolution of your covariate data (", terra::res(CovariatesCoarse)[1], ") used for training the kriging model. Kriging can't be performed!" ))
  }
  ## Extent ===============
  if(terra::ext(Data) == terra::ext(-180, 180, -90, 90)){
    stop("You are attempting to use kriging at a global extent. For reasons of computational expense and identity of relationships between covariates and variables not being homogenous across the globe, this is not recommended. Instead, try kriging of latitude bands if global kriging is really your goal.")
  }
  if(!all.equal(terra::ext(CovariatesCoarse), terra::ext(Data))){
    stop("The extents of your data and training covariates don't match. Kriging can't be performed!")
  }

  ## Data Availability ===============
  DataSkips <- NULL # data layers without enough data to be skipped in kriging
  Data_vals <- base::colSums(matrix(!is.na(terra::values(Data)), ncol = terra::nlyr(Data))) # a value of 0 indicates a layer only made of NAs
  if(length(which(Data_vals < 2)) > 0){
    if(length(which(Data_vals < 2)) != terra::nlyr(Data)){
      stop(paste0("Layer(s) ", paste(which(Data_vals == 0), collapse=", "), " of your data do(es) not contain enough data. Kriging cannot be performed. Usually, increasing the extent of kriging can fix this issue."))
      DataSkips <- which(Data_vals < 2)
    }else{
      stop("Your Data does not contain enough values. Kriging cannot be performed. Usually, increasing the extent of kriging can fix this issue.")
    }
  }
  CovCo_vals <- base::colSums(matrix(!is.na(terra::values(CovariatesCoarse)), ncol = terra::nlyr(CovariatesCoarse))) # a value of 0 indicates a layer only made of NAs
  if(length(which(CovCo_vals < 2)) > 0){
    if(length(which(CovCo_vals < 2)) != terra::nlyr(CovariatesCoarse)){
      warning(paste0("Layer(s) ", paste(which(CovCo_vals < 2), collapse=", "), " of your covariates at training resolution do(es) not contain enough data. This/these layer(s) is/are dropped. The Kriging equation might get altered."))
      CovariatesCoarse <- CovariatesCoarse[[-which(CovCo_vals < 2)]]
    }else{
      stop("Your covariate data at training resolution does not contain enough values. Kriging can't be performed!")
    }
  }
  CovFin_vals <- base::colSums(matrix(!is.na(terra::values(CovariatesFine)), ncol = terra::nlyr(CovariatesFine))) # a value of 0 indicates a layer only made of NAs
  if(length(which(CovFin_vals < 2)) > 0){
    if(length(which(CovFin_vals < 2)) != terra::nlyr(CovariatesFine)){
      warning(paste0("Layer(s) ", paste(which(CovFin_vals == 0), collapse=", "), " of your covariates at target resolution do(es) not contain enough data. This/these layer(s) is/are dropped."))
      CovariatesFine <- CovariatesFine[[-which(CovFin_vals < 2)]]
    }else{
      stop("Your covariate data at target resolution does not contain enough values. Kriging can't be performed!")
    }
  }
  ## Kriging Equation ===============
  Terms <- unlist(strsplit(labels(terms(KrigingEquation)), split = ":")) # identify parameters called to in formula
  Terms_Required <- unique(Terms) # isolate double-references (e.g. due to ":" indexing for interactions)
  Terms_Present <- Reduce(intersect, list(Terms_Required, terra::varnames(CovariatesCoarse), terra::varnames(CovariatesFine))) # identify the terms that are available and required
  if(sum(Terms_Required %in% Terms_Present) != length(Terms_Required)){
    if(length(Terms_Present) == 0){ # if none of the specified terms were found
      KrigingEquation <- paste0("Data ~ ", paste(terra::varnames(CovariatesCoarse), collapse = "+"))
      warn <- paste("None of the terms specified in your KrigingEquation are present in the covariate data sets. The KrigingEquation has been altered to include all available terms in a linear model:", "\n\n", KrigingEquation)
    }else{ # at least some of the specified terms were found
      KrigingEquation <- paste0("Data ~ ", paste(Terms_Present, collapse = "+"))
      warn <- paste("Not all of the terms specified in your KrigingEquation are present in the covariate data sets. The KrigingEquation has been altered to include all available and specified terms in a linear model:", "\n\n", KrigingEquation)
    }
    Cotinue <- menu(c("Yes", "No"), title=paste0(warn, ". \n\nDo you wish to continue using the new formula?"))
    if(Cotinue == 2){ # break operation if user doesn't want this
      stop("Kriging terminated by user due to formula issues.")
    }
  }
  ## Return data ===============
  return(list(as.formula(KrigingEquation), DataSkips))
}
