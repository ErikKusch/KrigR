.onLoad <- function(libname, pkgname) {
  op <- options()
  op.devtools <- list(
    devtools.path = "~/R-dev",
    devtools.install.args = "",
    devtools.name = "Erik Kusch",
    devtools.desc.author = "Kusch, Erik <erik@i-solution.de>",
    devtools.desc.suggests = NULL,
    devtools.desc = list()
  )
  toset <- !(names(op.devtools) %in% names(op))
  if(any(toset)) options(op.devtools[toset])

  invisible()
}

#' List of available variables
#'
#' This function presents the user with a selection of biologically relevant variables of the Era5-family which can be statistically downscaled. Notice that more variables are available and can be found here: \url{https://cds.climate.copernicus.eu/cdsapp#!/dataset/reanalysis-era5-land?tab=form} for Era5-Land data and here: \url{https://cds.climate.copernicus.eu/cdsapp#!/dataset/reanalysis-era5-single-levels?tab=form} for Era5 data. Notice that variable names for download requests are written in all lower-case letters and substitute spaces for underscores.
#'
#' @param DataSet Which ERA5 data set to download data from. 'era5' or 'era5-land'.
#' @examples
#' Variable_List(DataSet = "era5")
#'
Variable_List <- function(DataSet) {
  if(DataSet == "era5"){
    Variables <- data.frame(Clear = c("Wind speed at a height of 10m above the surface in line of constant latitude [m / s]",
                                      "Wind speed at a height of 10m above the surface in line of constant longitude [m / s]",
                                      "Wind speed at a height of 10m above the surface [m / s]",
                                      "Strength of wind gusts at a height of 10 m above the surface [m / s]",
                                      "Dewpoint temperature at a height of 2m above the surface [K]",
                                      "Dewpoint temperature at a height of 2m above the surface [K]",
                                      "Skin temperature of the surface [K]",
                                      "Total precipitation [m]",
                                      "Average evaporation []",
                                      "Total runoff []",
                                      "Amount of runoff at the surface []",
                                      "Amount of runoff underneath the surface []",
                                      "Height of lowest clouds [m]",
                                      "Fraction of cloud cover at high altitude []",
                                      "Fraction of cloud cover at mid-altitude []",
                                      "Fraction of cloud cover at low altitude []",
                                      "Fraction of cloud cover at any altitude []",
                                      "Amount of incoming solar radiation [W /m^2]",
                                      "Amount of incoming UV radiation [W /m^2]",
                                      "Soil temperature averaged over a depth of 0-7cm [K]",
                                      "Soil temperature averaged over a depth of 7-28cm [K]",
                                      "Soil temperature averaged over a depth of 28-100cm [K]",
                                      "Soil temperature averaged over a depth of 100-200m [K]",
                                      "Type of soil used in land model",
                                      "Soil moisture content averaged over a depth of 0-7cm [kg / kg]",
                                      "Soil moisture content averaged over a depth of 7-28cm [kg / kg]",
                                      "Soil moisture content averaged over a depth of 28-100cm [kg / kg]",
                                      "Soil moisture content averaged over a depth of 100-200cm [kg / kg]"),
                            Download = c("10m_u_component_of_wind",
                                         "10m_v_component_of_wind",
                                         "10m_wind_speed",
                                         "instantaneous_10m_wind_gust",
                                         "2m_dewpoint_temperature",
                                         "2m_temperature",
                                         "skin_temperature",
                                         "total_precipitation",
                                         "evaporation",
                                         "runoff",
                                         "surface_runoff",
                                         "sub_surface_runoff",
                                         "cloud_base_height",
                                         "high_cloud_cover",
                                         "medium_cloud_cover",
                                         "low_cloud_cover",
                                         "total_cloud_cover",
                                         "mean_surface_direct_short_wave_radiation_flux",
                                         "mean_surface_downward_uv_radiation_flux",
                                         "soil_temperature_level_1",
                                         "soil_temperature_level_2",
                                         "soil_temperature_level_3",
                                         "soil_temperature_level_4",
                                         "soil_type",
                                         "volumetric_soil_water_layer_1",
                                         "volumetric_soil_water_layer_2",
                                         "volumetric_soil_water_layer_3",
                                         "volumetric_soil_water_layer_4")
    )
  }

  if(DataSet == "era5-land"){
    Variables <- data.frame(Clear = c(),
                            Download = c()
    )
  }

  if(DataSet != "era5" & DataSet != "era5-land"){
    stop("Please select a DataSet from 'era5' or 'era5-land'.")
  }

  return(Variables)
}



#' List of available variables
#'
#' This function is called upon in the krigR function and performs sanity checks for some of the most common error sources in krigin thereby attempting to return more sensible error messages to the user than what is returned by default.
#'
#' @param Data A raster object containing the data to be kriged.
#' @param CovariatesCoarse A raster object containing covariates for kriging at training resolution.
#' @param CovariatesFine A raster object containing covariates for kriging at target resolution.
#' @param KrigingEquation A formula object obtained from a character vector via as.formula() specifying the covariates to be used in kriging. The covariates used have to be present and named as layers in CovariatesCoarse and CovariatesFine.
#' @examples
#' \dontrun{
#' check_Krig(????)
#' }
#'

check_Krig <- function(Data, CovariatesCoarse, CovariatesFine, KrigingEquation){
  ### RESOLUTIONS ----
  if(res(CovariatesFine)[1] < res(Data)[1]/10){
    warning("It is not recommended to use kriging for statistical downscaling of more than one order of magnitude. You are currently attempting this. Kriging will proceed.")
  }
  if(res(CovariatesCoarse) != res(Data)){
    stop(paste0("The resolution of your data (", res(Data)[1], ") does not match the resolution of your covariate data (", res(CovariatesCoarse)[1], ") used for training the kriging model. Kriging can't be performed!" ))
  }
  ### EXTENTS ----
  if(extent(Data) == extent(-180, 180, -90, 90)){
    warning("You are attempting to use kriging at a global extent. For reasons of computational expense, this is not recommended. Instead, try kriging of latitude bands if global kriging is really your goal.")
  }
  if(extent(CovariatesCoarse) != extent(Data) | extent(CovariatesCoarse) != extent(CovariatesFine)){
    stop("The extents of your data and covariates don't match. Kriging can't be performed!")
  }
  ### DATA AVAILABILITY ----
  DataSkips <- NULL # data layers without enough data to be skipped in kriging
  Data_vals <- base::colSums(matrix(!is.na(values(Data)), ncol = nlayers(Data))) # a value of 0 indicates a layer only made of NAs
  if(length(which(Data_vals < 2)) > 0){
    if(length(which(Data_vals < 2)) != nlayers(Data)){
      warning(paste0("Layer(s) ", paste(which(Data_vals == 0), collapse=", "), " of your data do(es) not contain enough data. Kriging will result in a raster identical do the input for this layer."))
      DataSkips <- which(Data_vals < 2)
    }else{
      stop("Your Data does not contain enough values. Kriging can't be performed!")
    }
  }
  CovCo_vals <- base::colSums(matrix(!is.na(values(CovariatesCoarse)), ncol = nlayers(CovariatesCoarse))) # a value of 0 indicates a layer only made of NAs
  if(length(which(CovCo_vals < 2)) > 0){
    if(length(which(CovCo_vals < 2)) != nlayers(CovariatesCoarse)){
      warning(paste0("Layer(s) ", paste(which(CovCo_vals < 2), collapse=", "), " of your covariates at training resolution do(es) not contain enough data. This/these layer(s) is/are dropped. The Kriging equation might get altered."))
      CovariatesCoarse <- CovariatesCoarse[[-which(CovCo_vals < 2)]]
    }else{
      stop("Your covariate data at training resolution does not contain enough values. Kriging can't be performed!")
    }
  }
  CovFin_vals <- base::colSums(matrix(!is.na(values(CovariatesFine)), ncol = nlayers(CovariatesFine))) # a value of 0 indicates a layer only made of NAs
  if(length(which(CovFin_vals < 2)) > 0){
    if(length(which(CovFin_vals < 2)) != nlayers(CovariatesFine)){
      warning(paste0("Layer(s) ", paste(which(CovFin_vals == 0), collapse=", "), " of your covariates at target resolution do(es) not contain enough data. This/these layer(s) is/are dropped."))
      CovariatesFine <- CovariatesFine[[-which(CovFin_vals < 2)]]
    }else{
      stop("Your covariate data at target resolution does not contain enough values. Kriging can't be performed!")
    }
  }
  ### EQUATION ----
  Terms <- unlist(strsplit(labels(terms(KrigingEquation)), split = ":")) # identify parameters called to in formula
  Terms_Required <- unique(Terms) # isolate double-references (e.g. due to ":" indexing for interactions)
  Terms_Present <- Reduce(intersect, list(Terms_Required, names(CovariatesCoarse), names(CovariatesFine))) # identify the terms that are available and required

  if(sum(Terms_Required %in% Terms_Present) != length(Terms_Required)){
    KrigingEquation <- as.formula(paste0("Data ~ ", paste(Terms_Present, collapse = "+")))
    warning(paste("Not all of the terms specified in your KrigingEquation are present in the covariate data sets. The KrigingEquation has been altered to include all available and specified terms in a linear model:", KrigingEquation))
  }

  return(list(KrigingEquation, DataSkips))
}



