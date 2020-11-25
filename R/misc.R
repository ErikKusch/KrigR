#' List of available variables
#'
#' This function presents the user with a selection of biologically relevant variables of the Era5-family which can be statistically downscaled. Notice that more variables are available and can be found here: \url{https://cds.climate.copernicus.eu/cdsapp#!/dataset/reanalysis-era5-land?tab=form} for Era5-Land data and here: \url{https://cds.climate.copernicus.eu/cdsapp#!/dataset/reanalysis-era5-single-levels?tab=form} for Era5 data. Notice that variable names for download requests are written in all lower-case letters and substitute spaces for underscores.
#'
#' @param DataSet Which ERA5 data set to download data from. 'era5', 'era5-land', or 'uerra'.
#' @examples
#' Variable_List(DataSet = "era5")
#'
#' @export
Variable_List <- function(DataSet) {
  if(DataSet == "era5"){
    Variables <- data.frame(Clear = c("100m u-component of wind",
                                      "100m v-component of wind",
                                      "10m u-component of wind",
                                      "10m v-component of wind",
                                      "10m u-component of neutral wind",
                                      "10m v-component of neutral wind",
                                      "10m wind speed",
                                      "2m dewpoint temperature",
                                      "2m temperature",
                                      "Angle of subgridscale orography",
                                      "Anisotropy of subgridscale orography",
                                      "Benjamin Feir index",
                                      "Boundary layer dissipation",
                                      "Boundary layer height",
                                      "Charnok",
                                      "clear_sky_direct_solar_radiation_at_surface",
                                      "cloud_base_height",
                                      "convective_available_potential_energy",
                                      "downward_uv_radiation_at_the_surface",
                                      "eastward_turbulent_surface_stress",
                                      "evaporation",
                                      "Forecast albedo",
                                      "friction_velocity",
                                      "high_cloud_cover",
                                      "Lake bottom temperature",
                                      "Lake depth",
                                      "Lake ice depth",
                                      "Lake ice temperature",
                                      "Lake mix-layer depth",
                                      "Lake mix-layer temperature",
                                      "Lake shape factor",
                                      "Lake total layer temperature",
                                      "land_sea_mask",
                                      "Leaf area index, high vegetation",
                                      "Leaf area index, low vegetation",
                                      "low_cloud_cover",
                                      "mean_potential_evaporation_rate",
                                      "mean_runoff_rate",
                                      "mean_sea_level_pressure",
                                      "mean_sub_surface_runoff_rate",
                                      "mean_surface_direct_short_wave_radiation_flux",
                                      "mean_surface_downward_long_wave_radiation_flux",
                                      "mean_surface_downward_short_wave_radiation_flux",
                                      "mean_surface_downward_uv_radiation_flux",
                                      "mean_surface_latent_heat_flux",
                                      "mean_surface_net_long_wave_radiation_flux",
                                      "mean_surface_net_short_wave_radiation_flux",
                                      "mean_surface_sensible_heat_flux",
                                      "mean_surface_runoff_rate",
                                      "mean_total_precipitation_rate",
                                      "medium_cloud_cover",
                                      "orography",
                                      "Potential evaporation",
                                      "precipitation_type",
                                      "Runoff",
                                      "Skin reservoir content",
                                      "Skin temperature",
                                      "Snow albedo",
                                      "Snow cover",
                                      "Snow density",
                                      "Snow depth",
                                      "Snow evaporation",
                                      "Snowfall",
                                      "Snowmelt",
                                      "Soil temperature level 1",
                                      "Soil temperature level 2",
                                      "Soil temperature level 3",
                                      "Soil temperature level 4",
                                      "Sub-surface runoff",
                                      "Surface latent heat flux",
                                      "Surface net solar radiation",
                                      "Surface net thermal radiation",
                                      "Surface pressure",
                                      "Surface runoff",
                                      "Surface sensible heat flux",
                                      "Surface solar radiation downwards",
                                      "Surface thermal radiation downwards",
                                      "Temperature of snow layer",
                                      "Total precipitation",
                                      "Volumetric soil water layer 1",
                                      "Volumetric soil water layer 2",
                                      "Volumetric soil water layer 3",
                                      "Volumetric soil water layer 4"),
                            Download = c("100m_u_component_of_wind",
                                         "100m_v_component_of_wind",
                                         "10m_u_component_of_wind",
                                         "10m_v_component_of_wind",
                                         "10m_u_component_of_neutral_wind",
                                         "10m_v_component_of_neutral_wind",
                                         "10m_wind_speed",
                                         "2m_dewpoint_temperature",
                                         "2m_temperature",
                                         "angle_of_sub_gridscale_orography",
                                         "anisotropy_of_sub_gridscale_orography",
                                         "benjamin_feir_index",
                                         "boundary_layer_dissipation",
                                         "boundary_layer_height",
                                         "Charnok",
                                         "clear_sky_direct_solar_radiation_at_surface",
                                         "cloud_base_height",
                                         "convective_available_potential_energy",
                                         "downward_uv_radiation_at_the_surface",
                                         "eastward_turbulent_surface_stress",
                                         "evaporation",
                                         "forecast_albedo",
                                         "friction_velocity",
                                         "high_cloud_cover",
                                         "lake_bottom_temperature",
                                         "lake_depth",
                                         "lake_ice_depth",
                                         "lake_ice_temperature",
                                         "lake_mix_layer_depth",
                                         "lake_mix_layer_temperature",
                                         "lake_shape_factor",
                                         "lake_total_layer_temperature",
                                         "land_sea_mask",
                                         "leaf_area_index_high_vegetation",
                                         "leaf_area_index_low_vegetation",
                                         "low_cloud_cover",
                                         "mean_potential_evaporation_rate",
                                         "mean_runoff_rate",
                                         "mean_sea_level_pressure",
                                         "mean_sub_surface_runoff_rate",
                                         "mean_surface_direct_short_wave_radiation_flux",
                                         "mean_surface_downward_long_wave_radiation_flux",
                                         "mean_surface_downward_short_wave_radiation_flux",
                                         "mean_surface_downward_uv_radiation_flux",
                                         "mean_surface_latent_heat_flux",
                                         "mean_surface_net_long_wave_radiation_flux",
                                         "mean_surface_net_short_wave_radiation_flux",
                                         "mean_surface_sensible_heat_flux",
                                         "mean_surface_runoff_rate",
                                         "mean_total_precipitation_rate",
                                         "medium_cloud_cover",
                                         "orography",
                                         "potential_evaporation",
                                         "precipitation_type",
                                         "runoff",
                                         "skin_reservoir_content",
                                         "skin_temperature",
                                         "snow_albedo",
                                         "snow_cover",
                                         "snow_density",
                                         "snow_depth",
                                         "snow_evaporation",
                                         "snowfall",
                                         "snowmelt",
                                         "soil_temperature_level_1",
                                         "soil_temperature_level_2",
                                         "soil_temperature_level_3",
                                         "soil_temperature_level_4",
                                         "sub_surface_runoff",
                                         "surface_latent_heat_flux",
                                         "surface_net_solar_radiation",
                                         "surface_net_thermal_radiation",
                                         "surface_pressure",
                                         "surface_runoff",
                                         "surface_sensible_heat_flux",
                                         "surface_solar_radiation_downwards",
                                         "surface_thermal_radiation_downwards",
                                         "temperature_of_snow_layer",
                                         "total_precipitation",
                                         "volumetric_soil_water_layer_1",
                                         "volumetric_soil_water_layer_2",
                                         "volumetric_soil_water_layer_3",
                                         "volumetric_soil_water_layer_4"),
                            Unit = c("m/s",
                                     "m/s",
                                     "m/s",
                                     "m/s",
                                     "m/s",
                                     "m/s",
                                     "m/s",
                                     "K",
                                     "K",
                                     "Radians",
                                     " ",
                                     " ",
                                     "J/m^2",
                                     "m",
                                     " ",
                                     "J/m^2",
                                     "m",
                                     "J/kg",
                                     "J/m^2",
                                     "N/m^2",
                                     "kg/m^2",
                                     "(0 - 1)",
                                     "m/s",
                                     "%",
                                     "K",
                                     "m",
                                     "m",
                                     "K",
                                     "m",
                                     "K",
                                     "dimensionless",
                                     "K",
                                     "(0-1)",
                                     "m^2/m^2",
                                     "m^2/m^2",
                                     "%",
                                     "kg/(m^2*s)",
                                     "m/s",
                                     "Pa",
                                     "kg/(m^2*s)",
                                     "J/m^2",
                                     "J/m^2",
                                     "J/m^2",
                                     "J/m^2",
                                     "J/m^2",
                                     "J/m^2",
                                     "J/m^2",
                                     "J/m^2",
                                     "kg/(m^2*s)",
                                     "kg/(m^2*s)",
                                     "%",
                                     "m",
                                     "m",
                                     " ",
                                     "m",
                                     "m of water equivalent",
                                     "K",
                                     "(0 - 1)",
                                     "%",
                                     "kg/m^3",
                                     "m",
                                     "m of water equivalent",
                                     "m of water equivalent",
                                     "m of water equivalent",
                                     "K",
                                     "K",
                                     "K",
                                     "K",
                                     "m",
                                     "J/m^2",
                                     "J/m^2",
                                     "J/m^2",
                                     "Pa",
                                     "m",
                                     "J/m^2",
                                     "J/m^2",
                                     "J/m^2",
                                     "K",
                                     "m",
                                     "m^3/m^3",
                                     "m^3/m^3",
                                     "m^3/m^3",
                                     "m^3/m^3")
    )
  }

  if(DataSet == "era5-land"){
    Variables <- data.frame(Clear = c("10m u-component of wind",
                                      "10m v-component of wind",
                                      "2m dewpoint temperature",
                                      "2m temperature",
                                      "Evaporation from bare soil",
                                      "Evaporation from open water surfaces excluding oceans",
                                      "Evaporation from the top of canopy",
                                      "Evaporation from vegetation transpiration",
                                      "Evapotranspiration",
                                      "Forecast albedo",
                                      "Lake bottom temperature",
                                      "Lake ice depth",
                                      "Lake ice temperature",
                                      "Lake mix-layer depth",
                                      "Lake mix-layer temperature",
                                      "Lake shape factor",
                                      "Lake total layer temperature",
                                      "Leaf area index, high vegetation",
                                      "Leaf area index, low vegetation",
                                      "Potential evaporation",
                                      "Runoff",
                                      "Skin reservoir content",
                                      "Skin temperature",
                                      "Snow albedo",
                                      "Snow cover",
                                      "Snow density",
                                      "Snow depth",
                                      "Snow depth water equivalent",
                                      "Snow evaporation",
                                      "Snowfall",
                                      "Snowmelt",
                                      "Soil temperature level 1",
                                      "Soil temperature level 2",
                                      "Soil temperature level 3",
                                      "Soil temperature level 4",
                                      "Sub-surface runoff",
                                      "Surface latent heat flux",
                                      "Surface net solar radiation",
                                      "Surface net thermal radiation",
                                      "Surface pressure",
                                      "Surface runoff",
                                      "Surface sensible heat flux",
                                      "Surface solar radiation downwards",
                                      "Surface thermal radiation downwards",
                                      "Temperature of snow layer",
                                      "Total precipitation",
                                      "Volumetric soil water layer 1",
                                      "Volumetric soil water layer 2",
                                      "Volumetric soil water layer 3",
                                      "Volumetric soil water layer 4"),
                            Download = c("10m_u_component_of_wind",
                                         "10m_v_component_of_wind",
                                         "2m_dewpoint_temperature",
                                         "2m_temperature",
                                         "evaporation_from_bare_soil",
                                         "evaporation_from_open_water_surfaces_excluding_oceans",
                                         "evaporation_from_the_top_of_canopy",
                                         "evaporation_from_vegetation_transpiration",
                                         "evapotranspiration",
                                         "forecast_albedo",
                                         "lake_bottom_temperature",
                                         "lake_ice_depth",
                                         "lake_ice_temperature",
                                         "lake_mix_layer_depth",
                                         "lake_mix_layer_temperature",
                                         "lake_shape_factor",
                                         "lake_total_layer_temperature",
                                         "leaf_area_index_high_vegetation",
                                         "leaf_area_index_low_vegetation",
                                         "potential_evaporation",
                                         "runoff",
                                         "skin_reservoir_content",
                                         "skin_temperature",
                                         "snow_albedo",
                                         "snow_cover",
                                         "snow_density",
                                         "snow_depth",
                                         "snow_depth_water_equivalent",
                                         "snow_evaporation",
                                         "snowfall",
                                         "snowmelt",
                                         "soil_temperature_level_1",
                                         "soil_temperature_level_2",
                                         "soil_temperature_level_3",
                                         "soil_temperature_level_4",
                                         "sub_surface_runoff",
                                         "surface_latent_heat_flux",
                                         "surface_net_solar_radiation",
                                         "surface_net_thermal_radiation",
                                         "surface_pressure",
                                         "surface_runoff",
                                         "surface_sensible_heat_flux",
                                         "surface_solar_radiation_downwards",
                                         "surface_thermal_radiation_downwards",
                                         "temperature_of_snow_layer",
                                         "total_precipitation",
                                         "volumetric_soil_water_layer_1",
                                         "volumetric_soil_water_layer_2",
                                         "volumetric_soil_water_layer_3",
                                         "volumetric_soil_water_layer_4"),
                            Unit = c("m/s",
                                     "m/s",
                                     "K",
                                     "K",
                                     "m of water equivalent",
                                     "m of water equivalent",
                                     "m of water equivalent",
                                     "m of water equivalent",
                                     "m of water equivalent",
                                     "(0 - 1)",
                                     "K",
                                     "m",
                                     "K",
                                     "m",
                                     "K",
                                     "dimensionless",
                                     "K",
                                     "m^2/m^2",
                                     "m^2/m^2",
                                     "m",
                                     "m",
                                     "m of water equivalent",
                                     "K",
                                     "(0 - 1)",
                                     "%",
                                     "kg/m^3",
                                     "m",
                                     "m of water equivalent",
                                     "m of water equivalent",
                                     "m of water equivalent",
                                     "m of water equivalent",
                                     "K",
                                     "K",
                                     "K",
                                     "K",
                                     "m",
                                     "J/m^2",
                                     "J/m^2",
                                     "J/m^2",
                                     "Pa",
                                     "m",
                                     "J/m^2",
                                     "J/m^2",
                                     "J/m^2",
                                     "K",
                                     "m",
                                     "m^3/m^3",
                                     "m^3/m^3",
                                     "m^3/m^3",
                                     "m^3/m^3")

    )
  }

  if(DataSet == "uerra"){
    Variables <- data.frame(Clear = c("Pressure",
                                      "Relative humidity",
                                      "Temperature",
                                      "Wind direction",
                                      "Wind speed",
                                      "Geopotential",
                                      "Geopotential height",
                                      "U-component of wind",
                                      "V-component of wind",
                                      "10m wind direction",
                                      "10m wind speed",
                                      "2m relative humidity",
                                      "2m temperature",
                                      "Albedo",
                                      "High cloud cover",
                                      "Land-sea mask",
                                      "Low cloud cover",
                                      "Mean sea level pressure",
                                      "Medium cloud cover",
                                      "Orography",
                                      "Skin temperature",
                                      "Snow density",
                                      "Snow depth water equivalent",
                                      "Surface pressure",
                                      "Surface roughness",
                                      "Total cloud cover",
                                      "Total column integrated water vapour",
                                      "Total precipitation",
                                      "Soil temperature",
                                      "Volumetric soil moisture",
                                      "Volumetric transpiration stress-onset (soil moisture)",
                                      "Volumetric wilting point"),
                            Download = c("pressure",
                                         "relative_humidity",
                                         "temperature",
                                         "wind_direction",
                                         "wind_speed",
                                         "geopotential",
                                         "geopotential_height",
                                         "u-component_of_wind",
                                         "v-component_of_wind",
                                         "10m_wind_direction",
                                         "10m_wind_speed",
                                         "2m_relative_humidity",
                                         "2m_temperature",
                                         "albedo",
                                         "high_cloud_cover",
                                         "land-sea mask",
                                         "low_cloud_cover",
                                         "mean_sea_level_pressure",
                                         "medium_cloud_cover",
                                         "orography",
                                         "skin_temperature",
                                         "snow_density",
                                         "snow_depth_water_equivalent",
                                         "surface_pressure",
                                         "surface_roughness",
                                         "total_cloud_cover",
                                         "total_column_integrated_water_vapour",
                                         "total_precipitation",
                                         "soil_temperature",
                                         "volumetric_soil_moisture",
                                         "volumetric_transpiration_stress-onset",
                                         "volumetric_wilting point"),
                            Unit = c("Pa",
                                     "%",
                                     "K",
                                     "Degrees",
                                     "m/s",
                                     "m^2/s^2",
                                     "gpm",
                                     "m/s",
                                     "m/s",
                                     "Degrees",
                                     "m/s",
                                     "%",
                                     "K",
                                     "%",
                                     "%",
                                     "Dimensionless",
                                     "%",
                                     "Pa",
                                     "%",
                                     "gpm",
                                     "K",
                                     "kg/m^3",
                                     "kg/m^2",
                                     "Pa",
                                     "m",
                                     "%",
                                     "kg/m^2",
                                     "kg/m^2",
                                     "K",
                                     "m^3/m^3",
                                     "m^3/m^3",
                                     "m^3/m^3"),
                            DataSet = c("height-levels",
                                        "height-levels/pressure-levels",
                                        "height-levels/pressure-levels",
                                        "height-levels",
                                        "height-levels",
                                        "pressure-levels",
                                        "pressure-levels",
                                        "pressure-levels",
                                        "pressure-levels",
                                        "single-levels",
                                        "single-levels",
                                        "single-levels",
                                        "single-levels",
                                        "single-levels",
                                        "single-levels",
                                        "single-levels",
                                        "single-levels",
                                        "single-levels",
                                        "single-levels",
                                        "single-levels",
                                        "single-levels",
                                        "single-levels",
                                        "single-levels",
                                        "single-levels",
                                        "single-levels",
                                        "single-levels",
                                        "single-levels",
                                        "single-levels",
                                        "soil-levels",
                                        "soil-levels",
                                        "soil-levels",
                                        "soil-levels"),
                            Origin = c(rep(NA, 9), # no origin specified for height and pressure levels
                                       "mescan_surfex/uerra_harmonie",
                                       "mescan_surfex/uerra_harmonie",
                                       "mescan_surfex/uerra_harmonie",
                                       "mescan_surfex/uerra_harmonie",
                                       "uerra_harmonie",
                                       "uerra_harmonie",
                                       "mescan_surfex/uerra_harmonie",
                                       "uerra_harmonie",
                                       "uerra_harmonie",
                                       "uerra_harmonie",
                                       "mescan_surfex/uerra_harmonie",
                                       "uerra_harmonie",
                                       "uerra_harmonie",
                                       "uerra_harmonie",
                                       "uerra_harmonie",
                                       "uerra_harmonie",
                                       "uerra_harmonie",
                                       "uerra_harmonie",
                                       "mescan_surfex",
                                       "uerra_harmonie",
                                       "uerra_harmonie",
                                       "mescan_surfex",
                                       "mescan_surfex")
                            )
  }

  if(DataSet != "era5" & DataSet != "era5-land" & DataSet != "uerra"){
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
#'
check_Krig <- function(Data, CovariatesCoarse, CovariatesFine, KrigingEquation){
  ### RESOLUTIONS ----
  if(res(CovariatesFine)[1] < res(Data)[1]/10){
    warning("It is not recommended to use kriging for statistical downscaling of more than one order of magnitude. You are currently attempting this. Kriging will proceed.")
  }
  if(!all.equal(res(CovariatesCoarse)[1], res(Data)[1])){
    stop(paste0("The resolution of your data (", res(Data)[1], ") does not match the resolution of your covariate data (", res(CovariatesCoarse)[1], ") used for training the kriging model. Kriging can't be performed!" ))
  }
  ### EXTENTS ----
  # if(extent(Data) == extent(-180, 180, -90, 90)){
  #   stop("You are attempting to use kriging at a global extent. For reasons of computational expense and identity of relationships between covariates and variables not being homogenous across the globe, this is not recommended. Instead, try kriging of latitude bands if global kriging is really your goal.")
  # }
  if(extent(CovariatesCoarse) != extent(Data)){
    stop("The extents of your data and training covariates don't match. Kriging can't be performed!")
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
    if(length(Terms_Present) == 0){ # if none of the specified terms were found
      KrigingEquation <- paste0("Data ~ ", paste(names(CovariatesCoarse), collapse = "+"))
      warn <- paste("None of the terms specified in your KrigingEquation are present in the covariate data sets. The KrigingEquation has been altered to include all available terms in a linear model:", KrigingEquation)
    }else{ # at least some of the specified terms were found
      KrigingEquation <- paste0("Data ~ ", paste(Terms_Present, collapse = "+"))
      warn <- paste("Not all of the terms specified in your KrigingEquation are present in the covariate data sets. The KrigingEquation has been altered to include all available and specified terms in a linear model:", KrigingEquation)
    }
    Cotinue <- menu(c("Yes", "No"), title=paste(warn, "Do you wish to continue using the new formula?"))
    if(Cotinue == 2){ # break operation if user doesn't want this
      stop("Kriging terminated by user due to formula issues.")
    }
  }

  ### NA DATA IN LAYERS ----
  # CovariatesFine <- CovariatesFine[[which(names(CovariatesFine) %in% Terms_Present)]] # only look at layers that the krigignequation targets
  # if(nlayers(CovariatesFine) > 1){
  #   MaskedPix <- length(which(values(sum(CovariatesFine, na.rm = TRUE)) != 0)) # number of non-masked pixels in which data is present in at least one layer
  #   MissingPix <- length(which(!is.na(values(sum(CovariatesFine, na.rm = FALSE))))) # number of pixels in which all layers have data
  #   if(MissingPix < MaskedPix){ # when there are any pixels for which data is absent for at least one layer
  #     stop("One or more more of your target covariate layers is missing data in locations where data is present for other layers. Please either fill these pixels with data or omit terms targeting these layers from your Kriging equation.")
  #   }
  # }
  return(list(as.formula(KrigingEquation), DataSkips))
}

#' Summary of Raster file characteristics
#'
#' This function is called upon in the krigR function and summarizes Raster characteristics without carrying along the raster file itself. This is used to create lists tracking calls to the function krigR without bloating them too much.
#'
#' @param Object_ras A raster object..
#' @examples
#'
#'
SummarizeRaster <- function(Object_ras = NULL){
  Summary_ls <- list(Class = class(Object_ras),
                     Dimensions = list(nrow = nrow(Object_ras),
                                       ncol = ncol(Object_ras),
                                       ncell = ncell(Object_ras)),
                     Extent = Object_ras@extent,
                     CRS = crs(Object_ras),
                     layers = names(Object_ras))
  return(Summary_ls)
}

#' Square Buffers Around Point Data
#'
#' @param Points A data.frame containing geo-referenced points with Lat and Lon columns
#' @param Buffer Identifies how big a rectangular buffer to draw around points. Expressed as centessimal degrees.
#' @param ID Identifies which column in to use for creation of individual buffers.
#'
#' @param XXX
#' @examples
#'
#'
buffer_Points <- function(Points = NULL, Buffer = .5, ID = "ID"){
  # set the radius for the plots
  radius <- Buffer # radius in meters
  # define the plot edges based upon the plot radius.
  yPlus <- Points$Lat+radius
  xPlus <- Points$Lon+radius
  yMinus <- Points$Lat-radius
  xMinus <- Points$Lon-radius
  # calculate polygon coordinates for each plot centroid.
  square=cbind(xMinus,yPlus,  # NW corner
               xPlus, yPlus,  # NE corner
               xPlus,yMinus,  # SE corner
               xMinus,yMinus, # SW corner
               xMinus,yPlus)  # NW corner again - close ploygon
  # Extract the plot ID information
  ID = Points[,ID]
  # create spatial polygons from coordinates
  polys <- SpatialPolygons(mapply(function(poly, id)
  {
    xy <- matrix(poly, ncol=2, byrow=TRUE)
    Polygons(list(Polygon(xy)), ID=id)
  },
  split(square, row(square)), ID),
  proj4string = CRS(as.character("+proj=utm +zone=11 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))
  )
}



#' Range Masking with Edge Support
#'
#' XXXX
#'
#' @param base.map A raster wwithin which coverage should be identified
#' @param Shape A polygon(-collection) whose coverage of the raster object is to be found.
#' @examples
#'
#'
mask_Shape <- function(base.map = NULL, Shape = NULL){
  base.map[] <- NA
  stars.base.map <- stars::st_as_stars(base.map)
  # Subset shape file
  select.ranges <- sf::st_as_sf(Shape)
  # Cast polygon as lines instead
  select.ranges.lines <- sf::st_cast(select.ranges, "MULTILINESTRING")
  select.ranges.lines$STARS <- 1
  # Get centroids (FAST!)
  range <- fasterize(select.ranges, base.map, fun = "first", background = 0)
  # Get edges (slower than fasterize but faster than rasterize)
  range.edges <- stars::st_rasterize(select.ranges.lines, stars.base.map, options = "ALL_TOUCHED=TRUE")
  range.edges <- as.vector(range.edges[[1]])
  range.edges <- ifelse(is.na(range.edges), 0, 1)
  # Merge
  range[] <- ifelse(range[] + range.edges, 1, 0)
  range[range==0] <- NA # set all cells which the shape doesn't touch to NA
  return(range)
}
