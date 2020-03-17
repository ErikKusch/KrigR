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
#' @examples
#' \dontrun{
#' Variable_List()
#' }
#'
Variable_List <- function() {
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
  return(Variables)
}
