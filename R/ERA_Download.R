#' Downloading ERA5(Land)-data from ECMWF servers
#'
#' This function generates a shell script to query downloading of ERA5(Land) data from the ECMWF servers as specified by user input. The actual time to download is dependant on ECMWF download queues. Users need an ECMWF account (https://apps.ecmwf.int/registration/) as well as an API key (https://api.ecmwf.int/v1/key/) to be set up.
#'
#' @param Variable ERA5(Land)-contained climate variable. See output of Variable_List() for possible values.
#' @param Measure Whether to download reanalysis ("Reanalysis") or ensemble ("Ensemble") data.
#' @param DataSet Which ERA5 data set to download data from. "ERA5" or "ERA5Land".
#' @param DateStart Date ("YYYY-MM-DD") at which to start time series of downloaded data.
#' @param DateStop Date ("YYYY-MM-DD") at which to stop time series of downloaded data.
#' @param TResolution Temporal resolution of final product. "Hour", "Day", "Month", "Year", "Decade"
#' @param TStep Which time steps (numeric) to consider for temporal resolution.
#' @param Extent Optional, download data according to rectangular bounding box (speed up downloading and processing)
#' @param Dir Directory specifying where to download data to.
#' @return A NETCDF (.nc) file in the specified directory.
#' @examples
#' # Downloading ERA5-Land air temperature reanalysis data in 12-hour intervals for the entire year of 1951 for the entire earth.
#' ERA_Download(Variable = "Tair", Measure = "Reanalysis", DataSet = "ERA5Land", DateStart = "1950-01-01", DateStop = "1950-12-31", TResolution = "Hour", TStep = 12)
#'
#'
ERA_Download <- function(Variable = NULL, Measure = "Reanalysis", DataSet = "ERA5Land",
                         DateStart = "1950-01-01", DateStop = Sys.Date(),
                         TResolution = "Month", TStep = 1, Extent = NULL,
                         Dir = getwd()){

}
