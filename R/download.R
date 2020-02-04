#' Downloading ERA5(Land)-data from ECMWF servers
#'
#' This function generates a shell script to query downloading of ERA5(Land) data from the ECMWF servers as specified by user input. The actual time to download is dependant on ECMWF download queues. Users need an ECMWF account (https://apps.ecmwf.int/registration/) as well as an API key (https://api.ecmwf.int/v1/key/) to be set up.
#'
#' @param Variable ERA5(Land)-contained climate variable. See output of Variable_List() for possible values.
#' @param Measure Whether to download reanalysis ('Reanalysis') or ensemble ('Ensemble') data.
#' @param DataSet Which ERA5 data set to download data from. 'ERA5' or 'ERA5Land'.
#' @param DateStart Date ('YYYY-MM-DD') at which to start time series of downloaded data.
#' @param DateStop Date ('YYYY-MM-DD') at which to stop time series of downloaded data.
#' @param TResolution Temporal resolution of final product. 'Hour', 'Day', 'Month', 'Year', 'Decade'
#' @param TStep Which time steps (numeric) to consider for temporal resolution.
#' @param Extent Optional, download data according to rectangular bounding box (speed up downloading and processing)
#' @param Dir Directory specifying where to download data to.
#' @return A NETCDF (.nc) file in the specified directory.
#' @examples
#' # Downloading ERA5-Land air temperature reanalysis data in
#' # 12-hour intervals for the entire year of 1951 for the entire earth.
#' download_ERA(Variable = 'Tair', Measure = 'Reanalysis', DataSet = 'ERA5Land',
#' DateStart = '1950-01-01', DateStop = '1950-12-31',
#' TResolution = 'Hour', TStep = 12)
#'
download_ERA <- function(Variable = NULL, Measure = "Reanalysis", DataSet = "ERA5Land",
                         DateStart = "1950-01-01", DateStop = Sys.Date(),
                         TResolution = "Month", TStep = 1, Extent = NULL,
                         Dir = getwd()) {
  print("Test")
}

#' Downloading HWSD data from FAO servers
#'
#' This function downloads and rescales Harmonized World Soil Database v1.2 (HWSD) data from the severs of the Food and Agriculture Organization of the United Nations (FAO). This data is the default for kriging within this package.
#'
#' @param Train_res The training resolution for the kriging step (i.e. wich resolution to downscale from). An object as specified/produced by raster::res().
#' @param Target_res The target resolution for the kriging step (i.e. wich resolution to downscale to). An object as specified/produced by raster::res().
#' @param Extent Optional, download data according to rectangular bounding box. An object of type extent (i.e. raster::extent()).
#' @param Dir Directory specifying where to download data to.
#' @return Two NETCDF (.nc) files in the specified directory.
#' @examples
#' # Downloading HWSD-data at resolutions of 0.08333333 x 0.08333333 (NDVI/target resolution)
#' # and 0.2810168 x 0.2810168 (ERA5/training resolution) within the box shaped
#' # between -50E, -34E, and -23N, 0N (the Caatinga in Brazil).
#' download_HWSD(Train_res = c(0.28125, 0.2810168),
#' Target_res = c(0.08333333, 0.08333333),
#' Extent = extent(-50,-34,-23,0))
#'
download_HWSD <- function(Train_res = NULL,
                          Target_res = NULL,
                          Extent = NULL, Dir = getwd()) {
print("Test")
}
