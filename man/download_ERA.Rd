% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/DEPRECATED.R
\name{download_ERA}
\alias{download_ERA}
\title{Downloading ERA5(Land)-data from ECMWF servers}
\usage{
download_ERA(
  Variable = NULL,
  PrecipFix = FALSE,
  Type = "reanalysis",
  DataSet = "era5-land",
  DateStart = "1981-01-01",
  DateStop = Sys.Date() - 100,
  TResolution = "month",
  TStep = 1,
  FUN = "mean",
  Extent = extent(-180, 180, -90, 90),
  Buffer = 0.5,
  ID = "ID",
  Dir = getwd(),
  FileName = NULL,
  API_User = NULL,
  API_Key = NULL,
  TryDown = 10,
  verbose = TRUE,
  Cores = 1,
  TimeOut = 36000,
  SingularDL = FALSE,
  ...
)
}
\arguments{
\item{Variable}{ERA5(Land)-contained climate variable.}

\item{PrecipFix}{Logical. Era5(-land) total precipitation is recorded in cumulative steps per hour from the 00:00 time mark per day. Setting PrecipFix to TRUE converts these into records which represent the total precipitation per hour. Monthly records in Era5(-land) express the average daily total precipitation. Setting this argument to TRUE multiplies monthly records by the number of days per the respective month(s) to get to total precipitation records instead of average.  Default is FALSE. This also applies to other variables in the data sets. See the data descriptor webpages (e.g.: https://cds.climate.copernicus.eu/cdsapp#!/dataset/reanalysis-era5-land-monthly-means?tab=overview) for an overview of which variables this applies to.}

\item{Type}{Whether to download reanalysis ('reanalysis', 'monthly_averaged_reanalysis_by_hour_of_day') or ensemble ('ensemble_members', 'ensemble_mean', or 'ensemble_spread') data. Only available for era5 data.}

\item{DataSet}{Which ERA5 data set to download data from. 'era5' or 'era5-land'.}

\item{DateStart}{Date ('YYYY-MM-DD') at which to start time series of downloaded data.}

\item{DateStop}{Date ('YYYY-MM-DD') at which to stop time series of downloaded data.}

\item{TResolution}{Temporal resolution of final product. 'hour', 'day', 'month', or 'year'.}

\item{TStep}{Which time steps (numeric) to consider for temporal resolution. For example, specify bi-monthly data records by setting TResolution to 'month' and TStep to 2.}

\item{FUN}{A raster calculation argument as passed to `raster::stackApply()`. This controls what kind of data to obtain for temporal aggregates of reanalysis data. Specify 'mean' (default) for mean values, 'min' for minimum values, and 'max' for maximum values, among others.}

\item{Extent}{Optional, download data according to rectangular bounding box. specify as extent() object or as a raster, a SpatialPolygonsDataFrame object, or a data.frame object. If Extent is a SpatialPolygonsDataFrame, this will be treated as a shapefile and the output will be cropped and masked to this shapefile. If Extent is a data.frame of geo-referenced point records, it needs to contain Lat and Lon columns as well as a non-repeating ID-column.}

\item{Buffer}{Optional. Identifies how big a rectangular buffer to draw around points if Extent is a data frame of points. Buffer is expressed as centessimal degrees.}

\item{ID}{Optional. Identifies which column in Extent to use for creation of individual buffers if Extent is a data.frame.}

\item{Dir}{Directory specifying where to download data to.}

\item{FileName}{A file name for the netcdf produced. Default is a combination parameters in the function call.}

\item{API_User}{Character; ECMWF cds user number.}

\item{API_Key}{Character; ECMWF cds API key.}

\item{TryDown}{Optional, numeric. How often to attempt the download of each individual file that the function queries from the server. This is to circumvent having to restart the entire function when encountering connectivity issues.}

\item{verbose}{Optional, logical. Whether to report progress of the function in the console or not.}

\item{Cores}{Numeric. How many cores to use.^This can speed up downloads of long time-series. If you want output to your console during the process, use Cores = 1. Parallel processing is carried out when Cores is bigger than 1. Default is 1.}

\item{TimeOut}{Numeric. The timeout for each download in seconds. Default 36000 seconds (10 hours).}

\item{SingularDL}{Logical. Whether to force download of data in one call to CDS or automatically break download requests into individual monthly downloads. Default is FALSE.}

\item{...}{Additional arguments used for parsing more information to the ecmwfr download call such as pressure_level = 1 for download of pressure_level data for the ERA5 reanalysis}
}
\value{
A raster object containing the downloaded ERA5(-Land) data, and a NETCDF (.nc) file in the specified directory.
}
\description{
This function is used to obtain Era5/Era5-Land data from the [Climate Data Store](https://cds.climate.copernicus.eu/#!/home) hosted by the [Copernicus Climate Change Service (C3S)](https://cds.climate.copernicus.eu/about-c3s). By default, this function breaks down download calls into monthly intervals, downloads Era5(-Land) data from [ECMWF](https://www.ecmwf.int/) servers according to user-specification, and fuses the downloaded files together according to user-demands. The actual time to download is dependent on ECMWF download queues. Users need an [API key](https://cds.climate.copernicus.eu/api-how-to) for download staging.
}
\details{
Use optional arguments verbose, Cores, and SingularDL for updates on function progress, parallel download staging and execution, and forcing of downloads into one singular download, espectively.
}
\examples{
\dontrun{
# Downloading ERA5-Land air temperature reanalysis data in 12-hour intervals for 02/01/1995 - 04/01/1995 (DD/MM/YYYY). API User and Key in this example are non-functional. Substitute with your user number and key to run this example.
Extent <- extent(11.8,15.1,50.1,51.7) # roughly the extent of Saxony
API_User <- "..."
API_Key <- "..."
State_Raw <- download_ERA(
Variable = "2m_temperature",
DataSet = "era5-land",
DateStart = "1995-01-02",
DateStop = "1995-01-04",
TResolution = "hour",
TStep = 12,
Extent = Extent,
API_User = API_User,
API_Key = API_Key
)
State_Raw # a raster brick with 6 layers at resolution of ~0.1°
}

}
