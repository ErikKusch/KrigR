#' Coordinates of select summits in Jotunheimen national park
#'
#' A data frame with four rows and three columns (Summit, Lon, Lat).
#'
#' @format a data.frame
#'
"Mountains_df"

#' Shapefile of boundaries of Jotunheimen national park
#'
#' An sf object containing a polygon describing the boundaries of Jotunheimen national park
#'
#' @format a sf POLYGON
#'
"Jotunheimen_poly"

#' Daily Air-Temperature Raster across Southern and Central Norway
#'
#' A SpatRaster object containing five layers of daily mean air-temperature data for the time-period 1995-01-01 to 1995-01-5 sourced from ERA5-Land.
#
#' @format a SpatRaster
#' @references Muñoz Sabater, J. (2019): ERA5-Land hourly data from 1950 to present. Copernicus Climate Change Service (C3S) Climate Data Store (CDS). DOI: 10.24381/cds.e2161bac (Accessed on 2024-06-24)
"CDS_rast"
