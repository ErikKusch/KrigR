#' Downloading ERA5(Land)-data from ECMWF servers
#'
#' This function generates a shell script to query downloading of ERA5(Land) data from the ECMWF servers as specified by user input. The actual time to download is dependant on ECMWF download queues. Users need an API key (https://cds.climate.copernicus.eu/api-how-to) to be set up.
#'
#' @param Variable ERA5(Land)-contained climate variable. See output of Variable_List() for possible values.
#' @param Type Whether to download reanalysis ('reanalysis') or ensemble ('ensemble_members', 'ensemble_mean', or 'ensemble_spread') data.
#' @param DataSet Which ERA5 data set to download data from. 'era5' or 'era5-land'.
#' @param DateStart Date ('YYYY-MM-DD') at which to start time series of downloaded data.
#' @param DateStop Date ('YYYY-MM-DD') at which to stop time series of downloaded data.
#' @param TResolution Temporal resolution of final product. hour', 'day', 'month'
#' @param TStep Which time steps (numeric) to consider for temporal resolution.
#' @param Extent Optional, download data according to rectangular bounding box. Specify as "ymax/xmin/ymin/xmax" (a global extent would read "90/-180/-90/180"). Alternatively, a raster or a SpatialPolygonsDataFrameobject. If Extent is a SpatialPolygonsDataFrame, this will be treated as a shapefile and the output will be cropped and masked to this shapefile.
#' @param Dir Directory specifying where to download data to.
#' @param FileName A file name for the netcdf produced. Default is a combination parameters in the function call.
#' @param API_Key ECMWF cds API key
#' @param API_User ECMWF cds user number
#' @return A NETCDF (.nc) file in the specified directory as well as a raster object containing the specified data.
#' @examples
#' \dontrun{
#' # Downloading ERA5-Land air temperature reanalysis data in 12-hour intervals for the entire year of 1951 for Germany.
#' download_ERA(Variable = '2m_temperature', Type = 'reanalysis', DataSet = 'era5-land', DateStart = '2000-01-01', DateStop = '2000-12-31', TResolution = 'hour', TStep = 12, Extent = "55/5/47/16", API_User = 12345, API_Key = 1234567891012345678910)
#' }
#'
download_ERA <- function(Variable = NULL, Type = "reanalysis", DataSet = "era5-land",
                         DateStart = "1981-01-01", DateStop = Sys.Date(),
                         TResolution = "Month", TStep = 1, Extent = "90/-180/-90/180",
                         Dir = getwd(), FileName = NULL,
                         API_User = NULL, API_Key = NULL) {

  ### SETTING UP API ----
  # Setting the API key for later retrieval by wf_request()
  API_Service = "cds"
  wf_set_key(user=as.character(API_User),
             key=as.character(API_Key),
             service=as.character(API_Service))

  # Type
  if(DataSet == "era5-land"){ # product check
    Type <- NA # set Type to NA for later omission from request list when downloading era5-land data
  } # end of product check

  # Data Set
  if(DataSet == "era5"){ # only append "single-levels" to era5 specification
    DataSet <- paste(DataSet, "single-levels", sep="-") # target reanalysis data sets of ECMWF servers
  }
  DataSet <- paste("reanalysis", DataSet, sep="-") # era5 family data sets must be adressed with "reanalysis-"
  if(TResolution != "hour" & TResolution != "day" ){ # if we don't want sub-daily data
    DataSet <- paste0(DataSet, "-monthly", "-means") # address daily, monthly, or annual means
  }

  # Dates
  DateStart <- as.Date(DateStart) # reformatting date
  DateStop <- as.Date(DateStop) # reformatting date

  #Years
  Years <- as.character(format(DateStart,'%Y'):format(DateStop,'%Y')) # month check: make a string of all years required

  # Months
  if(format(DateStart,'%m') <= format(DateStop,'%m')){ # if beginning month is earlier than stopping month in annual sequence
    Months <- str_pad(as.character(format(DateStart,'%m'):format(DateStop,'%m')), 2, "left","0") # set Months to sequence of months between dates
  }else{ # beginning month is later than stopping month in annual sequence
    Months <- str_pad(as.character(rev(format(DateStart,'%m'):format(DateStop,'%m'))), 2, "left","0") # invert month order
  } # end of month check

  # Days
  if(format(DateStart,'%m') == format(DateStop,'%m') & format(DateStart,'%Y') == format(DateStop,'%Y')){ # equality check: if time series does not exceed one month
    Days <- str_pad(as.character(format(DateStart,'%d'):format(DateStop,'%d')), 2, "left","0") # list all days between and including start and stop days
  }else{ # if time spans more than one month or overlaps months
    Days <- str_pad(1:31,2,"left","0") # full array of days for months
  } # end of equality check

  # Extent, fix for global
  if(class(Extent) != "character"){ # character check: whether already specified as character string
    if(class(Extent) == "Raster" | class(Extent) == "SpatialPolygonsDataFrame"){ # sanity check: ensure it is an Extent object if not character
      if(class(Extent) == "SpatialPolygonsDataFrame"){ # shape check
        Shape <- Extent
      }# end of shape check
      Extent <- extent(Extent) # extract extent
      Extent <- try(paste(Extent[4], Extent[1], Extent[3], Extent[2], sep="/")) # break Extent object down into character
    }else{ # if sanity check is failed, stop function and inform user
      stop('The Extent argument provided by you is neither formatted as a character of "xmin/xmax/ymin/ymax" nor a Raster or SpatialPolygonsDataFrame object. Please correct this.')
    } # end of sanity check
  } # end of character check

  # Time
  if(TResolution == "hour" | TResolution == "day"){ # time check: if we need sub-daily data
    Times <- str_pad(str_c(0:23,"00",sep=":"), 5,"left","0")
  }else{ # if data intervals are monthly or bigger
    Times <- "00:00"
  } # end of time check

  # FileName
  if(is.null(FileName)){
    FileName <- paste(Variable, DateStart, DateStop, TResolution, sep="_")
  }

  ### BUILDING REQUEST ----
  # Setting parameters of desired downloaded netcdf file according to user input
  Request_ls <- list("dataset"        = DataSet,
                     "product_type"   = Type,
                     "variable"       = Variable,
                     "year"           = Years,
                     "month"          = Months,
                     "day"            = Days,
                     "time"           = Times,
                     "area"           = Extent,
                     "format"         = "netcdf",
                     "target"         = paste0(FileName, ".nc"))
  Request_ls <- Request_ls[-which(is.na(Request_ls))] # removing NA type if era5-land is targeted

  ### EXECUTING REQUEST ----
  wf_request(user = as.character(API_User),
             request = Request_ls,
             transfer = TRUE,
             path = Dir,
             verbose = TRUE)


  ### LOAD DATA BACK IN ----
  Era5_ras <- brick(paste0(Dir, "/", FileName, ".nc")) # loading the data

  ### DAY/YEAR MEANS ----
  if(TResolution == "day" | TResolution == "year"){ # day/year check: need to build averages for days (from hours) and years (from months)
    if(TResolution == "day"){ # daily means
      factor <- 24 # number of hours per day
    }else{ # annual means
      factor <- 12 # number of months per year
    }
    Index <- rep(1:(nlayers(Era5_ras)/factor), each = factor) # build an index
    Era5_ras <- stackApply(Era5_ras, Index, fun='mean') # do the calculation
  }# end of day/year check

  ### TIME STEP MEANS ----
  if(nlayers(Era5_ras)%%TStep != 0){ # sanity check for completeness of time steps and data
    warning(paste0("Your specified time range does not allow for a clean integration of your selected time steps. Only full time steps will be computed. You specified a time series with a length of ", nlayers(Era5_ras), "(", TResolution,") and time steps of ", TStep, ". This works out to ", nlayers(Era5_ras)/TStep, " intervals. You will receive ", floor(nlayers(Era5_ras)/TStep), " intervals."))
  }# end of sanity check for time step completeness
  Index <- rep(1:(nlayers(Era5_ras)/TStep), each = TStep) # build an index
  Era5_ras <- stackApply(Era5_ras[[1:length(Index)]], Index, fun='mean') # do the calculation

  ### MASKING ----
  if(exists("Shape")){ # Shape check
    Era5_ras <- mask(Era5_ras, Shape)
  }# end of Shape check

  ### SAVING DATA ----
  return(Era5_ras)
  writeRaster(x = Era5_ras, filename = paste0(Dir, "/", FileName, ".nc"), overwrite = TRUE)
}

#' Downloading HWSD data from FAO servers
#'
#' This function downloads and rescales the median statistic of the Global Multi-resolution Terrain Elevation Data (GMTED2010) data from the severs of the U.S. Geological Survey (USGS) available at \url{https://topotools.cr.usgs.gov/gmted_viewer/gmted2010_global_grids.php}. The data is downloaded at 30 arc-sec latitude/longitude grid cells and subsequently resampled to match Train_res and Target_res. This data is the default for kriging within this package.
#'
#' @param Train_res The training resolution for the kriging step (i.e. wich resolution to downscale from). An object as specified/produced by raster::res().
#' @param Target_res The target resolution for the kriging step (i.e. wich resolution to downscale to). An object as specified/produced by raster::res().
#' @param Extent Optional, download data according to rectangular bounding box. Specify as extent object (obtained via raster::extent()). Alternatively, a raster or a SpatialPolygonsDataFrameobject. If Extent is a SpatialPolygonsDataFrame, this will be treated as a shapefile and the output will be cropped and masked to this shapefile.
#' @param Dir Directory specifying where to download data to.
#' @param Keep_Temporary Logical, whether to delete individual, global, 30 arc-sec files to be reused in later analyses.
#' @return Two NETCDF (.nc) files in the specified directory.
#' @examples
#' \dontrun{
#' # Downloading HWSD-data at resolutions of 0.01 x 0.01 (~30 arc-sec/target resolution) and 0.1 x 0.1 (ERA5-land/training resolution) across Germany.
#' download_HWSD(Train_res = c(0.1, 0.1), Target_res = c(0.01, 0.01), Extent = extent(5, 16, 47, 55))
#' }
#'
download_HWSD <- function(Train_res = NULL,
                          Target_res = NULL,
                          Extent = NULL, Dir = getwd(), Keep_Temporary = FALSE) {

  ### PREPARATION -----
  ## Extent
  if(class(Extent) == "Raster" | class(Extent) == "SpatialPolygonsDataFrame"){ # sanity check: ensure it is an Extent object
    if(class(Extent) == "SpatialPolygonsDataFrame"){ # shape check
      Shape <- Extent
    }# end of shape check
    Extent <- extent(Extent) # extract extent
  } # end of sanity check

  ## Downloading
  # Link to GMTED2010
  Link <- "https://edcintl.cr.usgs.gov/downloads/sciweb1/shared/topo/downloads/GMTED/Grid_ZipFiles/md30_grd.zip"

  ### DOWNLOADING & UNPACKING -----
  Dir.Data <- paste(Dir, "GMTED2010", sep ="/")
  dir.create(Dir.Data)
  if(!file.exists(paste0(Dir.Data, "/GMTED2010.zip"))){ # file check: check if file is not already downloaded
    download.file(Link, # product for donload
                  destfile = paste0(Dir.Data, "/GMTED2010.zip")) # destination file
  } # end of file check
  unzip(paste0(Dir.Data, "/GMTED2010.zip"), # which file to unzip
        exdir = Dir.Data) # where to unzip to

  ### RASTERIZING & CROPPING -----
  GMTED2010_ras <- raster(paste0(Dir.Data, "/md30_grd/w001001.adf")) # rasterising elevetation data
  if(!is.null(Extent)){ # cropping check
    GMTED2010_ras <- crop(GMTED2010_ras, Extent) # crop data
  } # end of cropping check

  ### MASKING ----
  if(exists("Shape")){ # Shape check
    GMTED2010_ras <- mask(GMTED2010_ras, Shape)
  }# end of Shape check

  ### RESAMPLING TO SPECIFIED RESOLUTIONS -----
  if(Target_res < res(GMTED2010_ras) | Train_res < res(GMTED2010_ras)){ # sanity check
    stop(paste0("You have specified Target_res, or Train_res to be finer than ", res(GMTED2010_ras), " (native GMTED2010 reslution). Please download higher-resolution DEM data instead."))
  } # end of sanity check

  res()

  GMTED2010Train_ras <- resample(x = GMTED2010_ras, y = Train_res)
  GMTED2010Target_ras <- resample()

  ### SAVING DATA ----

  ### REMOVE FILES FROM HARD DRIVE -----
  if(Keep_Temporary == FALSE){ # cleanup check
    for(Iter_download in 1:length(Links_ls)){ # variable loop: go over all HWSD variable
      unlink() ## UNLINKING!!
    } # end of variable loop
  }  # end of cleanup check


}


















































































