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
#' @param Extent Optional, download data according to rectangular bounding box. Specify as "ymax/xmin/ymin/xmax" (a global extent would read "90/-180/-90/180"). Alternatively, an Extent object obtained via raster::extent().
#' @param Dir Directory specifying where to download data to.
#' @param FileName A file name for the netcdf produced. Default is a combination parameters in the function call.
#' @param API_Key ECMWF cds API key
#' @param API_User ECMWF cds user number
#' @return A NETCDF (.nc) file in the specified directory as well as a raster object containing the specified data.
#' @examples
#' \dontrun{
#' # Downloading ERA5-Land air temperature reanalysis data in
#' # 12-hour intervals for the entire year of 1951 for the entire earth.
#' download_ERA(Variable = '2m_temperature', Measure = 'reanalysis', DataSet = 'era5-land',
#' DateStart = '2000-01-01', DateStop = '2000-12-31',
#' TResolution = 'hour', TStep = 12, API_User = 12345, API_Key = 1234567891012345678910)
#' }
#'
download_ERA <- function(Variable = NULL, Type = "reanalysis", DataSet = "era5-land",
                         DateStart = "1979-01-01", DateStop = Sys.Date(),
                         TResolution = "Month", TStep = 1, Extent = "90/-180/-90/180",
                         Dir = getwd(), FileName = NULL,
                         API_User = NULL, API_Key = NULL) {
  # Placeholder for real list!
  Variables_ls <- data.frame(ClearName = "2m temperature",
                            DownloadName = '2m_temperature')


  ### sETTING UP API ----
  # Setting the API key for later retrieval by wf_request()
  API_Service = "cds"
  wf_set_key(user=as.character(API_User),
             key=as.character(API_Key),
             service=as.character(API_Service))

  ### HANDLING USER INPUT ----
  # # Variable
  # if(Variable %in% Variables_ls$ClearName){ # sanity check: whether variable is available
  #   Variable <- as.character(Variables_ls$DownloadName[which(Variables_ls$ClearName == Variable)]) # return the download name as new Variable argument
  # }else{ # if sanity check is failed, stop function and inform user
  #   stop("The variable you specified is not available as per the variable list. See Variable_List() for further information.")
  # } # end of sanity check

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
    if(class(Extent) == "Extent"){ # sanity check: ensure it is an Extent object if not character
      Extent <- try(paste(Extent[4], Extent[1], Extent[3], Extent[2], sep="/")) # break Extent opbject down into character
    }else{ # if sanity check is failed, stop function and inform user
      stop('The Extent argument provided by you is neither formatted as a character of "xmin/xmax/ymin/ymax" nor an Extent object derived via raster::extent(). Please correct this.')
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

  ### SAVING DATA ----
  retunr(Era5_ras)
  writeRaster(x = Era5_ras, filename = paste0(Dir, "/", FileName, ".nc"))
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
