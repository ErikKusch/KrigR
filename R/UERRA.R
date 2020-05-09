#' Downloading UERRA data from ECMWF servers
#'
#' This function downloads UERRA data from ECMWF servers according to user-specification. The actual time to download is dependant on ECMWF download queues. Users need an API key (https://cds.climate.copernicus.eu/api-how-to) to be set up.
#'
#' @param Variable UERRA-contained climate variable. See output of Variable_List('uerra') for possible values.
#' @param DataSet Optional for most variables. Which UERRA data set to download data from. 'single-levels', 'soil-levels', 'height-levels', or 'pressure-levels'.
#' @param Origin Which origin system to use. 'uerra_harmonie' or 'mescan_surfex'. This is largely dictated by choice of variable and does influence spatial resolution of the final product.
#' @param Level Level of target variable. Required when DataSet is 'soil-levels', 'height-levels', or 'pressure-levels'
#' @param DateStart Date ('YYYY-MM-DD') at which to start time series of downloaded data.
#' @param DateStop Date ('YYYY-MM-DD') at which to stop time series of downloaded data.
#' @param TResolution Temporal resolution of final product. 'hour', 'day', 'month', or 'year'.
#' @param TStep Which time steps (numeric) to consider for temporal resolution. For example, specify bi-monthly data records by setting TResolution to 'month' and TStep to 2.
#' @param Extent Optional, download data according to rectangular bounding box. specify as extent() object or as a raster or a SpatialPolygonsDataFrame object. If Extent is a SpatialPolygonsDataFrame, this will be treated as a shapefile and the output will be cropped and masked to this shapefile. Note that UERRA data is limited to Europe.
#' @param Dir Directory specifying where to download data to.
#' @param FileName A file name for the netcdf produced. Default is a combination parameters in the function call.
#' @param API_Key ECMWF cds API key.
#' @param API_User ECMWF cds user number.
#' @return A raster object containing the downloaded UERRA data, and a NETCDF (.nc) file in the specified directory as well as a raster object containing the specified data.
#' @examples
#' \dontrun{
#' # Downloading UERRA (uerra_harmonie) temperature data in 15m height in 6-hour intervals for the entire year of 2000 for Germany. API User and Key in this example are non-functional. Substitute with your user number and key to run this example.
#' download_UERRA(Variable = 'temperature', DataSet = 'height-levels', Origin = 'uerra_harmonie', Level = 15, DateStart = '2000-01-01', DateStop = '2000-12-31', TResolution = 'hour', TStep = 6, Extent = extent(6,15,47,55), API_User = NULL, API_Key = NULL)
#' }
#'
#' @export
download_UERRA <- function(Variable = NULL, DataSet = NULL, Origin = "mescan_surfex", Level = NULL,
                           DateStart = "1961-01-01", DateStop = "2019-07-31",
                           TResolution = "hour", TStep = 6, Extent = NULL,
                           Dir = getwd(), FileName = NULL,
                           API_User = NULL, API_Key = NULL) {

  ### SETTING UP API ----
  # Setting the API key for later retrieval by wf_request()
  API_Service = "cds"
  wf_set_key(user=as.character(API_User),
             key=as.character(API_Key),
             service=as.character(API_Service))

  ### SANITY CHECKS ----
  # Dates (this makes manipulation easier)
  DateStart <- as.Date(DateStart) # reformatting date
  DateStop <- as.Date(DateStop) # reformatting date

  if(format(DateStart, "%y") != format(DateStop, "%y") & format(DateStart, "%m") != 1 & format(DateStop, "%m") != 12){
    stop("Unfortunately, when specifying the download of time series spanning across year breaks, I am unable to download anything but full year sets. Please set DateStart to 'YOURSTARTYEAR-01-01' and DateStop to 'YOURSTOPYEAR-12-31'. You may then limit the scope of the downloaded and processed data as you see fit.")
  }

  # DataSet Identification
  if(is.null(DataSet)){ # DataSet check: try to guess dataset according to user-specified variable
    DataSet <- as.character(Variable_List("uerra")$DataSet[which(Variable_List("uerra")$Download == Variable)]) # read out data set
  } # end of DataSet check

  # Multiple Data Set Targeting
  DataSet <- strsplit(DataSet, split ="/")[[1]] # attempt to split DataSet name at "/"
  # stop code if multiple data sets are possible and the user hasn't specified one so far
  if(length(DataSet) > 1){stop("The variable you have specified is available in more than one UERRA data set. Please specify which data set you want to obtain the data from in the function call to donwload_UERRA().")}

  # Origin checking
  if(is.na(Variable_List("uerra")$Origin[which(Variable_List("uerra")$Download == Variable)])){
    Origin <- NA
  }else{
    if(Variable_List("uerra")$Origin[which(Variable_List("uerra")$Download == Variable)] != "mescan_surfex/uerra_harmonie"){ # origin check: if there is only one origin available for the data
      Origin <- Variable_List("uerra")$Origin[which(Variable_List("uerra")$Download == Variable)] # assign that one possible origin irrespective of user input
    } # end of origin check
  }

  # Level checking
  soil_level <- NA
  if(DataSet == "soil-levels"){ # soil layer check
    if(is.null(Level)){
      stop(paste("You have not specified a soil level for which to obtain data. Possible levels are", paste(1:14, collapse = ", "))) # throw error and point user to possible levels
    }
    soil_level <- Level
    if(Variable_List("uerra")$Origin[which(Variable_List("uerra")$Download == Variable)] == "uerra_harmonie" & soil_level > 3){
      stop("uerra_harmonie soil levels only allow numbers 1, 2, and 3.")
    }
  } # end of soil layer check
  pressure_level <- NA
  if(DataSet == "pressure-levels"){ # pressure layer check
    if(is.null(Level)){
      stop(paste("You have not specified a pressure level for which to obtain data. Possible levels are 10", "20", "30", "50", "70", "100", "150", "200", "250", "300", "400", "500", "600", "700", "750", "800", "825", "850", "875", "900", "925", "950", "975", "1000", sep=", ")) # throw error and point user to possible levels
    }
    pressure_level <- Level
  } # end of pressure layer check
  height_level <- NA
  if(DataSet == "height-levels"){ # height layer check
    if(is.null(Level)){
      stop(paste("You have not specified a height level for which to obtain data. Possible levels are 15_m",
                 "30_m", "50_m", "75_m", "100_m", "150_m", "200_m", "250_m", "300_m", "400_m", "500_m", sep=", ")) # throw error and point user to possible levels
    }
    height_level <- Level
  } # end of height layer check

  # Temporal Resolution
  if(TResolution == "hour" & TStep%%6!=0){ # temporal resolution check: if user has defined a temporal resolution that can't be achieved with UERRA
    stop("You have specified an hourly temporal resolution in steps (TStep) that are not supported by UERRA which only contains hourly records in 6 hours intervals. Please alter your TStep argument to be a multiple of 6.")
  } # end of temporal resolution check

  ### SETTING UP PARAMETERS FOR DOWNLOAD CALL ----
  # Data Set (DataSet targeting in download calls is complicated and taken care of here)
  DataSet <- paste("reanalysis-uerra-europe", DataSet, sep="-")

  # Years (identify which years the user is targeting)
  Years <- as.character(format(DateStart,'%Y'):format(DateStop,'%Y'))

  # Months (identify which months the user is targeting)
  if(format(DateStart,'%m') <= format(DateStop,'%m')){ # if beginning month is earlier than stopping month in annual sequence
    Months <- str_pad(as.character(format(DateStart,'%m'):format(DateStop,'%m')), 2, "left","0") # set Months to sequence of months between dates
  }else{ # beginning month is later than stopping month in annual sequence
    Months <- str_pad(as.character(rev(format(DateStart,'%m'):format(DateStop,'%m'))), 2, "left","0") # invert month order
  } # end of month check

  # Days (identify which days the user is targeting)
  if(format(DateStart,'%m') == format(DateStop,'%m') & format(DateStart,'%Y') == format(DateStop,'%Y')){ # equality check: if time series does not exceed one month
    Days <- str_pad(as.character(format(DateStart,'%d'):format(DateStop,'%d')), 2, "left","0") # list all days between and including start and stop days
  }else{ # if time spans more than one month or overlaps months
    Days <- str_pad(1:31,2,"left","0") # full array of days for months
  } # end of equality check

  # Extent (prepare rectangular bounding box for download from user input)
  if(class(Extent) == "Raster" | class(Extent) == "SpatialPolygonsDataFrame"){ # sanity check: ensure it is a raster of Spatialpolygonsdataframe object if not an extent object
    if(class(Extent) == "SpatialPolygonsDataFrame"){ # shape check
      Shape <- Extent # save the shapefile for later masking
    } # end of shape check
    Extent <- extent(Extent) # extract extent
  } # end of sanity check
  if(class(Extent) != "Extent"){ # Extent check: whether already specified as Extent object
    stop('The Extent argument provided by you is neither formatted as an Extent nor a Raster or SpatialPolygonsDataFrame object. Please correct this.')
  } # # end of Extent check
  Extent <- extent(Extent) # extract extent

  # Time (set time for download depending on temporal resolution)
  Times <- str_pad(str_c(c(0,6,12,18),"00",sep=":"), 5,"left","0")

  # FileName (generate automatic filename if none is specified by user)
  if(is.null(FileName)){
    FileName <- paste(Variable, DateStart, DateStop, TResolution, sep="_")
  }
  FileName <- strsplit(FileName, split =".nc") # remove .nc ending, if specified by user so that next line doesn't end up with a file ending of ".nc.nc"
  FileName <- paste0(FileName, ".nc") # adding netcdf ending to file name

  ### BUILDING REQUEST ----
  # Setting parameters of desired downloaded netcdf file according to user input
  Request_ls <- list("dataset"        = DataSet,
                     "origin"         = Origin,
                     "soil_level"     = soil_level,
                     "pressure_level" = pressure_level,
                     "height_level"   = height_level,
                     "variable"       = Variable,
                     "year"           = Years,
                     "month"          = Months,
                     "day"            = Days,
                     "time"           = Times,
                     "format"         = "netcdf",
                     "target"         = paste0(FileName))

  ### EXECUTING REQUEST ----
  wf_request(user = as.character(API_User),
             request = Request_ls,
             transfer = TRUE,
             path = Dir,
             verbose = TRUE,
             time_out = 36000)

  ### LOAD DATA BACK IN ----
  invisible(capture.output(LayersSame <- suppressWarnings(all.equal(brick(file.path(Dir, "/", FileName), level = 1), brick(file.path(Dir, "/", FileName), level = 2))))) # Check if the layers are the same in brick loading

  ### ERROR CHECK (CDS sometimes produces a netcdf with two layers and break oint in the data being assigned to the first and then the second layer. this step fixes this) ----
  if(LayersSame == FALSE){ # problem check: if we were able to load a second layer from the data
    invisible(capture.output(Era5_ras <- brick(file.path(Dir, "/", FileName), level = 1))) # load initial data again for just the first band
    invisible(capture.output(Era5_ras2 <- brick(file.path(Dir, "/", FileName), level = 2))) # load second layer
    Sums_vec <- NA # recreate sum vector
    for(Iter_Check in 1:nlayers(Era5_ras2)){ # layer loop: go over all layers in Era5_ras2
      Sums_vec <- c(Sums_vec, sum(values(Era5_ras2[[Iter_Check]]), na.rm = TRUE)) # append sum of data values to sum vector, layers with issues will produce a 0
    } # end of layer loop
    Sums_vec <- na.omit(Sums_vec) # omit initial NA
    StopFirst <- min(which(Sums_vec != 0)) # identify the last layer of the brick that is problematic on the second data layer loaded above
    invisible(capture.output(Era5_ras <- stack(Era5_ras[[1:(StopFirst-1)]], Era5_ras2[[StopFirst:nlayers(Era5_ras2)]]))) # rebuild the Era5_ras stack as a combination of the data-containing layers in the two bricks
  }else{ # if there is no double layer issue
    invisible(capture.output(Era5_ras <- stack(file.path(Dir, FileName)))) # loading the data
  } # end of problem check

  ### FIXING THE EXTENT OF THE ENTIRE EUROPE REGION
  if(Origin == "uerra_harmonie" | is.na(Origin)){ # for all uerra-harmonie origin data
    extent(Era5_ras) <- extent(-60.404, 76.4047, 17.6118, 75.4842)
  }else{ # for all mescan surfex data
    Era5_ras <- flip(Era5_ras, direction = 2) # needs to be flipped horizontally
    extent(Era5_ras) <- extent(-58.1048, 74.1044, 20.2920, 75.3465)
  }
  crs(Era5_ras) <- "+proj=lcc +lat_1=33 +lat_2=45 +lat_0=40 +lon_0=-97 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs" # lambert conformal crs of UERRA

  ### DAY/YEAR MEANS ----
  if(TResolution == "day"){ # day check
    factor <- 4 # number of 6-hour intervals per day
    Index <- rep(1:(nlayers(Era5_ras)/factor), each = factor) # build an index
    Era5_ras <- stackApply(Era5_ras, Index, fun='mean') # do the calculation
  }# end of day check

  if(TResolution == "month"){ # year check
    Day_seq <- rep(seq.Date(from = DateStart, to = DateStop, by = "days"), each = 4)
    Index <- format(Day_seq, "%Y%m")
    Index <- rep(1:length(unique(Index)), times = as.numeric(table(Index)))
    Era5_ras <- stackApply(Era5_ras, Index, fun='mean') # do the calculation
  } # end of month check

  if(TResolution == "year"){ # year check
    Mon_seq <- rep(seq.Date(from = DateStart, to = DateStop, by = "days"), each = 4)
    Index <- format(Mon_seq, "%Y")
    Index <- rep(1:length(unique(Index)), times = as.numeric(table(Index)))
    Era5_ras <- stackApply(Era5_ras, Index, fun='mean') # do the calculation
  } # end of year check

  ### TIME STEP MEANS ----
  if(nlayers(Era5_ras)%%TStep != 0){ # sanity check for completeness of time steps and data
    warning(paste0("Your specified time range does not allow for a clean integration of your selected time steps. Only full time steps will be computed. You specified a time series with a length of ", nlayers(Era5_ras), "(", TResolution,") and time steps of ", TStep, ". This works out to ", nlayers(Era5_ras)/TStep, " intervals. You will receive ", floor(nlayers(Era5_ras)/TStep), " intervals."))
  }# end of sanity check for time step completeness
  Index <- rep(1:(nlayers(Era5_ras)/TStep), each = TStep) # build an index
  Era5_ras <- stackApply(Era5_ras[[1:length(Index)]], Index, fun='mean') # do the calculation

  ### MASKING ----
  if(exists("Shape")){ # Shape check
    Era5_ras <- crop(Era5_ras, Extent)
    Era5_ras <- mask(Era5_ras, Shape) # mask if shapefile was provided
  }# end of Shape check

  ### SAVING DATA ----
  writeRaster(x = Era5_ras, filename = file.path(Dir, FileName), overwrite = TRUE, format="CDF")
  return(Era5_ras)
}
