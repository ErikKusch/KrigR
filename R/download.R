#' Downloading ERA5(Land)-data from ECMWF servers
#'
#' This function breaks down download calls into monthly intervals, downloads ERA5(-Land) data from ECMWF servers according to user-specification, and fuses the downloaded files together according to user-demands. The actual time to download is dependent on ECMWF download queues. Users need an API key (https://cds.climate.copernicus.eu/api-how-to) to be set up.
#'
#' @param Variable ERA5(Land)-contained climate variable. See 'donwload' output of Variable_List() for possible values.
#' @param Type Whether to download reanalysis ('reanalysis') or ensemble ('ensemble_members', 'ensemble_mean', or 'ensemble_spread') data. Only available for era5 data.
#' @param DataSet Which ERA5 data set to download data from. 'era5' or 'era5-land'.
#' @param DateStart Date ('YYYY-MM-DD') at which to start time series of downloaded data.
#' @param DateStop Date ('YYYY-MM-DD') at which to stop time series of downloaded data.
#' @param TResolution Temporal resolution of final product. 'hour', 'day', 'month', or 'year'.
#' @param TStep Which time steps (numeric) to consider for temporal resolution. For example, specify bi-monthly data records by setting TResolution to 'month' and TStep to 2.
#' @param Extent Optional, download data according to rectangular bounding box. specify as extent() object or as a raster or a SpatialPolygonsDataFrame object. If Extent is a SpatialPolygonsDataFrame, this will be treated as a shapefile and the output will be cropped and masked to this shapefile.
#' @param Dir Directory specifying where to download data to.
#' @param FileName A file name for the netcdf produced. Default is a combination parameters in the function call.
#' @param API_Key ECMWF cds API key.
#' @param API_User ECMWF cds user number.
#' @return A raster object containing the downloaded ERA5(-Land) data, and a NETCDF (.nc) file in the specified directory.
#' @examples
#' \dontrun{
#' # Downloading ERA5-Land air temperature reanalysis data in 12-hour intervals for the entire year of 2000 for Germany. API User and Key in this example are non-functional. Substitute with your user number and key to run this example.
#' download_ERA(Variable = '2m_temperature', Type = 'reanalysis', DataSet = 'era5-land', DateStart = '2000-01-01', DateStop = '2000-12-31', TResolution = 'hour', TStep = 12, Extent = extent(6,15,47,55), API_User = NULL, API_Key = NULL)
#' }
#'
#' @export
download_ERA <- function(Variable = NULL, Type = "reanalysis", DataSet = "era5-land",
                         DateStart = "1981-01-01", DateStop = Sys.Date()-100,
                         TResolution = "month", TStep = 1, Extent = extent(-180,180,-90,90),
                         Dir = getwd(), FileName = NULL,
                         API_User = NULL, API_Key = NULL) {

  ### SETTING UP API ----
  # Setting the API key for later retrieval by wf_request()
  API_Service = "cds"
  wf_set_key(user = as.character(API_User),
             key = as.character(API_Key),
             service = API_Service)

  ### SETTING UP PARAMETERS FOR DOWNLOAD CALL ----
  # Extent Modifiers (needed for download product to produce square cells which are needed for Kriging to work)
  if(DataSet == "era5-land"){
    Grid <- ".1/.1" # era5-land-modifier
  }else{
    Grid <- ".5/.5" # era5-modifier
  }

  # Type (era5-land only provides reanalysis data and doesn't require a type argument, setting it to NA let's us ignore it further down the pipeline)
  if(DataSet == "era5-land"){ # product check
    Type <- NA # set Type to NA for later omission from request list when downloading era5-land data
  } # end of product check

  # Data Set (DataSet targeting in download calls is complicated and taken care of here)
  if(DataSet == "era5"){ # only append "single-levels" to era5 specification
    DataSet <- paste(DataSet, "single-levels", sep="-") # target reanalysis data sets of ECMWF servers
  }
  DataSet <- paste("reanalysis", DataSet, sep="-") # era5 family data sets must be adressed with "reanalysis-"
  if(TResolution != "hour" & TResolution != "day"){ # sub-daily check
    DataSet <- paste0(DataSet, "-monthly", "-means") # address monthly means
    if(Type != "reanalysis" & DataSet == "reanalysis-era5-single-levels-monthly-means"){ # ensemble check: if ensemble measures are requested
      Type <- paste0("monthly_averaged_", Type)
    }else{
      Type <- "monthly_averaged_reanalysis" # monthly averaged values are a product type that needs to be indexed for era5 and era5-land
    } # end of ensemble check
  } # end of subdaily check

  # Dates (this makes manipulation easier)
  DateStart <- as.Date(DateStart) # reformatting date
  DateStop <- as.Date(DateStop) # reformatting date
  Dates_seq <- seq(ymd(DateStart),ymd(DateStop), by = '1 day') # identify all days for which we need data
  Months_vec <- format(Dates_seq,'%Y-%m') # identify the YYYY-MM for each day
  Days_vec <- format(Dates_seq,'%d') # identify the day numbers in each month in each year
  n_calls <- length(unique(Months_vec)) # how many months we have in total
  Calls_ls <- as.list(rep(NA, n_calls)) # an empty list to be filled with date specifications
  for(Calls_Iter in 1:n_calls){ # calls loop: identify the dates to be called for each month in the sequence
    Calls_ls[[Calls_Iter]] <- c(gsub("-.*.", "", unique(Months_vec)[Calls_Iter]), # year
                                gsub(".*.-", "", unique(Months_vec)[Calls_Iter]), # month
                                min(Days_vec[which(Months_vec == unique(Months_vec)[Calls_Iter])]) : max(Days_vec[which(Months_vec == unique(Months_vec)[Calls_Iter])]) # days of that month
    )
  } # end of calls loop

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
  Extent <- try(paste(Extent[4], Extent[1], Extent[3], Extent[2], sep="/")) # break Extent object down into character

  # Time (set time for download depending on temporal resolution)
  if(TResolution == "hour" | TResolution == "day"){ # time check: if we need sub-daily data
    Times <- str_pad(str_c(0:23,"00",sep=":"), 5,"left","0")
  }else{ # if data intervals are monthly or bigger
    Times <- "00:00" # monthly averages are addressed with time stamp 00:00
  } # end of time check

  # FileName (generate automatic filename if none is specified by user)
  if(is.null(FileName)){
    FileName <- paste(Variable, DateStart, DateStop, TResolution, sep="_")
  }
  FileName <- strsplit(FileName, split =".nc") # remove .nc ending, if specified by user so that next line doesn't end up with a file ending of ".nc.nc"
  FileName <- paste0(FileName, ".nc") # adding netcdf ending to file name
  FileNames_vec <- paste0(str_pad(1:n_calls, 4, "left", "0"), "_", FileName) # names for individual downloads
  ### BUILDING REQUEST ----
  # Setting parameters of desired downloaded netcdf file according to user input
  for(Downloads_Iter in 1:n_calls){
    Request_ls <- list("dataset_short_name" = DataSet,
                       "product_type"   = Type,
                       "variable"       = Variable,
                       "year"           = Calls_ls[[Downloads_Iter]][1],
                       "month"          = Calls_ls[[Downloads_Iter]][2],
                       "day"            = Calls_ls[[Downloads_Iter]][3:length(Calls_ls[[Downloads_Iter]])],
                       "time"           = Times,
                       "area"           = Extent,
                       "format"         = "netcdf",
                       "target"         = FileNames_vec[Downloads_Iter],
                       "grid"           = Grid)

    ### EXECUTING REQUEST ----
    wf_request(user = as.character(API_User),
               request = Request_ls,
               transfer = TRUE,
               path = Dir,
               verbose = TRUE,
               time_out = 36000)
  }

  ### LOAD DATA BACK IN ----
  Files_vec <- file.path(Dir, list.files(Dir, pattern = FileName)) # all files belonging to this query with folder paths
  if(is.na(Type)){Type <- "reanalysis"} # na Type is used for Era5-land data which is essentially just reanalysis
  if(Type == "ensemble_members"){ # ensemble_member check: if user downloaded ensemble_member data
    Layers <- 1:10 # ensemble members come in 10 distinct layers
  }else{
    Layers <- 1 # non-ensemble members have 1 layer for each time step instead of 10
    ### ERROR CHECK (CDS sometimes produces a netcdf with two layers and break point in the data being assigned to the first and then the second layer in non-ensemble members. this step fixes this) ----
    for(Layers_Check in 1:length(Files_vec)){
      LayersSame <- suppressWarnings(all.equal(brick(Files_vec[Layers_Check], level = 1), brick(Files_vec[Layers_Check], level = 2))) # Check if the layers are the same in brick loading
      if(LayersSame == FALSE){
        Era5_ras <- brick(file.path(Dir, "/", Files_vec[Layers_Check]), level = 1) # load initial data again for just the first band
        Era5_ras2 <- brick(file.path(Dir, "/", Files_vec[Layers_Check]), level = 2) # load second layer
        Sums_vec <- NA # recreate sum vector
        for(Iter_Check in 1:nlayers(Era5_ras2)){ # layer loop: go over all layers in Era5_ras2
          Sums_vec <- c(Sums_vec, sum(values(Era5_ras2[[Iter_Check]]), na.rm = TRUE)) # append sum of data values to sum vector, layers with issues will produce a 0
        } # end of layer loop
        Sums_vec <- na.omit(Sums_vec) # omit initial NA
        StopFirst <- min(which(Sums_vec != 0)) # identify the last layer of the brick that is problematic on the second data layer loaded above
        Era5_ras <- stack(Era5_ras[[1:(StopFirst-1)]], Era5_ras2[[StopFirst:nlayers(Era5_ras2)]]) # rebuild the Era5_ras stack as a combination of the data-containing layers in the two bricks
        writeRaster(Era5_ras, filename = Files_vec[Layers_Check])
      }
    }
  } # end of ensemble_member check

  ### loop over files and layers here
  Era5_ls <- as.list(rep(NA, length(Layers))) # list for layer aggregation
  for(LoadIter in Layers){
    ERA5_ls <- as.list(rep(NA, n_calls)) # list for layer aggregation
    for(LOADIter in 1:n_calls){
      ERA5_ls[[LOADIter]] <- raster::brick(x = Files_vec[LOADIter], level = Layers[[LoadIter]]) # loading the data
    }
    Era5_ras <- stack(ERA5_ls)
    # Files_vec <- c(Files_vec, file.path(Dir, paste0(FileName, "_Stacked_", LoadIter)))
    # writeRaster(Era5_ras, filename = Files_vec[LoadIter+n_calls], format = "CDF")

    ### DAY/YEAR MEANS ----
    if(TResolution == "day" | TResolution == "year"){ # day/year check: need to build averages for days (from hours) and years (from months)
      if(TResolution == "day"){ # daily means
        if(Type == "reanalysis"){
          factor <- 24 # number of hours per day in reanalysis data
        }else{
          factor <- 8 # ensemble data only has 8 time steps in a day
        }
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
      Era5_ras <- mask(Era5_ras, Shape) # mask if shapefile was provided
    }# end of Shape check
    ### LIST SAVING
    Era5_ls[[LoadIter]] <- Era5_ras
  }
  Era5_ras <- brick(Era5_ls)
  if(Type == "ensemble_members"){ ## fixing indices of layers for ensemble means
    Indices <- sub(pattern = ".*\\_", replacement = "", names(Era5_ras))
    Indices2 <- strsplit(x = Indices, split = ".", fixed = TRUE)
    Indices3 <- str_pad(unlist(Indices2), 3, "left","0")
    PairNumbers <- rep(1:(length(Indices3)/2), each = 2)
    Indices4 <- paste(Indices3[which(PairNumbers == 1)], collapse = "")
    for(IndicesIter in 2:(length(Indices3)/2)){
      Indices4 <- c(Indices4, paste(Indices3[which(PairNumbers == IndicesIter)], collapse = ""))
    }
    Indices4 <- as.numeric(Indices4)
    Era5_ras <- Era5_ras[[order(Indices4)]]
  }
  ### SAVING DATA ----
  writeRaster(x = Era5_ras, filename = file.path(Dir, FileName), overwrite = TRUE, format="CDF", varname = Variable)
  unlink(Files_vec, recursive = TRUE)
  return(Era5_ras)
}

#' Downloading DEM data from USGS servers
#'
#' This function downloads and rescales the median statistic of the Global Multi-resolution Terrain Elevation Data (GMTED2010) data from the servers of the U.S. Geological Survey (USGS) available at \url{https://topotools.cr.usgs.gov/gmted_viewer/gmted2010_global_grids.php}. The data is downloaded at 30 arc-sec latitude/longitude grid cells and subsequently resampled to match Train_ras and Target_res. This data is the default for kriging within this package.
#'
#' @param Train_ras A raster file containing the data which is to be downscaled. GMTED2010 data is then resampled to match this.
#' @param Target_res The target resolution for the kriging step (i.e. wich resolution to downscale to). An object as specified/produced by raster::res() or a single number (GMTED2010 data will be aggregated) or a raster which the data should be comparable to after kriging (GMTED2010 data will be resampled).
#' @param Shape Optional, a SpatialPolygonsDataFrame object. This will be treated as a shapefile and the output will be masked to this shapefile.
#' @param Dir Directory specifying where to download data to.
#' @param Keep_Temporary Logical, whether to delete individual, global, 30 arc-sec files or keep them to be reused in later analyses.
#' @return A list containing two raster object ready to be used as covariates for kriging, and two NETCDF (.nc) files in the specified directory.
#' @examples
#' \dontrun{
#' # Downloading GMTED2010-data at resolution and extent obtained by a call to download_ERA and a target resolution of .01.
#' download_HWSD(Train_ras = download_ERA(...), Target_res = 0.01)
#' }
#'
#' @export
download_DEM <- function(Train_ras = NULL,
                         Target_res = NULL,
                         Shape = NULL, Dir = getwd(), Keep_Temporary = FALSE) {

  ### PREPARATION -----
  Extent <- extent(Train_ras) # extract extent for later cropping
  Link <- "https://edcintl.cr.usgs.gov/downloads/sciweb1/shared/topo/downloads/GMTED/Grid_ZipFiles/mn30_grd.zip" # Link to GMTED2010
  # handling Target_res
  ## distinguishing if Target_res is a raster or a resolution, this will change whether GMTED2010 is aggregated or resampled
  if(class(Target_res[[1]]) == "RasterLayer"){
    Target_ras <- Target_res
    Target_res <- res(Target_ras)
  }

  ### DOWNLOADING & UNPACKING -----
  Dir.Data <- file.path(Dir, "GMTED2010") # identify folder for GMTED2010 data
  if(!file.exists(file.path(Dir.Data, "GMTED2010.zip"))){ # file check: check if file is not already downloaded
    dir.create(Dir.Data) # create folder for GMTED2010 data
    print("Downloading GMTED2010 covariate data.") # inform user of download in console
    download.file(Link, # product for donload
                  destfile = file.path(Dir.Data, "GMTED2010.zip")) # destination file
    unzip(file.path(Dir.Data, "GMTED2010.zip"), # which file to unzip
          exdir = Dir.Data) # where to unzip to
  } # end of file check

  ### RASTERIZING & CROPPING -----
  GMTED2010_ras <- raster(file.path(Dir.Data, "mn30_grd/w001001.adf")) # rasterising elevetation data
  GMTED2010_ras <- crop(GMTED2010_ras, Extent) # crop data

  ### RESAMPLING TO SPECIFIED RESOLUTIONS -----
  if(Target_res[1] < res(GMTED2010_ras)[[1]] |
     res(Train_ras)[1] < res(GMTED2010_ras)[1] | Target_res[1] < res(GMTED2010_ras)[1]){ # sanity check
    stop(paste0("You have specified resolution(s) to be finer than ", res(GMTED2010_ras), " (native GMTED2010 reslution). Please download higher-resolution DEM data instead."))
  } # end of sanity check
  # resampling training data
  GMTED2010Train_ras <- resample(GMTED2010_ras, Train_ras)
  # resampling target data
  if(exists("Target_ras")){
    GMTED2010Target_ras <- resample(GMTED2010_ras, Target_ras) # resample if output raster was given
  }else{
    GMTED2010Target_ras <- suppressWarnings(aggregate(GMTED2010_ras, fact = Target_res[1]/res(GMTED2010_ras)[1])) # aggregate if output resolution was given
  }

  ### MASKING ----
  if(!is.null(Shape)){ # Shape check
    GMTED2010Train_ras <- mask(GMTED2010Train_ras, Shape)
    GMTED2010Target_ras <- mask(GMTED2010Target_ras, Shape)
  } # end of Shape check

  ### SAVING DATA ----
  names(GMTED2010Train_ras) <- c("DEM") # setting layer name for later use in KrigingEquation
  names(GMTED2010Target_ras) <- c("DEM") # setting layer name for later use in KrigingEquation
  writeRaster(x = GMTED2010Train_ras, filename = file.path(Dir, "GMTED2010_Train.nc"), overwrite = TRUE, format="CDF")
  writeRaster(x = GMTED2010Target_ras, filename = file.path(Dir, "GMTED2010_Target.nc"), overwrite = TRUE, format="CDF")

  ### REMOVE FILES FROM HARD DRIVE -----
  if(Keep_Temporary == FALSE){ # cleanup check
    unlink(Dir.Data, recursive = TRUE)
  }  # end of cleanup check

  return(list(GMTED2010Train_ras, GMTED2010Target_ras))
}
