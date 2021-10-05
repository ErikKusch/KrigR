#' Downloading ERA5(Land)-data from ECMWF servers
#'
#' This function breaks down download calls into monthly intervals, downloads ERA5(-Land) data from ECMWF servers according to user-specification, and fuses the downloaded files together according to user-demands. The actual time to download is dependent on ECMWF download queues. Users need an API key (https://cds.climate.copernicus.eu/api-how-to) to be set up.
#'
#' @param Variable ERA5(Land)-contained climate variable. See 'download' output of Variable_List() for possible values.
#' @param PrecipFix Logical. Era5(-land) total precipitation is recorded in cumulative steps per hour from the 00:00 time mark per day. Setting PrecipFix to TRUE converts these into records which represent the total precipitation per hour. Monthly records in Era5(-land) express the average daily total precipitation. Setting this argument to TRUE multiplies monthly records by the number of days per the respective month(s) to get to total precipitation records instead of average.  Default is FALSE.
#' @param Type Whether to download reanalysis ('reanalysis', 'monthly_averaged_reanalysis_by_hour_of_day') or ensemble ('ensemble_members', 'ensemble_mean', or 'ensemble_spread') data. Only available for era5 data.
#' @param DataSet Which ERA5 data set to download data from. 'era5' or 'era5-land'.
#' @param DateStart Date ('YYYY-MM-DD') at which to start time series of downloaded data.
#' @param DateStop Date ('YYYY-MM-DD') at which to stop time series of downloaded data.
#' @param TResolution Temporal resolution of final product. 'hour', 'day', 'month', or 'year'.
#' @param TStep Which time steps (numeric) to consider for temporal resolution. For example, specify bi-monthly data records by setting TResolution to 'month' and TStep to 2.
#' @param FUN A raster calculation argument as passed to `raster::stackApply()`. This controls what kind of data to obtain for temporal aggregates of reanalysis data. Specify 'mean' (default) for mean values, 'min' for minimum values, and 'max' for maximum values, among others.
#' @param Extent Optional, download data according to rectangular bounding box. specify as extent() object or as a raster, a SpatialPolygonsDataFrame object, or a data.frame opbject. If Extent is a SpatialPolygonsDataFrame, this will be treated as a shapefile and the output will be cropped and masked to this shapefile. If Extent is a data.frame of geo-referenced point records, it needs to contain Lat and Lon columns as well as a non-repeating ID-column.
#' @param Buffer Optional. Identifies how big a rectangular buffer to draw around points if Extent is a data frame of points. Buffer is expressed as centessimal degrees.
#' @param ID Optional. Identifies which column in Extent to use for creation of individual buffers if Extent is a data.frame.
#' @param Dir Directory specifying where to download data to.
#' @param FileName A file name for the netcdf produced. Default is a combination parameters in the function call.
#' @param API_Key Character; ECMWF cds API key.
#' @param API_User Character; ECMWF cds user number.
#' @param TryDown Optional, numeric. How often to attempt the download of each individual file that the function queries from the server. This is to circumvent having to restart the entire function when encountering connectivity issues.
#' @param verbose Optional, logical. Whether to report progress of the function in the console or not.
#' @param Cores Numeric. How many cores to use.^This can speed up downloads of long time-series. If you want output to your console during the process, use Cores = 1. Parallel processing is carried out when Cores is bigger than 1. Default is 1.
#' @param TimeOut Numeric. The timeout for each download in seconds. Default 36000 seconds (10 hours).
#' @return A raster object containing the downloaded ERA5(-Land) data, and a NETCDF (.nc) file in the specified directory.
#' @examples
#' \dontrun{
#' # Downloading ERA5-Land air temperature reanalysis data in 12-hour intervals for 02/01/1995 - 04/01/1995 (DD/MM/YYYY). API User and Key in this example are non-functional. Substitute with your user number and key to run this example.
#' Extent <- extent(11.8,15.1,50.1,51.7) # roughly the extent of Saxony
#' API_User <- "..."
#' API_Key <- "..."
#' State_Raw <- download_ERA(
#' Variable = "2m_temperature",
#' DataSet = "era5-land",
#' DateStart = "1995-01-02",
#' DateStop = "1995-01-04",
#' TResolution = "hour",
#' TStep = 12,
#' Extent = Extent,
#' API_User = API_User,
#' API_Key = API_Key
#' )
#' State_Raw # a raster brick with 6 layers at resolution of ~0.1°
#' }
#'
#' @export
download_ERA <- function(Variable = NULL, PrecipFix = FALSE, Type = "reanalysis", DataSet = "era5-land",
                         DateStart = "1981-01-01", DateStop = Sys.Date()-100,
                         TResolution = "month", TStep = 1, FUN = 'mean',
                         Extent = extent(-180,180,-90,90), Buffer = 0.5, ID = "ID",
                         Dir = getwd(), FileName = NULL,
                         API_User = NULL, API_Key = NULL, TryDown = 10, verbose = TRUE, Cores = 1, TimeOut = 36000) {

  if(isTRUE(verbose)){print("donwload_ERA() is starting. Depending on your specifications, this can take a significant time.")}

  ### PrecipFix Mispecification Check ----
  if(PrecipFix == TRUE & Variable != "total_precipitation"){
    stop("You cannot specify PrecipFix = TRUE without calling on Variable = total_precipitation")
  }

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
  TypeOrigin <- Type # save original type input
  if(DataSet == "era5-land"){ # product check
    Type <- NA # set Type to NA for later omission from request list when downloading era5-land data
  } # end of product check

  # Data Set (DataSet targeting in download calls is complicated and taken care of here)
  if(DataSet == "era5"){ # only append "single-levels" to era5 specification
    DataSet <- paste(DataSet, "single-levels", sep="-") # target reanalysis data sets of ECMWF servers
  }
  DataSet <- paste("reanalysis", DataSet, sep="-") # era5 family data sets must be adressed with "reanalysis-"
  if(length(grep(DataSet, pattern = "preliminary")) != 1){ # do not change preliminary data calls according to monthly means
    if(TResolution != "hour" & TResolution != "day"){ # sub-daily check
      DataSet <- paste0(DataSet, "-monthly", "-means") # address monthly means
      if(Type != "reanalysis" & DataSet == "reanalysis-era5-single-levels-monthly-means"){ # ensemble check: if ensemble measures are requested
        Type <- paste0("monthly_averaged_", Type)
      }else{
        Type <- "monthly_averaged_reanalysis" # monthly averaged values are a product type that needs to be indexed for era5 and era5-land
      } # end of ensemble check
    } # end of subdaily check
  } # end of preliminary check
  if(TypeOrigin == "monthly_averaged_reanalysis_by_hour_of_day"){
    Type <- TypeOrigin
  }

  # Dates (this makes manipulation easier)
  DateStart <- as.Date(DateStart) # reformatting date
  if(PrecipFix == TRUE & TResolution == "day" | PrecipFix == TRUE & TResolution == "hour"){
    DateStop <- as.Date(DateStop)+1 # reformatting date
  }else{
    DateStop <- as.Date(DateStop) # reformatting date
  }

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

  # Extent vs. Shapefile vs. Points
  if(class(Extent) == "data.frame"){ # if we have been given point data
    Extent <- buffer_Points(Points = Extent, Buffer = Buffer, ID = ID)
  }

  # Extent (prepare rectangular bounding box for download from user input)
  if(class(Extent) == "Raster" | class(Extent) == "SpatialPolygonsDataFrame" | class(Extent) == "SpatialPolygons"){ # sanity check: ensure it is a raster of Spatialpolygonsdataframe object if not an extent object
    if(class(Extent) == "SpatialPolygonsDataFrame" | class(Extent) == "SpatialPolygons"){ # shape check
      Shape <- Extent # save the shapefile for later masking
    } # end of shape check
    Extent <- extent(Extent) # extract extent
  } # end of sanity check
  if(class(Extent) != "Extent"){ # Extent check: whether already specified as Extent object
    stop('The Extent argument provided by you is neither formatted as an Extent nor a Raster or SpatialPolygonsDataFrame object. Please correct this.')
  } # # end of Extent check
  Modifier <- as.numeric(strsplit(Grid, "/")[[1]][1]) # for widening the extent to ensure full coverage of shapefile
  Extent <- try(paste(Extent[4]+Modifier, Extent[1]-Modifier, Extent[3]-Modifier, Extent[2]+Modifier, sep="/")) # break Extent object down into character
  # Extent global extent sanity check
  Corner_vec <- as.numeric(unlist(strsplit(Extent, "/")))
  Musts_vec <- c(90, -180, -90, 180)
  for(Iter_Corners in 1:length(Corner_vec)){
    if(abs(Corner_vec[Iter_Corners]) > abs(Musts_vec[Iter_Corners])){
      Corner_vec[Iter_Corners] <- Musts_vec[Iter_Corners]
    }
  }
  Extent <- try(paste(Corner_vec, collapse="/")) # break Extent object down into character

  # Time (set time for download depending on temporal resolution)
  if(TResolution == "hour" | TResolution == "day"){ # time check: if we need sub-daily data
    Times <- str_pad(str_c(0:23,"00",sep=":"), 5,"left","0")
  }else{ # if data intervals are monthly or bigger
    Times <- "00:00" # monthly averages are addressed with time stamp 00:00
    if(TypeOrigin == "monthly_averaged_reanalysis_by_hour_of_day"){
      Times <- str_pad(str_c(0:23,"00",sep=":"), 5,"left","0")
    }
  } # end of time check

  # FileName (generate automatic filename if none is specified by user)
  if(is.null(FileName)){
    FileName <- paste(Variable, DateStart, DateStop, TResolution, sep="_")
  }
  # FileName <- strsplit(FileName, split =".nc") # remove .nc ending, if specified by user so that next line doesn't end up with a file ending of ".nc.nc"
  FileName <- paste0(FileName, ".nc") # adding netcdf ending to file name
  FileNames_vec <- paste0(str_pad(1:n_calls, 4, "left", "0"), "_", FileName) # names for individual downloads
  ### BUILDING REQUEST ----
  if(isTRUE(verbose)){print(paste("Staging", n_calls, "downloads."))}
  # Setting parameters of desired downloaded netcdf file according to user input
  looptext <- "
    Request_ls <- list('dataset_short_name' = DataSet,
                       'product_type'   = Type,
                       'variable'       = Variable,
                       'year'           = Calls_ls[[Downloads_Iter]][1],
                       'month'          = Calls_ls[[Downloads_Iter]][2],
                       'day'            = Calls_ls[[Downloads_Iter]][3:length(Calls_ls[[Downloads_Iter]])],
                       'time'           = Times,
                       'area'           = Extent,
                       'format'         = 'netcdf',
                       'target'         = FileNames_vec[Downloads_Iter],
                       'grid'           = Grid)

    ### EXECUTING REQUEST ----
    if(file.exists(file.path(Dir, FileNames_vec[Downloads_Iter]))){
      if(isTRUE(verbose)){print(paste(FileNames_vec[Downloads_Iter], 'already downloaded'))}
      next()
    }
    if(isTRUE(verbose)){print(paste(FileNames_vec[Downloads_Iter], 'download queried'))}
    Down_try <- 0
    while(!file.exists(file.path(Dir, FileNames_vec[Downloads_Iter])) & Down_try < TryDown){
      if(Down_try>1){print('Retrying Download')}
        API_request <- 1
        try(API_request <- wf_request(user = as.character(API_User),
                     request = Request_ls,
                     transfer = TRUE,
                     path = Dir,
                     verbose = verbose,
                     time_out = TimeOut))
      if(length(API_request) != 1){
        wf_delete(user = as.character(API_User),
                      url = API_request$request_id,
                      service = API_Service)
      }
      Down_try <- Down_try+1
    }
    if(!file.exists(file.path(Dir, FileNames_vec[Downloads_Iter]))){ # give error if kriging fails
      stop(paste('Downloading for month', Downloads_Iter, 'failed with error message above.'))
    }
    "

  if(Cores > 1){ # Cores check: if parallel processing has been specified
    ForeachObjects <- c("DataSet", "Type", "Variable", "Calls_ls", "Times", "Extent", "FileNames_vec", "Grid", "API_Key", "API_User", "Dir", "verbose", "TryDown")
    cl <- makeCluster(Cores) # Assuming Cores node cluster
    registerDoParallel(cl) # registering cores
    foreach(Downloads_Iter = 1:n_calls,
            .packages = c("ecmwfr"), # import packages necessary to each itteration
            .export = ForeachObjects) %:% when(!file.exists(file.path(Dir, FileNames_vec[Downloads_Iter]))) %dopar% {
              eval(parse(text=looptext))
            } # end of parallel kriging loop
    stopCluster(cl) # close down cluster
  }else{ # if non-parallel processing has been specified
    for(Downloads_Iter in 1:n_calls){eval(parse(text=looptext))}
  } # end of non-parallel loop

  ### LOAD DATA BACK IN ----
  if(isTRUE(verbose)){print("Checking for known data issues.")}
  Files_vec <- file.path(Dir, list.files(Dir, pattern = paste0("_", FileName))) # all files belonging to this query with folder paths
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
  if(isTRUE(verbose)){print("Loading downloaded data for masking and aggregation.")}
  if(length(Layers) == 1 & Layers == 1){
    Era5_ras <- stack(Files_vec)
  }else{
    Era5_ls <- as.list(rep(NA, length(Layers))) # list for layer aggregation
    for(LoadIter in Layers){
      ERA5_ls <- as.list(rep(NA, n_calls)) # list for layer aggregation
      for(LOADIter in 1:n_calls){
        ERA5_ls[[LOADIter]] <- raster::brick(x = Files_vec[LOADIter], level = Layers[[LoadIter]]) # loading the data
      }
      Era5_ras <- stack(ERA5_ls)
      ### LIST SAVING
      Era5_ls[[LoadIter]] <- Era5_ras
    }
    Era5_ras <- stack(Era5_ls)
  }

  if(Type == "ensemble_members" & TResolution == "hour"){ ## fixing indices of layers for ensemble means
    Indices <- sub(pattern = "X", replacement = "", names(Era5_ras))
    Indices <- sub(pattern = ".*\\_", replacement = "", Indices)
    Indices2 <- strsplit(x = Indices, split = ".", fixed = TRUE)
    Len <- length(Indices2[[1]])
    Indices3 <- str_pad(unlist(Indices2), 3, "left","0")
    PairNumbers <- rep(1:(length(Indices3)/Len), each = Len)
    Indices4 <- paste(Indices3[which(PairNumbers == 1)], collapse = "")
    for(IndicesIter in 2:(length(Indices3)/Len)){
      Indices4 <- c(Indices4, paste(Indices3[which(PairNumbers == IndicesIter)], collapse = ""))
    }
    Indices4 <- as.numeric(Indices4)
    Era5_ras <- Era5_ras[[order(Indices4)]]
  }

  ### MASKING ----
  if(exists("Shape")){ # Shape check
    if(isTRUE(verbose)){print("Masking according to shape/buffer polygon")}
    range_m <- mask_Shape(base.map = Era5_ras[[1]], Shape = Shape)
    Era5_ras <- mask(Era5_ras, range_m)
    # Shape_ras <- rasterize(Shape, Era5_ras, getCover=TRUE) # identify which cells are covered by the shape
    # Shape_ras[Shape_ras==0] <- NA # set all cells which the shape doesn't touch to NA
    # Era5_ras <- mask(x = Era5_ras, mask = Shape_ras) # mask if shapefile was provided
  }# end of Shape check

  ### PRECIP FIX ----
  if(isTRUE(verbose)){print("Aggregating to temporal resolution of choice")}
  if(PrecipFix == TRUE & TResolution == "day" | PrecipFix == TRUE & TResolution == "hour"){
    if(DateStart == "1950-01-01"){ ## apply fix for first-hour of 1981 here, too
      Era5_ras <- Era5_ras[[-(nlayers(Era5_ras)-22):-nlayers(Era5_ras)]]
    }else{
      Era5_ras <- Era5_ras[[c(-1, -(nlayers(Era5_ras)-22):-nlayers(Era5_ras))]]
    }
    counter <- 1
    Era5_ls <- as.list(rep(NA, nlayers(Era5_ras)))
    names(Era5_ls) <- names(Era5_ras)

    for(i in 1:nlayers(Era5_ras)){

      if(counter > 24){counter <- 1}

      if(counter == 1){
        Era5_ls[[i]] <- Era5_ras[[i]]
        StartI <- i
      }

      if(counter == 24){
        Era5_ls[[i]] <- Era5_ras[[i]]-sum(brick(Era5_ls[StartI:(StartI+counter-2)]))
      }
      if(counter != 24 & counter != 1){
        Era5_ls[[i]] <- Era5_ras[[i+1]] - Era5_ras[[i]]
      }
      counter <- counter + 1
    }
    Era5_ras <- stack(Era5_ls)
    warning("You toggled on the PrecipFix option in the function call. Hourly records have been converted from cumulative aggregates to individual hourly records of precipitation. This is currently an experimental feature.")
  }
  if(PrecipFix == TRUE & TResolution == "month" | PrecipFix == TRUE & TResolution == "year"){
    Era5_ras <- Era5_ras * days_in_month(seq(ymd(DateStart),ymd(DateStop), by = '1 month'))
    warning("You toggled on the PrecipFix option in the function call. Monthly records have been multiplied by the amount of days per respective month. This is currently an experimental feature.")
  }

  ### DAY/YEAR MEANS ----
  if(TResolution == "day" | TResolution == "year"){ # day/year check: need to build averages for days (from hours) and years (from months), we pulled hourly data
    if(TResolution == "day"){ # daily means
      if(Type == "reanalysis"){
        factor <- 24 # number of hours per day in reanalysis data
      }else{
        factor <- 8 # ensemble data only has 8 time steps in a day
      }
    }else{ # annual means
      factor <- 12 # number of months per year
    }
    if(Type != "ensemble_members"){
      # if(TResolution == "hour" | TResolution == "day" & DateStart == "1981-01-01"){
      #   Index <- rep(1:((nlayers(Era5_ras)+1)/factor), each = factor)[-1] # fix first-hour issue for 01-01-1981
      # }else{
      Index <- rep(1:(nlayers(Era5_ras)/factor), each = factor) # build an index
      # }
    }else{
      Index <- rep(1:(nlayers(Era5_ras)/factor), each = factor*10) # build an index
    }
    Era5_ras <- stackApply(Era5_ras, Index, fun=FUN) # do the calculation
  }# end of day/year check
  if(exists("range_m")){Era5_ras <- mask(Era5_ras, range_m)} ## apply masking again for stackapply functions which don't track NAs properly

  ### TIME STEP MEANS ----
  if(nlayers(Era5_ras)%%TStep != 0){ # sanity check for completeness of time steps and data
    warning(paste0("Your specified time range does not allow for a clean integration of your selected time steps. Only full time steps will be computed. You specified a time series with a length of ", nlayers(Era5_ras), "(", TResolution,") and time steps of ", TStep, ". This works out to ", nlayers(Era5_ras)/TStep, " intervals. You will receive ", floor(nlayers(Era5_ras)/TStep), " intervals."))
  }# end of sanity check for time step completeness
  Index <- rep(1:(nlayers(Era5_ras)/TStep), each = TStep) # build an index
  Era5_ras <- stackApply(Era5_ras[[1:length(Index)]], Index, fun=FUN) # do the calculation
  if(exists("range_m")){Era5_ras <- mask(Era5_ras, range_m)} ## apply masking again for stackapply functions which don't track NAs properly

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
#' @param Shape Optional, a SpatialPolygonsDataFrame or data.frame object. If Shape is a SpatialPolygonsDataFrame, this will be treated as a shapefile and the output will be cropped and masked to this shapefile. If Shape is a data.frame of geo-referenced point records, it needs to contain Lat and Lon columns as well as a non-repeating ID-column.
#' @param Buffer Optional. Identifies how big a rectangular buffer to draw around points if Shape is a data frame of points. Buffer is expressed as centessimal degrees.
#' @param ID Optional. Identifies which column in Shape to use for creation of individual buffers if Shape is a data.frame.
#' @param Dir Directory specifying where to download data to.
#' @param Keep_Temporary Logical, whether to delete individual, global, 30 arc-sec files or keep them to be reused in later analyses.
#' @param Source Character. Whether to attempt download from the official USGS data viewer (Source = "USGS") or a static copy of the data set on a private drive (Source = "Drive"). Default is "USGS". Use this if the USGS viewer is unavailable.
#' @return A list containing two raster object ready to be used as covariates for kriging, and two NETCDF (.nc) files in the specified directory.
#' @examples
#' \dontrun{
#' # Downloading ERA5-Land air temperature reanalysis data in 12-hour intervals for 02/01/1995 - 04/01/1995 (DD/MM/YYYY). API User and Key in this example are non-functional. Substitute with your user number and key to run this example.
#' Extent <- extent(c(11.8,15.1,50.1,51.7)) # roughly the extent of Saxony
#' API_User <- "..."
#' API_Key <- "..."
#' State_Raw <- download_ERA(
#' Variable = "2m_temperature",
#' DataSet = "era5-land",
#' DateStart = "1995-01-02",
#' DateStop = "1995-01-04",
#' TResolution = "hour",
#' TStep = 12,
#' Extent = Extent,
#' API_User = API_User,
#' API_Key = API_Key
#' )
#' State_Raw # a raster brick with 6 layers at resolution of ~0.1°
#' # Downloading GMTED2010-data at resolution and extent obtained by a call to download_ERA and a target resolution of .02.
#' Covs_ls <- download_DEM(
#' Train_ras = State_Raw,
#' Target_res = .02,
#' Keep_Temporary = TRUE
#' )
#' Covs_ls # a list with two elements: (1) GMTED 2010 data at training resolution, and (2) GMTED 2010 data aggregated as close as possible to a resolution of 0.02
#' }
#'
#' @export
download_DEM <- function(Train_ras = NULL,
                         Target_res = NULL,
                         Shape = NULL, Buffer = 0.5, ID = "ID",
                         Dir = getwd(), Keep_Temporary = FALSE, Source = "USGS"){

  ### PREPARATION -----
  Extent <- extent(Train_ras) # extract extent for later cropping
  if(Source == "USGS"){
    Link <- "https://edcintl.cr.usgs.gov/downloads/sciweb1/shared/topo/downloads/GMTED/Grid_ZipFiles/mn30_grd.zip" # Link to GMTED2010
  }
  if(Source == "Drive"){
    Link <- "https://www.dropbox.com/s/whkje7jc401xuwx/GMTED2010.zip?raw=1"# link to DropBox with GMTED2010
  }

  # handling Target_res
  ## distinguishing if Target_res is a raster or a resolution, this will change whether GMTED2010 is aggregated or resampled
  if(class(Target_res[[1]]) == "RasterLayer"){
    Target_ras <- Target_res
    Target_res <- res(Target_ras)
  }

  # Extent vs. Shapefile vs. Points
  if(class(Shape) == "data.frame"){ # if we have been given point data
    Shape <- buffer_Points(Points = Shape, Buffer = Buffer, ID = ID)
  }

  ### DOWNLOADING & UNPACKING -----
  Dir.Data <- file.path(Dir, "GMTED2010") # identify folder for GMTED2010 data
  if(!file.exists(file.path(Dir.Data, "GMTED2010.zip"))){ # file check: check if file is not already downloaded
    dir.create(Dir.Data) # create folder for GMTED2010 data
    print("Downloading GMTED2010 covariate data.") # inform user of download in console
    httr::GET(Link,
              write_disk(file.path(Dir.Data, "GMTED2010.zip")),
              progress(), overwrite = TRUE)
    unzip(file.path(Dir.Data, "GMTED2010.zip"), # which file to unzip
          exdir = Dir.Data) # where to unzip to
  } # end of file check

  ### RASTERIZING & CROPPING -----
  GMTED2010_ras <- raster(file.path(Dir.Data, "mn30_grd/w001001.adf")) # rasterising elevation data
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

  GMTED2010Target_ras <- setValues(raster(GMTED2010Target_ras), GMTED2010Target_ras[]) # remove attributes
  ### MASKING ----
  if(!is.null(Shape)){ # Shape check
    range_m <- mask_Shape(base.map = GMTED2010Train_ras, Shape = Shape)
    GMTED2010Train_ras <- mask(GMTED2010Train_ras, range_m)
    range_m <- mask_Shape(base.map = GMTED2010Target_ras, Shape = Shape)
    GMTED2010Target_ras <- mask(GMTED2010Target_ras, range_m)
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
