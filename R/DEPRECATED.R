#' Downloading ERA5(Land)-data from ECMWF servers
#'
#' This function is used to obtain Era5/Era5-Land data from the [Climate Data Store](https://cds.climate.copernicus.eu/#!/home) hosted by the [Copernicus Climate Change Service (C3S)](https://cds.climate.copernicus.eu/about-c3s). By default, this function breaks down download calls into monthly intervals, downloads Era5(-Land) data from [ECMWF](https://www.ecmwf.int/) servers according to user-specification, and fuses the downloaded files together according to user-demands. The actual time to download is dependent on ECMWF download queues. Users need an [API key](https://cds.climate.copernicus.eu/api-how-to) for download staging.
#'
#' Use optional arguments verbose, Cores, and SingularDL for updates on function progress, parallel download staging and execution, and forcing of downloads into one singular download, espectively.
#'
#' @param Variable ERA5(Land)-contained climate variable.
#' @param PrecipFix Logical. Era5(-land) total precipitation is recorded in cumulative steps per hour from the 00:00 time mark per day. Setting PrecipFix to TRUE converts these into records which represent the total precipitation per hour. Monthly records in Era5(-land) express the average daily total precipitation. Setting this argument to TRUE multiplies monthly records by the number of days per the respective month(s) to get to total precipitation records instead of average.  Default is FALSE. This also applies to other variables in the data sets. See the data descriptor webpages (e.g.: https://cds.climate.copernicus.eu/cdsapp#!/dataset/reanalysis-era5-land-monthly-means?tab=overview) for an overview of which variables this applies to.
#' @param Type Whether to download reanalysis ('reanalysis', 'monthly_averaged_reanalysis_by_hour_of_day') or ensemble ('ensemble_members', 'ensemble_mean', or 'ensemble_spread') data. Only available for era5 data.
#' @param DataSet Which ERA5 data set to download data from. 'era5' or 'era5-land'.
#' @param DateStart Date ('YYYY-MM-DD') at which to start time series of downloaded data.
#' @param DateStop Date ('YYYY-MM-DD') at which to stop time series of downloaded data.
#' @param TResolution Temporal resolution of final product. 'hour', 'day', 'month', or 'year'.
#' @param TStep Which time steps (numeric) to consider for temporal resolution. For example, specify bi-monthly data records by setting TResolution to 'month' and TStep to 2.
#' @param FUN A raster calculation argument as passed to `raster::stackApply()`. This controls what kind of data to obtain for temporal aggregates of reanalysis data. Specify 'mean' (default) for mean values, 'min' for minimum values, and 'max' for maximum values, among others.
#' @param Extent Optional, download data according to rectangular bounding box. specify as extent() object or as a raster, a SpatialPolygonsDataFrame object, or a data.frame object. If Extent is a SpatialPolygonsDataFrame, this will be treated as a shapefile and the output will be cropped and masked to this shapefile. If Extent is a data.frame of geo-referenced point records, it needs to contain Lat and Lon columns as well as a non-repeating ID-column.
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
#' @param SingularDL Logical. Whether to force download of data in one call to CDS or automatically break download requests into individual monthly downloads. Default is FALSE.
#' @param ... Additional arguments used for parsing more information to the ecmwfr download call such as pressure_level = 1 for download of pressure_level data for the ERA5 reanalysis
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
                          API_User = NULL, API_Key = NULL, TryDown = 10, verbose = TRUE,
                          Cores = 1, TimeOut = 36000, SingularDL = FALSE,
                          ...) {

  stop("This function is deprecated and has been superceeded by the CDownloadS() function which offers improvements over this implementation. To get started using the new function, you may want to consult the relevant documentation by calling ?CDownloadS(). Please note that this deprecated function will be removed alltogether when KrigR reaches version 1.0.0.")

  if(verbose){message("download_ERA() is starting. Depending on your specifications, this can take a significant time.")}

  if(verbose){
    ProgBar <- 'text'
  }else{
    ProgBar <- NULL
  }

  ### PrecipFix Mispecification Check ----
  # if(PrecipFix == TRUE & !startsWith(x = Variable, prefix = "total_")){
  #   stop("You cannot specify PrecipFix = TRUE without calling on a Variable that starts with total_ indicating a cummulative nature")
  # }

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
  if(DataSet == "era5-land"){
    Type <- NA # we do not hand era5-land api requests a Type
  }

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
  if(is.data.frame(Extent)){ # if we have been given point data
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
    stop('The Extent argument provided by you is neither formatted as an Extent nor a Raster nor SpatialPolygonsDataFrame object nor an object of only class data.frame. Please correct this.')
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
  Extent <- Corner_vec

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
  FileName <- tools::file_path_sans_ext(FileName) # remove .nc ending, if specified by user so that next line doesn't end up with a file ending of ".nc.nc"
  FileName <- paste0(FileName, ".nc") # adding netcdf ending to file name
  FileNames_vec <- paste0(str_pad(1:n_calls, 4, "left", "0"), "_", FileName) # names for individual downloads
  ### REQUEST DATA ----
  if(SingularDL){
    n_calls <- 1
    FileNames_vec <- FileNames_vec[1]
    Cores <- 1
  }

  looptext <- "
if(SingularDL){ # If user forced download to happen in one
                     ## finding the start and stop dates for SingularDownload
                     SingularDL_Start <- as.Date(paste(
                       min(unique(sapply(Calls_ls, '[[', 1))),
                       min(unique(sapply(Calls_ls, '[[', 2))),
                       str_pad(min(as.numeric(unique(unlist(lapply(Calls_ls, function(x) x[-1:-2]))))), 2, 'left', '0'),
                       sep = '-'))
                     SingularDL_Stop <- as.Date(paste(
                       max(unique(sapply(Calls_ls, '[[', 1))),
                       max(unique(sapply(Calls_ls, '[[', 2))),
                       days_in_month(Dates_seq[length(Dates_seq)]),
                       sep = '-'))
                     if(TResolution == 'day' | TResolution == 'hour'){
      LayerDL_seq <- paste(rep(seq.Date(from = SingularDL_Start, to = SingularDL_Stop, by = 'day'), each = 24), paste0(str_pad(1:24, 2, 'left', 0), ':00'), sep = '_')
    }else{
      LayerDL_seq <- seq.Date(from = SingularDL_Start, to = SingularDL_Stop, by = 'month')
    }
                     if(length(LayerDL_seq)>1e5){stop('Your download is too big. Please specify a shorter time window, coarser temporal resolution, or set SingularDL = FALSE.')}
                     ## notify user of mismatch in time windows if there is one
                     if(SingularDL_Start != DateStart | SingularDL_Stop != DateStop){
                     if(TypeOrigin != 'reanalysis'){stop('Currently, SIngularDL may only be toggled on for reanalysis type download queries or any query where full months within one year, or full years of data are queried irrespective of dataset type.')}
                       message(paste('Setting SingularDL to TRUE has forced your download to retrieve data in intervals of', TStep, TResolution, 'between', SingularDL_Start, '(YYYY-MM-DD) and', SingularDL_Stop, '(YYYY-MM-DD). KrigR will limit the data to your originally desired time range of', DateStart, '(YYYY-MM-DD) to', DateStop, '(YYYY-MM-DD).')
                       )
                     }
                   }

                   if(SingularDL){
                     FName <- FileNames_vec[1]
                     Year_call <- unique(sapply(Calls_ls, '[[', 1))
                     month_call <- unique(sapply(Calls_ls, '[[', 2))
                     day_call <- str_pad(unique(unlist(lapply(Calls_ls, function(x) x[-1:-2]))), 2, 'left', 0)
                   }else{
                     FName <- FileNames_vec[Downloads_Iter]
                     Year_call <- Calls_ls[[Downloads_Iter]][1]
                     month_call <- Calls_ls[[Downloads_Iter]][2]
                     day_call <- Calls_ls[[Downloads_Iter]][3:length(Calls_ls[[Downloads_Iter]])]
                   }

                   ### Requesting Download
                   Request_ls <- c(
                   list('dataset_short_name' = DataSet,
                                      'product_type'   = Type,
                                      'variable'       = Variable,
                                      'year'           = Year_call,
                                      'month'          = month_call,
                                      'day'            = day_call,
                                      'time'           = Times,
                                      'area'           = Extent,
                                      'format'         = 'netcdf',
                                      'target'         = FName,
                                      'grid'           = Grid
                                      ),
                                      list(...)
                                      )
                    # print(Request_ls)

                   if(file.exists(file.path(Dir, FName))){
                     if(verbose){message(paste(FName, 'already downloaded'))}
                   }else{
                   if(verbose & SingularDL){message('Staging your request as a singular download now. This can take a long time due to size of required product.')}
                     if(verbose){message(paste(FName, 'download queried'))}
                     Down_try <- 0
                     while(!file.exists(file.path(Dir, FName)) & Down_try < TryDown){
                       if(Down_try>1){message('Retrying Download')}
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
                   }
                                                              "

  if(verbose){message(paste("Staging", n_calls, "download(s)."))}
  if(Cores > 1){ # Cores check: if parallel processing has been specified
    ForeachObjects <- c("DataSet", "Type", "Variable", "Calls_ls", "Times", "Extent", "FileNames_vec", "Grid", "API_Key", "API_User", "Dir", "verbose", "TryDown", "TimeOut", "API_Service", "TResolution", "SingularDL")
    pb <- txtProgressBar(max = n_calls, style = 3)
    progress <- function(n){setTxtProgressBar(pb, n)}
    opts <- list(progress = progress)
    cl <- makeCluster(Cores) # Assuming Cores node cluster
    registerDoSNOW(cl) # registering cores
    foreach::foreach(Downloads_Iter = 1:n_calls,
                     .packages = c("ecmwfr"), # import packages necessary to each itteration
                     .export = ForeachObjects,
                     .options.snow = opts) %:% when(!file.exists(file.path(Dir, FileNames_vec[Downloads_Iter]))) %dopar% {
                       eval(parse(text=looptext))
                     } # end of parallel kriging loop
    close(pb)
    stopCluster(cl)
  }else{ # if non-parallel processing has been specified
    for(Downloads_Iter in 1:n_calls){
      eval(parse(text=looptext))
    }
  } # end of non-parallel loop

  FileName <- tools::file_path_sans_ext(FileName)

  ### LOAD DATA BACK IN ----
  if(verbose){message("Checking for known data issues.")}
  Files_vec <- file.path(Dir, FileNames_vec) # all files belonging to this query with folder paths
  if(is.na(Type)){Type <- "reanalysis"} # na Type is used for Era5-land data which is essentially just reanalysis
  # ensemble_member check: if user downloaded ensemble_member data
  if(Type == "ensemble_members" | Type == "monthly_averaged_ensemble_members"){  # ensemble_member check: if user downloaded ensemble_member data
    Layers <- 1:10 # ensemble members come in 10 distinct layers
  }else{
    Layers <- 1 # non-ensemble members have 1 layer for each time step instead of 10
    ### ERROR CHECK (CDS sometimes produces a netcdf with two layers and break point in the data being assigned to the first and then the second layer in non-ensemble members. this step fixes this) ----
    for(Layers_Check in 1:length(Files_vec)){
      LayersSame <- suppressWarnings(all.equal(brick(Files_vec[Layers_Check], level = 1), brick(Files_vec[Layers_Check], level = 2))) # Check if the layers are the same in brick loading
      if(LayersSame == FALSE){
        Era5_ras <- brick(Files_vec[Layers_Check], level = 1) # load initial data again for just the first band
        Era5_ras2 <- brick(Files_vec[Layers_Check], level = 2) # load second layer
        Sums_vec <- NA # recreate sum vector
        for(Iter_Check in 1:nlayers(Era5_ras2)){ # layer loop: go over all layers in Era5_ras2
          Sums_vec <- c(Sums_vec, sum(values(Era5_ras2[[Iter_Check]]), na.rm = TRUE)) # append sum of data values to sum vector, layers with issues will produce a 0
        } # end of layer loop
        Sums_vec <- na.omit(Sums_vec) # omit initial NA
        StopFirst <- min(which(Sums_vec != 0)) # identify the last layer of the brick that is problematic on the second data layer loaded above
        Era5_ras <- stack(Era5_ras[[1:(StopFirst-1)]], Era5_ras2[[StopFirst:nlayers(Era5_ras2)]]) # rebuild the Era5_ras stack as a combination of the data-containing layers in the two bricks
        terra::writeCDF(x = as(brick(Era5_ras), "SpatRaster"), filename = Files_vec[Layers_Check])
      }
    }
  } # end of ensemble_member check

  ### loop over files and layers here
  if(verbose){message("Loading downloaded data for masking and aggregation.")}
  if(length(Layers) == 1){
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

  ## Fix for extent of some variables like evaporation_from_vegetation_transpiration for which the x-extent values come out weirdly despite the correct data being downloaded
  MaxOffset <- max(abs(round(as.vector(extent(Era5_ras))-c(Extent[2], Extent[4], Extent[3], Extent[1]), 2))) # identify whether there is an issue (currently, we only know of cases where western-hemisphere data is reported as x-extents that are 360-x-Extent values)
  if(MaxOffset > 270){
    DataOffset <- min(abs(round(as.vector(extent(Era5_ras))-c(Extent[2], Extent[4], Extent[3], Extent[1]), 2))) # identify by how much grid had to be expanded for download
    extent(Era5_ras) <- extent(Extent[2]-DataOffset, Extent[4]+DataOffset,
                               Extent[3]-DataOffset, Extent[1]+DataOffset) # the extent the object should have assigned to the object
  }

  ### SingularDL limiting of data to original time-series requirements
  ## time-sequence of requested download by user
  if(TResolution == "day" | TResolution == "hour"){
    Layer_seq <- paste(rep(seq.Date(from = DateStart, to = DateStop, by = "day"), each = 24), paste0(str_pad(1:24, 2, "left", 0), ":00"), sep = "_")
  }else{
    Layer_seq <- seq.Date(from = DateStart, to = DateStop, by = "month")
  }
  if(DateStart == "1950-01-01" & TResolution == "day" | DateStart == "1950-01-01" & TResolution == "hour"){
    Layer_seq <- Layer_seq[-1]
  }
  ## subsetting of downloaded data
  if(SingularDL){
    ## time-sequence of requested download by SingularDL
    # LayerDL_seq is actually downloaded
    # Layer_seq is not actually downloaded, but requested by user
    if(TResolution == "day" | TResolution == "hour"){
      LayerDL_seq <- paste(rep(seq.Date(from = SingularDL_Start, to = SingularDL_Stop, by = "day"), each = 24), paste0(str_pad(1:24, 2, "left", 0), ":00"), sep = "_")
    }else{
      LayerDL_seq <- seq.Date(from = SingularDL_Start, to = SingularDL_Stop, by = "month")
    }
    if(DateStart == "1950-01-01" & TResolution == "day"| DateStart == "1950-01-01" & TResolution == "hour"){
      LayerDL_seq <- LayerDL_seq[-1]
    }

    if(Type == "ensemble_members"){
      LayerDL_seq <- rep(LayerDL_seq[rep(c(TRUE, c(FALSE, FALSE)), length.out = length(LayerDL_seq))], each = 10)
      Layer_seq <- rep(Layer_seq[rep(c(TRUE, c(FALSE, FALSE)), length.out = length(Layer_seq))], each = 10)
    }

    if(Type == "monthly_averaged_ensemble_members"){
      LayerDL_seq <- rep(LayerDL_seq, each = 10)
      Layer_seq <- rep(LayerDL_seq, each = 10)
    }
    ## subsetting
    Era5_ras <- Era5_ras[[which(LayerDL_seq %in% Layer_seq)]]
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
    if(verbose){message("Masking according to shape/buffer polygon")}
    range_m <- mask_Shape(base.map = Era5_ras[[1]], Shape = Shape)
    Era5_ras <- mask(Era5_ras, range_m, progress=ProgBar)
    # Shape_ras <- rasterize(Shape, Era5_ras, getCover=TRUE) # identify which cells are covered by the shape
    # Shape_ras[Shape_ras==0] <- NA # set all cells which the shape doesn't touch to NA
    # Era5_ras <- mask(x = Era5_ras, mask = Shape_ras) # mask if shapefile was provided
  }# end of Shape check

  ### PRECIP FIX ----
  if(verbose){message("Aggregating to temporal resolution of choice")}
  if(PrecipFix == TRUE & TResolution == "day" | PrecipFix == TRUE & TResolution == "hour"){
    if(DateStart == "1950-01-01"){ ## do this by layer names as contained in downloaded data!!!
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
    if(Type != "ensemble_members" & Type != "monthly_averaged_ensemble_members"){
      Days_in_Month_vec <- days_in_month(seq(ymd(DateStart),ymd(DateStop), by = '1 month'))
    }else{
      Days_in_Month_vec <- rep(days_in_month(seq(ymd(DateStart),ymd(DateStop), by = '1 month')), each = 10)
    }
    Era5_ras <- Era5_ras * Days_in_Month_vec
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
    if(Type != "ensemble_members" & Type != "monthly_averaged_ensemble_members"){
      if(TResolution == "hour" | TResolution == "day" & DateStart == "1950-01-01"){
        Index <- rep(1:((nlayers(Era5_ras)+1)/factor), each = factor)[-1] # fix first-hour issue for 01-01-1981
      }else{
        Index <- rep(1:(nlayers(Era5_ras)/factor), each = factor) # build an index
      }
    }else{
      Index <- rep(1:(nlayers(Era5_ras)/factor), each = factor*10) # build an index
    }
    if(sum(duplicated(Index)) != 0){
      Era5_ras <- stackApply(Era5_ras, Index, fun=FUN, progress=ProgBar) # do the calculation
      if(exists("range_m")){Era5_ras <- mask(Era5_ras, range_m)} ## apply masking again for stackapply functions which don't track NAs properly
    }
  }# end of day/year check


  ### TIME STEP MEANS ----
  if(nlayers(Era5_ras)%%TStep != 0){ # sanity check for completeness of time steps and data
    warning(paste0("Your specified time range does not allow for a clean integration of your selected time steps. Only full time steps will be computed. You specified a time series with a length of ", nlayers(Era5_ras), "(", TResolution,") and time steps of ", TStep, ". This works out to ", nlayers(Era5_ras)/TStep, " intervals. You will receive ", floor(nlayers(Era5_ras)/TStep), " intervals."))
  }# end of sanity check for time step completeness
  Index <- rep(1:(nlayers(Era5_ras)/TStep), each = TStep) # build an index
  if(sum(duplicated(Index)) != 0){
    Era5_ras <- stackApply(Era5_ras[[1:length(Index)]], Index, fun=FUN, progress=ProgBar) # do the calculation
    if(exists("range_m")){Era5_ras <- mask(Era5_ras, range_m)} ## apply masking again for stackapply functions which don't track NAs properly
  }

  ### SAVING DATA ----
  if(class(Era5_ras) == "RasterBrick"){
    Era5_spatras <- as(Era5_ras, "SpatRaster")
  }else{
    Era5_spatras <- as(brick(Era5_ras), "SpatRaster")
  }

  ## z-values for date tracking
  OneStepSeq <- seq(from = as.POSIXct(DateStart), to = as.POSIXct(DateStop), by = TResolution)

  if(TResolution == "hour" & PrecipFix){
    OneStepSeq <- seq(from = as.POSIXct(DateStart), to = (as.POSIXct(DateStop)+3600), by = TResolution)
    OneStepSeq <- OneStepSeq[-length(OneStepSeq)]
  }
  if(TResolution == "hour" & !PrecipFix){
    OneStepSeq <- seq(from = as.POSIXct(DateStart), to = (as.POSIXct(DateStop)+86400), by = TResolution)
    OneStepSeq <- OneStepSeq[-length(OneStepSeq)]
  }

  Time_z <- OneStepSeq[seq(1, length(OneStepSeq), TStep)]
  if(PrecipFix & (TResolution != "month" & TResolution != "year")){Time_z <- Time_z[-1]}
  terra::time(Era5_spatras) <- Time_z

  terra::writeCDF(x = Era5_spatras, filename = paste0(file.path(Dir, FileName), ".nc"), overwrite = TRUE, varname = Variable)

  unlink(Files_vec, recursive = TRUE)
  return(stack(file.path(Dir, paste0(FileName, ".nc")))) # to circumvent issues with 1-hour downloads
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

    stop("This function is deprecated and has been superceeded by the CovariateSetup() function which offers improvements over this implementation. To get started using the new function, you may want to consult the relevant documentation by calling ?CovariateSetup(). Please note that this deprecated function will be removed alltogether when KrigR reaches version 1.0.0.")

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
    message("Downloading GMTED2010 covariate data.") # inform user of download in console
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
  terra::writeCDF(x = as(brick(GMTED2010Train_ras), "SpatRaster"), filename = file.path(Dir, "GMTED2010_Train.nc"), overwrite = TRUE)
  terra::writeCDF(x = as(brick(GMTED2010Target_ras), "SpatRaster"), filename = file.path(Dir, "GMTED2010_Target.nc"), overwrite = TRUE)
  # writeRaster(x = GMTED2010Train_ras, filename = file.path(Dir, "GMTED2010_Train.nc"), overwrite = TRUE, format="CDF")
  # writeRaster(x = GMTED2010Target_ras, filename = file.path(Dir, "GMTED2010_Target.nc"), overwrite = TRUE, format="CDF")

  ### REMOVE FILES FROM HARD DRIVE -----
  if(Keep_Temporary == FALSE){ # cleanup check
    unlink(Dir.Data, recursive = TRUE)
  }  # end of cleanup check

  return(list(GMTED2010Train_ras, GMTED2010Target_ras))
}

#' (multi-core) Kriging
#'
#' This function statistically downscales input data using covariate data and the kriging methodology. The function can be run in two ways:
#' \enumerate{
#' \item \strong{By Itself}: Use the arguments Data, Covariates_coarse, Covariates_fine when you already have raster files for your data which is to be downscaled as well as covariate raster data.
#' \item \strong{From Scratch}: Use the arguments Variable, Type, DataSet, DateStart, DateStop, TResolution, TStep, Extent, Dir, FileName, API_Key, API_User, and arget_res. By doing so, krigR will call the functions download_ERA() and download_DEM() for one coherent kriging workflow. Note that this process does not work when targetting UERRA data.
#' }
#' Use optional arguments such as Dir, FileName, Keep_Temporary, SingularTry, KrigingEquation and Cores for ease of use, substitution of non-GMTED2010 covariates, and parallel processing.
#'
#' @param Data Raster file which is to be downscaled.
#' @param Covariates_coarse Raster file containing covariates at training resolution.
#' @param Covariates_fine Raster file containing covariates at target resolution.
#' @param KrigingEquation Formula or character string specifying which covariates to use and how. Layer names in Covariates_coarse and Covariates_fine need to match Parameters in this formula. Needs to start with "X ~ ". X can read anything you like.
#' @param Dir Optional. Directory specifying where to place final kriged product. Default is current working directory.
#' @param FileName Optional. A file name for the netcdf produced. Default is a combination parameters in the function call.
#' @param Keep_Temporary Logical, whether to delete individual kriging products of layers in Data after processing. Default is TRUE.
#' @param Cores Numeric. How many cores to use. If you want output to your console during the process, use Cores == 1. Parallel processing is carried out when Cores is bigger than 1. Default is detecting all cores of your machine.
#' @param SingularTry Numeric. How often to try kriging of each layer of the input. This usually gets around issues of singular covariance matrices in the kriging process, but takes some time. Default is 10
#' @param Variable Optional, calls download_ERA(). ERA5(Land)-contained climate variable.
#' @param PrecipFix Optional. Era5(-land) total precipitation is recorded in cumulative steps per hour from the 00:00 time mark per day. Setting PrecipFix to TRUE converts these into records which represent the total precipitation per hour. Monthly records in Era5(-land) express the average daily total precipitation. Setting this argument to TRUE multiplies monthly records by the number of days per the respective month(s) to get to total precipitation records instead of average.  Default is FALSE.
#' @param Type  Optional. Whether to download reanalysis ('reanalysis') or ensemble ('ensemble_members', 'ensemble_mean', or 'ensemble_spread') data. Passed on to download_ERA.
#' @param DataSet Optional. Which ERA5 data set to download data from. 'era5' or 'era5-land'. Passed on to download_ERA.
#' @param DateStart Optional. Date ('YYYY-MM-DD') at which to start time series of downloaded data. Passed on to download_ERA.
#' @param DateStop Optional. Date ('YYYY-MM-DD') at which to stop time series of downloaded data. Passed on to download_ERA.
#' @param TResolution Optional. Temporal resolution of final product. hour', 'day', 'month'. Passed on to download_ERA.
#' @param TStep Optional. Which time steps (numeric) to consider for temporal resolution. Passed on to download_ERA.
#' @param FUN Optional. A raster calculation argument as passed to `raster::stackApply()`. This controls what kind of data to obtain for temporal aggregates of reanalysis data. Specify 'mean' (default) for mean values, 'min' for minimum values, and 'max' for maximum values, among others.
#' @param Extent Optional, download data according to rectangular bounding box. specify as extent() object or as a raster, a SpatialPolygonsDataFrame object, or a data.frame object. If Extent is a SpatialPolygonsDataFrame, this will be treated as a shapefile and the output will be cropped and masked to this shapefile. If Extent is a data.frame of geo-referenced point records, it needs to contain Lat and Lon columns as well as a non-repeating ID-column. Passed on to download_ERA and download_DEM.
#' @param Buffer Optional. Identifies how big a rectangular buffer to draw around points if Extent is a data frame of points. Buffer is expressed as centessimal degrees. Passed on to download_ERA and download_DEM.
#' @param ID Optional. Identifies which column in Extent to use for creation of individual buffers if Extent is a data.frame. Passed on to download_ERA and download_DEM.
#' @param Target_res Optional. The target resolution for the kriging step (i.e. which resolution to downscale to). An object as specified/produced by raster::res(). Passed on to download_DEM.
#' @param Source Optional, character. Whether to attempt download from the official USGS data viewer (Source = "USGS") or a static copy of the data set on a  private drive (Source = "Drive"). Default is "USGS". Use this if the USGS viewer is unavailable. Passed on to download_DEM.
#' @param API_Key Optional. ECMWF cds API key. Passed on to download_ERA.
#' @param API_User Optional. ECMWF cds user number. Passed on to download_ERA.
#' @param nmax Optional. Controls local kriging. Number of nearest observations to be used kriging of each observation. Default is to use all available (Inf). You can specify as a number (numeric).
#' @param TryDown Optional, numeric. How often to attempt the download of each individual file (if querying data download) that the function queries from the server. This is to circumvent having to restart the entire function when encountering connectivity issues.
#' @param verbose Optional, logical. Whether to report progress of data download (if queried) in the console or not.
#' @param TimeOut Numeric. The timeout for each download in seconds. Default 36000 seconds (10 hours).
#' @param SingularDL Logical. Whether to force download of data in one call to CDS or automatically break download requests into individual monthly downloads. Default is FALSE.
#' @return A list object containing the downscaled data as well as the standard error for downscaling as well as the call to the krigR function, and two NETCDF (.nc) file in the specified directory which are the two data contents of the aforementioned list. A temporary directory is populated with individual NETCDF (.nc) files throughout the runtime of krigR which is deleted upon completion if Keep_Temporary = TRUE and all layers in the Data raster object were kriged successfully.
#' @examples
#' \dontrun{
#' ## THREE-STEP PROCESS (By Itself)
#' # Downloading ERA5-Land air temperature reanalysis data in 12-hour intervals for 02/01/1995 - 04/01/1995 (DD/MM/YYYY). API User and Key in this example are non-functional. Substitute with your user number and key to run this example.
#' Extent <- extent(c(11.8, 15.1, 50.1, 51.7)) # roughly the extent of Saxony
#' API_User <- "..."
#' API_Key <- "..."
#' State_Raw <- download_ERA(
#'   Variable = "2m_temperature",
#'   DataSet = "era5-land",
#'   DateStart = "1995-01-02",
#'   DateStop = "1995-01-04",
#'   TResolution = "hour",
#'   TStep = 12,
#'   Extent = Extent,
#'   API_User = API_User,
#'   API_Key = API_Key
#' )
#' State_Raw # a raster brick with 6 layers at resolution of ~0.1°
#' # Downloading GMTED2010-data at resolution and extent obtained by a call to download_ERA and a target resolution of .02.
#' Covs_ls <- download_DEM(
#'   Train_ras = State_Raw,
#'   Target_res = .02,
#'   Keep_Temporary = TRUE
#' )
#' Covs_ls # a list with two elements: (1) GMTED 2010 data at training resolution, and (2) GMTED 2010 data aggregated as close as possible to a resolution of 0.02
#' # Kriging the data sets prepared with the previous functions.
#' State_Krig <- krigR(
#'   Data = State_Raw, # data we want to krig as a raster object
#'   Covariates_coarse = Covs_ls[[1]], # training covariate as a raster object
#'   Covariates_fine = Covs_ls[[2]], # target covariate as a raster object
#' )
#'
#' ## PIPELINE (From Scratch)
#' #' # Downloading ERA5-Land air temperature reanalysis data in 12-hour intervals for 02/01/1995 - 04/01/1995 (DD/MM/YYYY), downloading and preparing GMTED 2010 covariate data, and kriging. API User and Key in this example are non-functional. Substitute with your user number and key to run this example. This example produces the same output as the example above.
#' Extent <- extent(c(11.8, 15.1, 50.1, 51.7)) # roughly the extent of Saxony
#' API_User <- "..."
#' API_Key <- "..."
#' Pipe_Krig <- krigR(
#'   Variable = "2m_temperature",
#'   Type = "reanalysis",
#'   DataSet = "era5-land",
#'   DateStart = "1995-01-02",
#'   DateStop = "1995-01-04",
#'   TResolution = "hour", #
#'   TStep = 12,
#'   Extent = Extent,
#'   API_User = API_User,
#'   API_Key = API_Key,
#'   Target_res = .02,
#' )
#' }
#'
#' @export
krigR <- function(Data = NULL, Covariates_coarse = NULL, Covariates_fine = NULL, KrigingEquation = "ERA ~ DEM", Cores = detectCores(), Dir = getwd(), FileName, Keep_Temporary = TRUE, SingularTry = 10, Variable, PrecipFix = FALSE, Type = "reanalysis", DataSet = "era5-land", DateStart, DateStop, TResolution = "month", TStep = 1, FUN = "mean", Extent, Buffer = 0.5, ID = "ID", API_Key, API_User, Target_res, Source = "USGS", nmax = Inf, TryDown = 10, verbose = TRUE, TimeOut = 36000, SingularDL = FALSE, ...) {
  stop("This function is deprecated and has been superceeded by the Kriging() function which offers improvements over this implementation. To get started using the new function, you may want to consult the relevant documentation by calling ?Kriging(). Please note that this deprecated function will be removed alltogether when KrigR reaches version 1.0.0.")

  ## CALL LIST (for storing how the function as called in the output) ----
  if (is.null(Data)) {
    Data_Retrieval <- list(
      Variable = Variable,
      Type = Type,
      PrecipFix = PrecipFix,
      DataSet = DataSet,
      DateStart = DateStart,
      DateStop = DateStop,
      TResolution = TResolution,
      TStep = TStep,
      Extent = Extent
    )
  } else {
    Data_Retrieval <- "None needed. Data was not queried via krigR function, but supplied by user."
  }
  ## CLIMATE DATA (call to download_ERA function if no Data set is specified) ----
  if (is.null(Data)) { # data check: if no data has been specified
    Data <- download_ERA(Variable = Variable, PrecipFix = PrecipFix, Type = Type, DataSet = DataSet, DateStart = DateStart, DateStop = DateStop, TResolution = TResolution, TStep = TStep, FUN = FUN, Extent = Extent, API_User = API_User, API_Key = API_Key, Dir = Dir, TryDown = TryDown, verbose = verbose, ID = ID, Cores = Cores, TimeOut = TimeOut, SingularDL = SingularDL)
  } # end of data check

  ## COVARIATE DATA (call to download_DEM function when no covariates are specified) ----
  if (is.null(Covariates_coarse) & is.null(Covariates_fine)) { # covariate check: if no covariates have been specified
    if (class(Extent) == "SpatialPolygonsDataFrame" | class(Extent) == "data.frame") { # Extent check: if Extent input is a shapefile
      Shape <- Extent # save shapefile for use as Shape in masking covariate data
    } else { # if Extent is not a shape, then extent specification is already baked into Data
      Shape <- NULL # set Shape to NULL so it is ignored in download_DEM function when masking is applied
    } # end of Extent check
    Covs_ls <- download_DEM(Train_ras = Data, Target_res = Target_res, Shape = Shape, Buffer = Buffer, ID = ID, Keep_Temporary = Keep_Temporary, Dir = Dir, Source = Source)
    Covariates_coarse <- Covs_ls[[1]] # extract coarse covariates from download_DEM output
    Covariates_fine <- Covs_ls[[2]] # extract fine covariates from download_DEM output
  } # end of covariate check

  ## KRIGING FORMULA (assure that KrigingEquation is a formula object) ----
  KrigingEquation <- as.formula(KrigingEquation)

  ## CALL LIST (for storing how the function as called in the output) ----
  Call_ls <- list(
    Data = SummarizeRaster(Data),
    Covariates_coarse = SummarizeRaster(Covariates_coarse),
    Covariates_fine = SummarizeRaster(Covariates_fine),
    KrigingEquation = KrigingEquation,
    Cores = Cores,
    FileName = FileName,
    Keep_Temporary = Keep_Temporary,
    nmax = nmax,
    Data_Retrieval = Data_Retrieval
  )

  ## SANITY CHECKS (step into check_Krig function to catch most common error messages) ----
  Check_Product <- check_Krig(Data = Data, CovariatesCoarse = Covariates_coarse, CovariatesFine = Covariates_fine, KrigingEquation = KrigingEquation)
  KrigingEquation <- Check_Product[[1]] # extract KrigingEquation (this may have changed in check_Krig)
  DataSkips <- Check_Product[[2]] # extract which layers to skip due to missing data (this is unlikely to ever come into action)
  Terms <- unique(unlist(strsplit(labels(terms(KrigingEquation)), split = ":"))) # identify which layers of data are needed

  ## DATA REFORMATTING (Kriging requires spatially referenced data frames, reformatting from rasters happens here) ---
  Origin <- raster::as.data.frame(Covariates_coarse, xy = TRUE) # extract covariate layers
  Origin <- Origin[, c(1:2, which(colnames(Origin) %in% Terms))] # retain only columns containing terms

  Target <- raster::as.data.frame(Covariates_fine, xy = TRUE) # extract covariate layers
  Target <- Target[, c(1:2, which(colnames(Target) %in% Terms))] # retain only columns containing terms
  Target <- na.omit(Target)
  suppressWarnings(gridded(Target) <- ~ x + y) # establish a gridded data product ready for use in kriging
  Target@grid@cellsize[1] <- Target@grid@cellsize[2] # ensure that grid cells are square

  ## SET-UP TEMPORARY DIRECTORY (this is where kriged products of each layer will be saved) ----
  Dir.Temp <- file.path(Dir, paste("Kriging", FileName, sep = "_"))
  if (!dir.exists(Dir.Temp)) {
    dir.create(Dir.Temp)
  }

  ## KRIGING SPECIFICATION (this will be parsed and evaluated in parallel and non-parallel evaluations further down) ----
  looptext <- "
  OriginK <- cbind(Origin, raster::extract(x = Data[[Iter_Krige]], y = Origin[,1:2], df=TRUE)[, 2]) # combine data of current data layer with training covariate data
  OriginK <- na.omit(OriginK) # get rid of NA cells
  colnames(OriginK)[length(Terms)+3] <- c(terms(KrigingEquation)[[2]]) # assign column names
  suppressWarnings(gridded(OriginK) <-  ~x+y) # generate gridded product
  OriginK@grid@cellsize[1] <- OriginK@grid@cellsize[2] # ensure that grid cells are square

  Iter_Try = 0 # number of tries set to 0
  kriging_result <- NULL
  while(class(kriging_result)[1] != 'autoKrige' & Iter_Try < SingularTry){ # try kriging SingularTry times, this is because of a random process of variogram identification within the automap package that can fail on smaller datasets randomly when it isn't supposed to
    try(invisible(capture.output(kriging_result <- autoKrige(formula = KrigingEquation, input_data = OriginK, new_data = Target, nmax = nmax))), silent = TRUE)
    Iter_Try <- Iter_Try +1
  }
  if(class(kriging_result)[1] != 'autoKrige'){ # give error if kriging fails
    message(paste0('Kriging failed for layer ', Iter_Krige, '. Error message produced by autoKrige function: ', geterrmessage()))
  }

  ## retransform to raster
  try( # try fastest way - this fails with certain edge artefacts in meractor projection and is fixed by using rasterize
    Krig_ras <- raster(x = kriging_result$krige_output, layer = 1), # extract raster from kriging product
    silent = TRUE
  )
  try(
    Var_ras <- raster(x = kriging_result$krige_output, layer = 3), # extract raster from kriging product
    silent = TRUE
  )
  if(!exists('Krig_ras') & !exists('Var_ras')){
    Krig_ras <- rasterize(x = kriging_result$krige_output, y = Covariates_fine[[1]])[[2]] # extract raster from kriging product
    Var_ras <- rasterize(x = kriging_result$krige_output, y = Covariates_fine)[[4]] # extract raster from kriging product
  }
  crs(Krig_ras) <- crs(Data) # setting the crs according to the data
  crs(Var_ras) <- crs(Data) # setting the crs according to the data

  if(Cores == 1){
  Ras_Krig[[Iter_Krige]] <- Krig_ras
  Ras_Var[[Iter_Krige]] <- Var_ras
  } # stack kriged raster into raster list if non-parallel computing

  terra::writeCDF(x = as(brick(Krig_ras), 'SpatRaster'), filename = file.path(Dir.Temp, paste0(str_pad(Iter_Krige,4,'left','0'), '_data.nc')), overwrite = TRUE)
  # writeRaster(x = Krig_ras, filename = file.path(Dir.Temp, paste0(str_pad(Iter_Krige,4,'left','0'), '_data.nc')), overwrite = TRUE, format='CDF') # save kriged raster to temporary directory
  terra::writeCDF(x = as(brick(Var_ras), 'SpatRaster'), filename = file.path(Dir.Temp, paste0(str_pad(Iter_Krige,4,'left','0'), '_SE.nc')), overwrite = TRUE)
 # writeRaster(x = Var_ras, filename = file.path(Dir.Temp, paste0(str_pad(Iter_Krige,4,'left','0'), '_SE.nc')), overwrite = TRUE, format='CDF') # save kriged raster to temporary directory

  if(Cores == 1){ # core check: if processing non-parallel
    if(Count_Krige == 1){ # count check: if this was the first actual computation
      T_End <- Sys.time() # record time at which kriging was done for current layer
      Duration <- as.numeric(T_End)-as.numeric(T_Begin) # calculate how long it took to krig on layer
      message(paste('Kriging of remaining ', nlayers(Data)-Iter_Krige, ' data layers should finish around: ', as.POSIXlt(T_Begin + Duration*nlayers(Data), tz = Sys.timezone(location=TRUE)), sep='')) # console output with estimate of when the kriging should be done
      ProgBar <- txtProgressBar(min = 0, max = nlayers(Data), style = 3) # create progress bar when non-parallel processing
      Count_Krige <- Count_Krige + 1 # raise count by one so the stimator isn't called again
    } # end of count check
    setTxtProgressBar(ProgBar, Iter_Krige) # update progress bar with number of current layer
  } # end of core check
  "

  ## KRIGING PREPARATION (establishing objects which the kriging refers to) ----
  Ras_Krig <- as.list(rep(NA, nlayers(Data))) # establish an empty list which will be filled with kriged layers
  Ras_Var <- as.list(rep(NA, nlayers(Data))) # establish an empty list which will be filled with kriged layers

  if (verbose) {
    message("Commencing Kriging")
  }
  ## DATA SKIPS (if certain layers in the data are empty and need to be skipped, this is handled here) ---
  if (!is.null(DataSkips)) { # Skip check: if layers need to be skipped
    for (Iter_Skip in DataSkips) { # Skip loop: loop over all layers that need to be skipped
      Ras_Krig[[Iter_Skip]] <- Data[[Iter_Skip]] # add raw data (which should be empty) to list
      terra::writeCDF(x = as(brick(Ras_Krig[[Iter_Skip]]), "SpatRaster"), filename = file.path(Dir.Temp, str_pad(Iter_Skip, 4, "left", "0")), overwrite = TRUE)
      # writeRaster(x = Ras_Krig[[Iter_Skip]], filename = file.path(Dir.Temp, str_pad(Iter_Skip,4,'left','0')), overwrite = TRUE, format = 'CDF') # save raw layer to temporary directory, needed for loading back in when parallel processing
    } # end of Skip loop
    Layers_vec <- 1:nlayers(Data) # identify vector of all layers in data
    Compute_Layers <- Layers_vec[which(!Layers_vec %in% DataSkips)] # identify which layers can actually be computed on
  } else { # if we don't need to skip any layers
    Compute_Layers <- 1:nlayers(Data) # set computing layers to all layers in data
  } # end of Skip check


  ## ACTUAL KRIGING (carry out kriging according to user specifications either in parallel or on a single core) ----
  if (Cores > 1) { # Cores check: if parallel processing has been specified
    ### PARALLEL KRIGING ---
    ForeachObjects <- c("Dir.Temp", "Cores", "Data", "KrigingEquation", "Origin", "Target", "Covariates_coarse", "Covariates_fine", "Terms", "SingularTry", "nmax") # objects which are needed for each kriging run and are thus handed to each cluster unit
    pb <- txtProgressBar(max = length(Compute_Layers), style = 3)
    progress <- function(n) {
      setTxtProgressBar(pb, n)
    }
    opts <- list(progress = progress)
    cl <- makeCluster(Cores) # Assuming Cores node cluster
    registerDoSNOW(cl) # registering cores
    foreach(
      Iter_Krige = Compute_Layers, # kriging loop over all layers in Data, with condition (%:% when(...)) to only run if current layer is not present in Dir.Temp yet
      .packages = c("raster", "stringr", "automap", "ncdf4", "rgdal", "terra"), # import packages necessary to each itteration
      .export = ForeachObjects,
      .options.snow = opts
    ) %:% when(!paste0(str_pad(Iter_Krige, 4, "left", "0"), "_data.nc") %in% list.files(Dir.Temp)) %dopar% { # parallel kriging loop
      Ras_Krig <- eval(parse(text = looptext)) # evaluate the kriging specification per cluster unit per layer
    } # end of parallel kriging loop
    close(pb)
    stopCluster(cl) # close down cluster
    Files_krig <- list.files(Dir.Temp)[grep(pattern = "_data.nc", x = list.files(Dir.Temp))]
    Files_var <- list.files(Dir.Temp)[grep(pattern = "_SE.nc", x = list.files(Dir.Temp))]
    for (Iter_Load in 1:length(Files_krig)) { # load loop: load data from temporary files in Dir.Temp
      Ras_Krig[[Iter_Load]] <- raster(file.path(Dir.Temp, Files_krig[Iter_Load])) # load current temporary file and write contents to list of rasters
      Ras_Var[[Iter_Load]] <- raster(file.path(Dir.Temp, Files_var[Iter_Load])) # load current temporary file and write contents to list of rasters
    } # end of load loop
  } else { # if non-parallel processing has been specified
    ### NON-PARALLEL KRIGING ---
    Count_Krige <- 1 # Establish count variable which is targeted in kriging specification text for producing an estimator
    for (Iter_Krige in Compute_Layers) { # non-parallel kriging loop over all layers in Data
      if (paste0(str_pad(Iter_Krige, 4, "left", "0"), "_data.nc") %in% list.files(Dir.Temp)) { # file check: if this file has already been produced
        Ras_Krig[[Iter_Krige]] <- raster(file.path(Dir.Temp, paste0(str_pad(Iter_Krige, 4, "left", "0"), "_data.nc"))) # load already produced kriged file and save it to list of rasters
        Ras_Var[[Iter_Krige]] <- raster(file.path(Dir.Temp, paste0(str_pad(Iter_Krige, 4, "left", "0"), "_SE.nc")))
        if (!exists("ProgBar")) {
          ProgBar <- txtProgressBar(min = 0, max = nlayers(Data), style = 3)
        } # create progress bar when non-parallel processing}
        setTxtProgressBar(ProgBar, Iter_Krige) # update progress bar
        next() # jump to next layer
      } # end of file check
      T_Begin <- Sys.time() # record system time when layer kriging starts
      eval(parse(text = looptext)) # evaluate the kriging specification per layer
    } # end of non-parallel kriging loop
  } # end of Cores check

  ## SAVING FINAL PRODUCT ----
  if (is.null(DataSkips)) { # Skip check: if no layers needed to be skipped
    # convert list of kriged layers in actual rasterbrick of kriged layers
    names(Ras_Krig) <- names(Data)
    if (class(Ras_Krig) != "RasterBrick") {
      Ras_Krig <- brick(Ras_Krig)
    }
    Krig_terra <- as(Ras_Krig, "SpatRaster")
    names(Krig_terra) <- names(Data)
    terra::writeCDF(x = Krig_terra, filename = file.path(Dir, paste0(FileName, ".nc")), overwrite = TRUE)
    # writeRaster(x = Ras_Krig, filename = file.path(Dir, FileName), overwrite = TRUE, format="CDF") # save final product as raster
    # convert list of kriged layers in actual rasterbrick of kriged layers
    names(Ras_Var) <- names(Data)
    if (class(Ras_Var) != "RasterBrick") {
      Ras_Var <- brick(Ras_Var)
    }
    Var_terra <- as(Ras_Var, "SpatRaster")
    names(Var_terra) <- names(Data)

    terra::writeCDF(x = Var_terra, filename = file.path(Dir, paste0("SE_", paste0(FileName, ".nc"))), overwrite = TRUE)
    # writeRaster(x = Ras_Var, filename = file.path(Dir, paste0("SE_",FileName)), overwrite = TRUE, format="CDF") # save final product as raster
  } else { # if some layers needed to be skipped
    warning(paste0("Some of the layers in your raster could not be kriged. You will find all the individual layers (kriged and not kriged) in ", Dir, "."))
    Keep_Temporary <- TRUE # keep temporary files so kriged products are not deleted
  } # end of Skip check

  ### REMOVE FILES FROM HARD DRIVE ---
  if (Keep_Temporary == FALSE) { # cleanup check
    unlink(Dir.Temp, recursive = TRUE)
  } # end of cleanup check

  Krig_ls <- list(Ras_Krig, Ras_Var, Call_ls)
  names(Krig_ls) <- c("Kriging_Output", "Kriging_SE", "Call")
  return(Krig_ls) # return raster or list of layers
}
