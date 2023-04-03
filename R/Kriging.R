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
#' # Kriging the data sets prepared with the previous functions.
#' State_Krig <- krigR(
#'   Data = State_Raw, # data we want to krig as a raster object
#' Covariates_coarse = Covs_ls[[1]], # training covariate as a raster object
#' Covariates_fine = Covs_ls[[2]], # target covariate as a raster object
#' )
#'
#' ## PIPELINE (From Scratch)
#' #' # Downloading ERA5-Land air temperature reanalysis data in 12-hour intervals for 02/01/1995 - 04/01/1995 (DD/MM/YYYY), downloading and preparing GMTED 2010 covariate data, and kriging. API User and Key in this example are non-functional. Substitute with your user number and key to run this example. This example produces the same output as the example above.
#' Extent <- extent(c(11.8,15.1,50.1,51.7)) # roughly the extent of Saxony
#' API_User <- "..."
#' API_Key <- "..."
#' Pipe_Krig <- krigR(
#' Variable = "2m_temperature",
#' Type = "reanalysis",
#' DataSet = "era5-land",
#' DateStart = "1995-01-02",
#' DateStop = "1995-01-04",
#' TResolution = "hour",#
#' TStep = 12,
#' Extent = Extent,
#' API_User = API_User,
#' API_Key = API_Key,
#' Target_res = .02,
#' )
#' }
#'
#' @export
krigR <- function(Data = NULL, Covariates_coarse = NULL, Covariates_fine = NULL, KrigingEquation = "ERA ~ DEM", Cores = detectCores(), Dir = getwd(), FileName, Keep_Temporary = TRUE, SingularTry = 10, Variable, PrecipFix = FALSE, Type = "reanalysis", DataSet = "era5-land", DateStart, DateStop, TResolution = "month", TStep = 1, FUN = 'mean', Extent, Buffer = 0.5, ID = "ID", API_Key, API_User, Target_res, Source = "USGS", nmax = Inf,  TryDown = 10, verbose = TRUE, TimeOut = 36000, SingularDL = FALSE, ...){
  ## CALL LIST (for storing how the function as called in the output) ----
  if(is.null(Data)){
    Data_Retrieval <- list(Variable = Variable,
                           Type = Type,
                           PrecipFix = PrecipFix,
                           DataSet = DataSet,
                           DateStart = DateStart,
                           DateStop = DateStop,
                           TResolution = TResolution,
                           TStep = TStep,
                           Extent = Extent)
  }else{
    Data_Retrieval <- "None needed. Data was not queried via krigR function, but supplied by user."
  }
  ## CLIMATE DATA (call to download_ERA function if no Data set is specified) ----
  if(is.null(Data)){ # data check: if no data has been specified
    Data <- download_ERA(Variable = Variable, PrecipFix = PrecipFix, Type = Type, DataSet = DataSet, DateStart = DateStart, DateStop = DateStop, TResolution = TResolution, TStep = TStep, FUN = FUN, Extent = Extent, API_User = API_User, API_Key = API_Key, Dir = Dir, TryDown = TryDown, verbose = verbose, ID = ID, Cores = Cores, TimeOut = TimeOut, SingularDL = SingularDL)
  } # end of data check

  ## COVARIATE DATA (call to download_DEM function when no covariates are specified) ----
  if(is.null(Covariates_coarse) & is.null(Covariates_fine)){ # covariate check: if no covariates have been specified
    if(class(Extent) == "SpatialPolygonsDataFrame" | class(Extent) == "data.frame"){ # Extent check: if Extent input is a shapefile
      Shape <- Extent # save shapefile for use as Shape in masking covariate data
    }else{ # if Extent is not a shape, then extent specification is already baked into Data
      Shape <- NULL # set Shape to NULL so it is ignored in download_DEM function when masking is applied
    } # end of Extent check
    Covs_ls <- download_DEM(Train_ras = Data, Target_res = Target_res, Shape = Shape, Buffer = Buffer, ID = ID, Keep_Temporary = Keep_Temporary, Dir = Dir, Source = Source)
    Covariates_coarse <- Covs_ls[[1]] # extract coarse covariates from download_DEM output
    Covariates_fine <- Covs_ls[[2]] # extract fine covariates from download_DEM output
  } # end of covariate check

  ## KRIGING FORMULA (assure that KrigingEquation is a formula object) ----
  KrigingEquation <- as.formula(KrigingEquation)

  ## CALL LIST (for storing how the function as called in the output) ----
  Call_ls <- list(Data = SummarizeRaster(Data),
                  Covariates_coarse = SummarizeRaster(Covariates_coarse),
                  Covariates_fine = SummarizeRaster(Covariates_fine),
                  KrigingEquation = KrigingEquation,
                  Cores = Cores,
                  FileName = FileName,
                  Keep_Temporary = Keep_Temporary,
                  nmax = nmax,
                  Data_Retrieval = Data_Retrieval,
                  misc = ...)

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
  suppressWarnings(gridded(Target) <- ~x+y) # establish a gridded data product ready for use in kriging
  Target@grid@cellsize[1] <- Target@grid@cellsize[2] # ensure that grid cells are square

  ## SET-UP TEMPORARY DIRECTORY (this is where kriged products of each layer will be saved) ----
  Dir.Temp <- file.path(Dir, paste("Kriging", FileName, sep="_"))
  if(!dir.exists(Dir.Temp)){dir.create(Dir.Temp)}

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

  terra::writeCDF(x = as(Krig_ras, 'SpatRaster'), filename = file.path(Dir.Temp, paste0(str_pad(Iter_Krige,4,'left','0'), '_data.nc')), overwrite = TRUE)
  # writeRaster(x = Krig_ras, filename = file.path(Dir.Temp, paste0(str_pad(Iter_Krige,4,'left','0'), '_data.nc')), overwrite = TRUE, format='CDF') # save kriged raster to temporary directory
  terra::writeCDF(x = as(Var_ras, 'SpatRaster'), filename = file.path(Dir.Temp, paste0(str_pad(Iter_Krige,4,'left','0'), '_SE.nc')), overwrite = TRUE)
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

  if(verbose){message("Commencing Kriging")}
  ## DATA SKIPS (if certain layers in the data are empty and need to be skipped, this is handled here) ---
  if(!is.null(DataSkips)){ # Skip check: if layers need to be skipped
    for(Iter_Skip in DataSkips){ # Skip loop: loop over all layers that need to be skipped
      Ras_Krig[[Iter_Skip]] <- Data[[Iter_Skip]] # add raw data (which should be empty) to list
      terra::writeCDF(x = as(Ras_Krig[[Iter_Skip]], 'SpatRaster'), filename = file.path(Dir.Temp, str_pad(Iter_Skip,4,'left','0')), overwrite = TRUE)
      # writeRaster(x = Ras_Krig[[Iter_Skip]], filename = file.path(Dir.Temp, str_pad(Iter_Skip,4,'left','0')), overwrite = TRUE, format = 'CDF') # save raw layer to temporary directory, needed for loading back in when parallel processing
    } # end of Skip loop
    Layers_vec <- 1:nlayers(Data) # identify vector of all layers in data
    Compute_Layers <- Layers_vec[which(!Layers_vec %in% DataSkips)] # identify which layers can actually be computed on
  }else{ # if we don't need to skip any layers
    Compute_Layers <- 1:nlayers(Data) # set computing layers to all layers in data
  } # end of Skip check


  ## ACTUAL KRIGING (carry out kriging according to user specifications either in parallel or on a single core) ----
  if(Cores > 1){ # Cores check: if parallel processing has been specified
    ### PARALLEL KRIGING ---
    ForeachObjects <- c("Dir.Temp", "Cores", "Data", "KrigingEquation", "Origin", "Target", "Covariates_coarse", "Covariates_fine", "Terms", "SingularTry", "nmax") # objects which are needed for each kriging run and are thus handed to each cluster unit
    pb <- txtProgressBar(max = length(Compute_Layers), style = 3)
    progress <- function(n){setTxtProgressBar(pb, n)}
    opts <- list(progress = progress)
    cl <- makeCluster(Cores) # Assuming Cores node cluster
    registerDoSNOW(cl) # registering cores
    foreach(Iter_Krige = Compute_Layers, # kriging loop over all layers in Data, with condition (%:% when(...)) to only run if current layer is not present in Dir.Temp yet
            .packages = c("raster", "stringr", "automap", "ncdf4", "rgdal", "terra"), # import packages necessary to each itteration
            .export = ForeachObjects,
            .options.snow = opts) %:% when(!paste0(str_pad(Iter_Krige,4,"left","0"), '_data.nc') %in% list.files(Dir.Temp)) %dopar% { # parallel kriging loop
              Ras_Krig <- eval(parse(text=looptext)) # evaluate the kriging specification per cluster unit per layer
            } # end of parallel kriging loop
    close(pb)
    stopCluster(cl) # close down cluster
    Files_krig <- list.files(Dir.Temp)[grep(pattern = "_data.nc", x = list.files(Dir.Temp))]
    Files_var <- list.files(Dir.Temp)[grep(pattern = "_SE.nc", x = list.files(Dir.Temp))]
    for(Iter_Load in 1:length(Files_krig)){ # load loop: load data from temporary files in Dir.Temp
      Ras_Krig[[Iter_Load]] <- raster(file.path(Dir.Temp, Files_krig[Iter_Load])) # load current temporary file and write contents to list of rasters
      Ras_Var[[Iter_Load]] <- raster(file.path(Dir.Temp, Files_var[Iter_Load])) # load current temporary file and write contents to list of rasters
    } # end of load loop
  }else{ # if non-parallel processing has been specified
    ### NON-PARALLEL KRIGING ---
    Count_Krige <- 1 # Establish count variable which is targeted in kriging specification text for producing an estimator
    for(Iter_Krige in Compute_Layers){ # non-parallel kriging loop over all layers in Data
      if(paste0(str_pad(Iter_Krige,4,'left','0'), '_data.nc') %in% list.files(Dir.Temp)){ # file check: if this file has already been produced
        Ras_Krig[[Iter_Krige]] <- raster(file.path(Dir.Temp, paste0(str_pad(Iter_Krige,4,'left','0'), '_data.nc'))) # load already produced kriged file and save it to list of rasters
        Ras_Var[[Iter_Krige]] <- raster(file.path(Dir.Temp, paste0(str_pad(Iter_Krige,4,'left','0'), '_SE.nc')))
        if(!exists("ProgBar")){ProgBar <- txtProgressBar(min = 0, max = nlayers(Data), style = 3)} # create progress bar when non-parallel processing}
        setTxtProgressBar(ProgBar, Iter_Krige) # update progress bar
        next() # jump to next layer
      } # end of file check
      T_Begin <- Sys.time() # record system time when layer kriging starts
      eval(parse(text=looptext)) # evaluate the kriging specification per layer
    } # end of non-parallel kriging loop
  } # end of Cores check

  ## SAVING FINAL PRODUCT ----
  if(is.null(DataSkips)){ # Skip check: if no layers needed to be skipped
    Ras_Krig <- brick(Ras_Krig) # convert list of kriged layers in actual rasterbrick of kriged layers
    terra::writeCDF(x = as(Ras_Krig, "SpatRaster"), filename = file.path(Dir, FileName), overwrite = TRUE)
    # writeRaster(x = Ras_Krig, filename = file.path(Dir, FileName), overwrite = TRUE, format="CDF") # save final product as raster
    Ras_Var <- brick(Ras_Var) # convert list of kriged layers in actual rasterbrick of kriged layers
    terra::writeCDF(x = as(Ras_Var, "SpatRaster"), filename = file.path(Dir, paste0("SE_", FileName)), overwrite = TRUE)
    # writeRaster(x = Ras_Var, filename = file.path(Dir, paste0("SE_",FileName)), overwrite = TRUE, format="CDF") # save final product as raster
  }else{ # if some layers needed to be skipped
    warning(paste0("Some of the layers in your raster could not be kriged. You will find all the individual layers (kriged and not kriged) in ", Dir, "."))
    Keep_Temporary <- TRUE # keep temporary files so kriged products are not deleted
  } # end of Skip check

  ### REMOVE FILES FROM HARD DRIVE ---
  if(Keep_Temporary == FALSE){ # cleanup check
    unlink(Dir.Temp, recursive = TRUE)
  }  # end of cleanup check

  Krig_ls <- list(Ras_Krig, Ras_Var, Call_ls)
  names(Krig_ls) <- c("Kriging_Output", "Kriging_SE", "Call")
  return(Krig_ls) # return raster or list of layers
}
