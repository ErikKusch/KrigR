#' (multi-core) Kriging
#'
#' This function statistically downscales input data using covariate data and the kriging methodology. The function can be run in two ways:
#' \enumerate{
#' \item \strong{By Itself}: Use the arguments Data, Covariates_coarse, Covariates_fine when you already have raster files for your data which is to be downscaled as well as covariate raster data.
#' \item \strong{From Scratch}: Use the arguments Variable, Type, DataSet, DateStart, DateStop, TResolution, TStep, Extent, Dir, FileName, API_Key, API_User, and arget_res. By doing so, krigR will call the functions download_ERA() and download_DEM() for one coherent kriging workflow. Note that this process does not work when targetting UERRA data.
#' }
#' Use optional arguments such as Dir, FileName, Keep_Temporary, SingularTry, KrigingEquation and Cores for ease of use, substituion of non-GMTED2010 covariates, and parallel processing.
#'
#' @param Data Raster file which is to be downscaled.
#' @param Covariates_coarse Raster file containing covariates at training resolution.
#' @param Covariates_fine Raster file containing covariates at target resolution.
#' @param KrigingEquation Formula or character string specifying which covariates to use and how. Layer names in Covariates_coarse and Covariates_fine need to match Parameters in this formula. Needs to start with "X ~ ". X can read anything you like.
#' @param Dir Optional. Directory specifying where to place final kriged product. Default is current working directory.
#' @param FileName Optional. A file name for the netcdf produced. Default is a combination parameters in the function call.
#' @param Keep_Temporary Logical, whether to delete individual kriging products of layers in Data after processing. Default is TRUE.
#' @param Cores Numeric. How many cores to use. If you want output to your console during the process, use Cores == 1. Paralell processing is carried out when Cores is bigger than 1. Default is detecting all cores of your machine.
#' @param SingularTry Numeric. How often to try kriging of each layer of the input. This usually gets around issues of singular covariance matrices in the kriging process, but takes some time. Default is 10
#' @param Variable Optional, calls download_DEM(). ERA5(Land)-contained climate variable. See output of Variable_List() for possible values.
#' @param Type  Optional. Whether to download reanalysis ('reanalysis') or ensemble ('ensemble_members', 'ensemble_mean', or 'ensemble_spread') data. Passed on to download_ERA.
#' @param DataSet Optional. Which ERA5 data set to download data from. 'era5' or 'era5-land'. Passed on to download_ERA.
#' @param DateStart Optional. Date ('YYYY-MM-DD') at which to start time series of downloaded data. Passed on to download_ERA.
#' @param DateStop Optional. Date ('YYYY-MM-DD') at which to stop time series of downloaded data. Passed on to download_ERA.
#' @param TResolution Optional. Temporal resolution of final product. hour', 'day', 'month'. Passed on to download_ERA.
#' @param TStep Optional. Which time steps (numeric) to consider for temporal resolution. Passed on to download_ERA.
#' @param Extent Optional. Download data according to rectangular bounding box. Specify as extent object (obtained via raster::extent()). Alternatively, a raster or a SpatialPolygonsDataFrameobject. If Extent is a SpatialPolygonsDataFrame, this will be treated as a shapefile and the output will be cropped and masked to this shapefile. Passed on to download_ERA and downbload_DEM.
#' @param Target_res Optional. The target resolution for the kriging step (i.e. wich resolution to downscale to). An object as specified/produced by raster::res(). Passed on to download_DEM.
#' @param API_Key Optional. ECMWF cds API key. Passed on to download_ERA.
#' @param API_User Optional. ECMWF cds user number. Passed on to download_ERA.
#' @return A list object containing the downscaled data as well as the standard error for downscaling, and two NETCDF (.nc) file in the specified directory which are the contents of the aforementioned list.
#' @examples
#' \dontrun{
#' # Downloading and downscaling ERA5-Land air temperature reanalysis data in monthly intervals for the entire year of 2000 for Germany. API User and Key in this example are non-functional. Substitute with your user number and key to run this example.
#' krigR(Variable = '2m_temperature', Type = 'reanalysis', DataSet = 'era5-land', DateStart = '2000-01-01', DateStop = '2000-12-31', TResolution = 'month', TStep = 1, Extent = extent(6,15,47,55), API_User = NULL, API_Key = NULL, Target_res = .01, Cores = 4, FileName = "KrigingOutput.nc", Dir = file.path(getwd(), "KrigRTesting"))
#' }
#'
#' @export
krigR <- function(Data = NULL, Covariates_coarse = NULL, Covariates_fine = NULL, KrigingEquation = "ERA ~ DEM", Cores = detectCores(), Dir = getwd(), FileName, Keep_Temporary = TRUE, SingularTry = 10, Variable, Type, DataSet, DateStart, DateStop, TResolution, TStep, Extent, API_Key, API_User, Target_res){
  ## CLIMATE DATA (call to download_ERA function if no Data set is specified) ----
  if(is.null(Data)){ # data check: if no data has been specified
    Data <- download_ERA(Variable = Variable, Type = Type, DataSet = DataSet, DateStart = DateStart, DateStop = DateStop, TResolution = TResolution, TStep = TStep, Extent = Extent, API_User = API_User, API_Key = API_Key, Dir = Dir)
  } # end of data check

  ## COVARIATE DATA (call to download_DEM function when no covariates are specified) ----
  if(is.null(Covariates_coarse) & is.null(Covariates_fine)){ # covariate check: if no covariates have been specified
    if(class(Extent) == "SpatialPolygonsDataFrame"){ # Extent check: if Extent input is a shapefile
      Shape <- Extent # save shapefile for use as Shape in masking covariate data
    }else{ # if Extent is not a shape, then extent specification is already baked into Data
      Shape <- NULL # set Shape to NULL so it is ignored in download_DEM function when masking is applied
    } # end of Extent check
    Covs_ls <- download_DEM(Train_ras = Data, Target_res = Target_res, Shape = Shape, Keep_Temporary = Keep_Temporary, Dir = Dir)
    Covariates_coarse <- Covs_ls[[1]] # extract coarse covariates from download_DEM output
    Covariates_fine <- Covs_ls[[2]] # extract fine covariates from download_DEM output
  } # end of covariate check

  ## KRIGING FORMULA (assure that KrigingEquation is a formula object) ----
  KrigingEquation <- as.formula(KrigingEquation)

  ## SANITY CHECKS (step into check_Krig function to catch most common error messages) ----
  Check_Product <- check_Krig(Data = Data, CovariatesCoarse = Covariates_coarse, CovariatesFine = Covariates_fine, KrigingEquation = KrigingEquation)
  KrigingEquation <- Check_Product[[1]] # extract KrigingEquation (this may have changed in check_Krig)
  DataSkips <- Check_Product[[2]] # extract which layers to skip due to missing data (this is unlikely to ever come into action)
  Terms <- unique(unlist(strsplit(labels(terms(KrigingEquation)), split = ":"))) # identify which layers of data are needed

  ## DATA REFORMATTING (Kriging requires spatially referenced data frames, reformatting from rasters happens here) ---
  Origin <- raster::as.data.frame(Covariates_coarse[[which(names(Covariates_coarse) == Terms[[1]])]], xy = TRUE) # extract first targeted covariate layer
  Origin[, 3] <- extract(x = Covariates_coarse[[which(names(Covariates_coarse) == Terms[[1]])]], y = Origin[,1:2], df=TRUE)[, 2] # extract pixel data of locations identified above
  if(length(Terms) > 1){ # coarse layer check: if covariates file has more than 1 layer
    for(Iter_Coarse in 2:length(Terms)){ # loop over layers in coarse covariates raster
      Covariate <- extract(x = Covariates_coarse[[which(names(Covariates_coarse) == Terms[[Iter_Coarse]])]], y = Origin[,1:2], df=TRUE)[, 2] # extract data of current layer
      Origin <- cbind(Origin, Covariate) # only append data portion of covariate layer
    } # end of layer loop
  } # end of coarse layer check
  colnames(Origin) <- c("x","y", Terms) # assign column names from layer names

  Target <- raster::as.data.frame(Covariates_fine[[which(names(Covariates_fine) == Terms[[1]])]], xy = TRUE) # extract first covariate layer
  Target[, 3] <- extract(x = Covariates_fine[[which(names(Covariates_fine) == Terms[[1]])]], y = Target[,1:2], df=TRUE)[, 2] # extract pixel data of locations identified above
  if(length(Terms) > 1){ # fine layer check: if covariates file has more than 1 layer
    for(Iter_Fine in 2:length(Terms)){ # loop over layers in fine covariates raster
      Covariate <- extract(x = Covariates_fine[[which(names(Covariates_fine) == Terms[[Iter_Fine]])]], y = Target[,1:2], df=TRUE)[, 2] # extract data of current layer
      Target <- cbind(Target, Covariate) # append data
    } # end of layer loop
  } # end of fine layer check
  colnames(Target) <- c("x","y", Terms)  # assign column names from layer names
  Target <- na.omit(Target)
  suppressWarnings(gridded(Target) <- ~x+y) # establish a gridded data product ready for use in kriging
  Target@grid@cellsize[1] <- Target@grid@cellsize[2] # ensure that grid cells are square

  ## SET-UP TEMPORARY DIRECTORY (this is where kriged products of each layer will be saved) ----
  Dir.Temp <- file.path(Dir, paste("Kriging", FileName, sep="_"))
  if(!dir.exists(Dir.Temp)){dir.create(Dir.Temp)}

  ## KRIGING SPECIFICATION (this will be parsed and evaluated in parallel and non-parallel evaluations further down) ----
  looptext <- "
  OriginK <- cbind(Origin, extract(x = Data[[Iter_Krige]], y = Origin[,1:2], df=TRUE)[, 2]) # combine data of current data layer with training covariate data
  OriginK <- na.omit(OriginK) # get rid of NA cells
  colnames(OriginK) <- c('x','y', Terms, terms(KrigingEquation)[[2]]) # assign column names
  suppressWarnings(gridded(OriginK) <-  ~x+y) # generate gridded product
  OriginK@grid@cellsize[1] <- OriginK@grid@cellsize[2] # ensure that grid cells are square

  Iter_Try = 0 # number of tries set to 0
  kriging_result <- NULL
  while(class(kriging_result)[1] != 'autoKrige' & Iter_Try < SingularTry){ # try kriging SingularTry times, this is because of a random process of variogram identification within the automap package that can fail on smaller datasets randomly when it isn't supposed to
    try(invisible(capture.output(kriging_result <- autoKrige(KrigingEquation, OriginK, Target, verbose = FALSE))), silent = TRUE)
    Iter_Try <- Iter_Try +1
  }
  if(class(kriging_result)[1] != 'autoKrige'){ # give error if kriging fails
    print(paste0('Kriging failed for layer ', Iter_Krige, '. Error message produced by autoKrige function: ', geterrmessage()))
  }

  Krig_ras <- raster(kriging_result$krige_output) # extract raster from kriging product
  crs(Krig_ras) <- crs(Data) # setting the crs according to the data
  Var_ras <- stack(kriging_result$krige_output)[[3]] # extract raster from kriging product
  crs(Var_ras) <- crs(Data) # setting the crs according to the data

  if(Cores == 1){Ras_Krig[[Iter_Krige]] <- Krig_ras} # stack kriged raster into raster list if non-parallel computing
 writeRaster(x = Krig_ras, filename = file.path(Dir.Temp, paste0(str_pad(Iter_Krige,4,'left','0'), '_data.nc')), overwrite = TRUE, format='CDF') # save kriged raster to temporary directory
 writeRaster(x = Var_ras, filename = file.path(Dir.Temp, paste0(str_pad(Iter_Krige,4,'left','0'), '_SE.nc')), overwrite = TRUE, format='CDF') # save kriged raster to temporary directory

  if(Cores == 1){ # core check: if processing non-parallel
    if(Count_Krige == 1){ # count check: if this was the first actual computation
      T_End <- Sys.time() # record time at which kriging was done for current layer
      Duration <- as.numeric(T_End)-as.numeric(T_Begin) # calculate how long it took to krig on layer
      print(paste('Kriging of remaining ', nlayers(Data)-Iter_Krige, ' data layers should finish around: ', as.POSIXlt(T_Begin + Duration*(nlayers(Data)-Iter_Krige), tz = Sys.timezone(location=TRUE)), sep='')) # console output with estimate of when the kriging should be done
      Count_Krige <- Count_Krige + 1 # raise count by one so the stimator isn't called again
    } # end of count check
    setTxtProgressBar(ProgBar, Iter_Krige) # update progress bar with number of current layer
  } # end of core check
  "

  ## KRIGING PREPARATION (establishing objects which the kriging refers to) ----
  Ras_Krig <- as.list(rep(NA, nlayers(Data))) # establish an empty list which will be filled with kriged layers
  Ras_Var <- as.list(rep(NA, nlayers(Data))) # establish an empty list which will be filled with kriged layers
  if(Cores == 1){ProgBar <- txtProgressBar(min = 0, max = nlayers(Data), style = 3)} # create progress bar when non-parallel processing

  ## DATA SKIPS (if certain layers in the data are empty and need to be skipped, this is handled here) ---
  if(!is.null(DataSkips)){ # Skip check: if layers need to be skipped
    for(Iter_Skip in DataSkips){ # Skip loop: loop over all layers that need to be skipped
      Ras_Krig[[Iter_Skip]] <- Data[[Iter_Skip]] # add raw data (which should be empty) to list
      writeRaster(x = Ras_Krig[[Iter_Skip]], filename = file.path(Dir.Temp, str_pad(Iter_Skip,4,'left','0')), overwrite = TRUE, format = "CDF") # save raw layer to temporary directory, needed for loading back in when parallel processing
    } # end of Skip loop
    Layers_vec <- 1:nlayers(Data) # identify vector of all layers in data
    Compute_Layers <- Layers_vec[which(!Layers_vec %in% DataSkips)] # identify which layers can actually be computed on
  }else{ # if we don't need to skip any layers
    Compute_Layers <- 1:nlayers(Data) # set computing layers to all layers in data
  } # end of Skip check


  ## ACTUAL KRIGING (carry out kriging according to user specifications either in parallel or on a single core) ----
  if(Cores > 1){ # Cores check: if parallel processing has been specified
    ### PARALLEL KRIGING ---
    ForeachObjects <- c("Dir.Temp", "Cores", "Data", "KrigingEquation", "Origin", "Target", "Covariates_coarse", "Covariates_fine", "Terms", "SingularTry") # objects which are needed for each kriging run and are thus handed to each cluster unit
    cl <- makeCluster(Cores) # Assuming Cores node cluster
    registerDoParallel(cl) # registering cores
    foreach(Iter_Krige = Compute_Layers, # kriging loop over all layers in Data, with condition (%:% when(...)) to only run if current layer is not present in Dir.Temp yet
            .packages = c("raster", "stringr", "automap", "ncdf4", "rgdal"), # import packages necessary to each itteration
            .export = ForeachObjects) %:% when(!paste0(str_pad(Iter_Krige,4,"left","0"), '_data.nc') %in% list.files(Dir.Temp)) %dopar% { # parallel kriging loop
              Ras_Krig <- eval(parse(text=looptext)) # evaluate the kriging specification per cluster unit per layer
            } # end of parallel kriging loop
    stopCluster(cl) # close down cluster
    for(Iter_Load in 1:length(list.files(Dir.Temp))){ # load loop: load data from temporary files in Dir.Temp
      Files_krig <- list.files(Dir.Temp)[grep(pattern = "_data.nc", x = list.files(Dir.Temp))]
      Files_var <- list.files(Dir.Temp)[grep(pattern = "_SE.nc", x = list.files(Dir.Temp))]
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
    writeRaster(x = Ras_Krig, filename = file.path(Dir, FileName), overwrite = TRUE, format="CDF") # save final product as raster
    Ras_Var <- brick(Ras_Var) # convert list of kriged layers in actual rasterbrick of kriged layers
    writeRaster(x = Ras_Var, filename = file.path(Dir, paste0("SE_",FileName)), overwrite = TRUE, format="CDF") # save final product as raster
  } # end of Skip check

  ### REMOVE FILES FROM HARD DRIVE ---
  if(Keep_Temporary == FALSE){ # cleanup check
    unlink(Dir.Temp, recursive = TRUE)
  }  # end of cleanup check

  Krig_ls <- list(Ras_Krig, Ras_Var)
  names(Krig_ls) <- c("Kriging_Output", "Kriging_SE")
  return(Krig_ls) # return raster or list of layers
}
