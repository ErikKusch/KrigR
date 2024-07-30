#' (multi-core) Kriging
#'
#' This function statistically downscales input data using covariate data and the kriging methodology.
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
#' @param nmax Optional. Controls local kriging. Number of nearest observations to be used kriging of each observation. Default is to use all available (Inf). You can specify as a number (numeric).
#' @param verbose Optional, logical. Whether to report progress of data download (if queried) in the console or not.
#'
#' @return A list object containing the downscaled data as well as the standard error for downscaling as well as the call to the krigR function, and two NETCDF (.nc) file in the specified directory which are the two data contents of the aforementioned list. A temporary directory is populated with individual NETCDF (.nc) files throughout the runtime of krigR which is deleted upon completion if Keep_Temporary = TRUE and all layers in the Data raster object were kriged successfully.
#'
#' @examples
#' \dontrun{
#'
#' }
#' @export
Kriging <- function(
    Data,
    Covariates_training,
    Covariates_target,
    Equation = NULL,
    Cores = detectCores(),
    Dir = getwd(),
    FileName,
    FileExtension,
    Keep_Temporary = TRUE,
    nmax = Inf,
    verbose = TRUE
    ){

  ## Run Preparations ===============
  ## if no equation is specified, assign additive combination of variables in training covariates
  if(is.null(Equation)){Equation <- paste(terra::varnames(Covariates_training), collapse = " + ")}
  ### assure that KrigingEquation is a formula object
  KrigingEquation <- as.formula(paste("Data ~", Equation))
  ### Metadata
  KrigRCall <- match.call()
  Meta_vec <- as.character(KrigRCall)
  names(Meta_vec) <- names(KrigRCall)
  Meta_vec <- c(
    "Citation" = paste0("Data kriged using KrigR (DOI:10.1088/1748-9326/ac48b3) on ", Sys.time()),
    "KrigRCall" = Meta_vec
  )
  ### Temporary Directory
  Dir.Temp <- file.path(Dir, paste("TEMP-Kriging", FileName, sep="_"))
  if(!dir.exists(Dir.Temp)){dir.create(Dir.Temp)}
  ### Establishing objects which the kriging execution refers to
  Ras_Krig <- as.list(rep(NA, terra::nlyr(Data))) # establish an empty list which will be filled with kriged layers
  Ras_SE <- as.list(rep(NA, terra::nlyr(Data))) # establish an empty list which will be filled with kriged layers
  ### OnExit commands

  ## Catching Most Frequent Issues ===============
  Check_Product <- Check.Krig(Data = Data, CovariatesCoarse = Covariates_training, CovariatesFine = Covariates_target, KrigingEquation = KrigingEquation)
  KrigingEquation <- Check_Product[[1]] # extract KrigingEquation (this may have changed in check_Krig)
  # DataSkips <- Check_Product[[2]] # extract which layers to skip due to missing data (this is unlikely to ever come into action)
  Terms <- unique(unlist(strsplit(labels(terms(KrigingEquation)), split = ":"))) # identify which layers of data are needed

  ## Data Reformatting ===============
  # (Kriging requires spatially referenced data frames, reformatting from rasters happens here)
  Origin <- as.data.frame(Covariates_training, xy = TRUE)
  colnames(Origin)[-1:-2] <- terra::varnames(Covariates_training)
  Origin <- Origin[, c(1:2, which(colnames(Origin) %in% Terms))] # retain only columns containing terms
  Origin <- sf::st_as_sf(Origin, coords = c("x", "y"))

  Target <- as.data.frame(Covariates_target, xy = TRUE)
  colnames(Target)[-1:-2] <- terra::varnames(Covariates_target)
  Target <- Target[, c(1:2, which(colnames(Target) %in% Terms))] # retain only columns containing terms
  Target <- sf::st_as_sf(Target, coords = c("x", "y"))

  ## Kriging Specification ===============
  # (this will be parsed and evaluated in parallel and non-parallel evaluations further down)
  looptext <- "

  #### NEW STUFF - ready to work
  DataSF <- as.data.frame(Data[[1]], xy = TRUE)
  colnames(DataSF)[-1:-2] <- 'Data'
  DataSF <- sf::st_as_sf(DataSF, coords = c('x', 'y'))
  KrigData <- cbind(Origin, DataSF$Data)
  colnames(KrigData)[ncol(Origin)] <- 'Data'
  KrigTest <- autoKrige(formula = KrigingEquation, input_data = na.omit(KrigData), new_data = Target, nmax = nmax)
  Pred_rast <- terra::rast(x = cbind(sf::st_coordinates(KrigTest$krige_output), sf::st_drop_geometry(KrigTest$krige_output)$var1.pred), type = 'xyz')
  StDe_rast <- terra::rast(x = cbind(sf::st_coordinates(KrigTest$krige_output), sf::st_drop_geometry(KrigTest$krige_output)$var1.stdev), type = 'xyz')

  #### OLD STUFF - combine with new
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

  ## Kriging Execution ===============
  if(verbose){message("Commencing Kriging")}
  # carry out kriging according to user specifications either in parallel or on a single core
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

  ## Data Saving ===============
  if(is.null(DataSkips)){ # Skip check: if no layers needed to be skipped
    # convert list of kriged layers in actual rasterbrick of kriged layers
    names(Ras_Krig) <- names(Data)
    if(class(Ras_Krig) != "RasterBrick"){Ras_Krig <- brick(Ras_Krig)}
    Krig_terra <- as(Ras_Krig, "SpatRaster")
    names(Krig_terra) <- names(Data)
    terra::writeCDF(x = Krig_terra, filename = file.path(Dir, paste0(FileName, ".nc")), overwrite = TRUE)
    # writeRaster(x = Ras_Krig, filename = file.path(Dir, FileName), overwrite = TRUE, format="CDF") # save final product as raster
    # convert list of kriged layers in actual rasterbrick of kriged layers
    names(Ras_Var) <- names(Data)
    if(class(Ras_Var) != "RasterBrick"){Ras_Var <- brick(Ras_Var)}
    Var_terra <- as(Ras_Var, "SpatRaster")
    names(Var_terra) <- names(Data)

    terra::writeCDF(x = Var_terra, filename = file.path(Dir, paste0("SE_", paste0(FileName, ".nc"))), overwrite = TRUE)
    # writeRaster(x = Ras_Var, filename = file.path(Dir, paste0("SE_",FileName)), overwrite = TRUE, format="CDF") # save final product as raster
  }else{ # if some layers needed to be skipped
    warning(paste0("Some of the layers in your raster could not be kriged. You will find all the individual layers (kriged and not kriged) in ", Dir, "."))
    Keep_Temporary <- TRUE # keep temporary files so kriged products are not deleted
  } # end of Skip check

  ## Removing Temporary Files ===============
  if(Keep_Temporary == FALSE){ # cleanup check
    unlink(Dir.Temp, recursive = TRUE)
  }  # end of cleanup check

  ## Data Return ===============
  Krig_ls <- list(Ras_Krig, Ras_Var, Call_ls)
  names(Krig_ls) <- c("Kriging_Output", "Kriging_SE", "Call")
  return(Krig_ls) # return raster or list of layers
}
