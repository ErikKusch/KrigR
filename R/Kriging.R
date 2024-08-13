#' (multi-core) Kriging
#'
#' This function statistically downscales input data using covariate data and the kriging methodology.
#' Use optional arguments such as Dir, Keep_Temporary, KrigingEquation, nmax and Cores for ease of use, substitution of non-default covariates, localisation of kriging, and parallel processing.
#'
#' @param Data SpatRaster which is to be downscaled.
#' @param Covariates_training SpatRaster containing covariates at training resolution.
#' @param Covariates_target SpatRaster containing covariates at target resolution.
#' @param Equation Formula or character string specifying which covariates to use and how. Layer names in Covariates_training and Covariates_target need to match parameters in this formula. Do not include ". ~", just supply the righthand side of this formula like so: "Covariate1+Covariate2" or "Covariate1*Covariate2", etc.
#' @param Cores Numeric. How many cores to use. Parallel processing is carried out when Cores is bigger than 1. Default is detecting all cores of your machine.
#' @param nmax NUmeric. Controls local kriging. Number of nearest observations to be used kriging of each observation. Default is to use all available (Inf). You can specify as a number (numeric).
#' @param Dir Character/Directory Pointer. Directory specifying where to place final kriged product. Default is current working directory.
#' @param FileName Character. A file name for the produced files.
#' @param FileExtension Character. A file extension for the produced file. Supported values are ".nc" (default) and ".tif" (better support for metadata).
#' @param Keep_Temporary Logical, whether to delete individual kriging products of layers in Data after processing. Default is TRUE. These temporary files are stored in a newly created directory in Dir which is pre-pended with "TEMP-" and is deleted if Keep_Temporary = FALSE upon completion.
#' @param verbose Optional, logical. Whether to report progress of data download (if queried) in the console or not.
#'
#' @importFrom progress progress_bar
#' @importFrom terra nlyr
#' @importFrom terra crs
#' @importFrom terra varnames
#' @importFrom tools file_path_sans_ext
#' @importFrom terra rast
#' @importFrom terra units
#' @importFrom terra metags
#' @importFrom terra writeRaster
#' @importFrom terra writeCDF
#' @importFrom terra time
#' @importFrom sf st_as_sf
#' @importFrom sf st_drop_geometry
#' @importFrom sf st_coordinates
#' @importFrom sf st_as_sf
#' @importFrom automap autoKrige
#' @importFrom stringr str_pad
#' @importFrom doSNOW registerDoSNOW
#' @importFrom parallel makeCluster
#' @importFrom parallel stopCluster
#' @importFrom foreach %dopar%
#' @importFrom foreach foreach
#'
#' @return A list object containing SpatRasters reporting (1) the downscaled data as well as  (2) the standard deviation for downscaling. Also produces two files of specified extension in the specified directory which are the two data contents of the aforementioned list. A temporary directory is populated with individual files during the execution of this function which is deleted upon completion if Keep_Temporary = FALSE and all layers in the Data raster object were kriged successfully.
#'
#' The produced SpatRasters contains metadata/attributes as a named vector that can be retrieved with terra::metags(...):
#' \itemize{
#' \item{Citation}{ - A string which to use for in-line citation of the data product obtained with Kriging}.
#' \item{KrigRCall.X}{ - Arguments passed to the Kriging function that produced the file}.
#' }
#'
#' \strong{ATTENTION:} If data is loaded again from disk at a later point with a different function, take note that citation and KrigR-call metadata will not be loaded properly from a .nc when loading data through a different function. Kriging() handles these .nc specific issues when loading .nc files created previously with Kriging() from disk.
#'
#' @seealso \code{\link{CovariateSetup}}.
#'
#' @examples
#' \dontrun{
#' ## Kriging using pre-fab data with a rectangular extent and a fives layers of data with parallel processing
#' ### Loading data
#' CDS_rast <- terra::rast(system.file("extdata", "CentralNorway.nc", package="KrigR"))
#' Cov_train <- terra::rast(system.file("extdata", "Covariates_Train.nc", package="KrigR"))
#' Cov_target <- terra::rast(system.file("extdata", "Covariates_Target.nc", package="KrigR"))
#'
#' ### kriging itself
#' ExtentKrig <- Kriging(
#'   Data = CDS_rast,
#'   Covariates_training = Cov_train,
#'   Covariates_target = Cov_target,
#'   Equation = "GMTED2010",
#'   Cores = 2,
#'   FileName = "KrigTest1",
#'   FileExtension = ".nc",
#'   Keep_Temporary = TRUE,
#'   nmax = 40,
#'   verbose = TRUE
#' )
#'
#' ## Kriging using full KrigR pipeline with shapefile data
#' ### Shapefile loading
#' data("Jotunheimen_poly")
#' ### CDS data download
#' Qsoil_rast <- CDownloadS(
#'   Variable = "Volumetric soil water layer 1", # can also specify as "volumetric_soil_water_layer_1"
#'   # time-window, default set to range of dataset-type
#'   DateStart = "1995-01-01 00:00",
#'   DateStop = "1995-01-03 23:00",
#'   TZone = "CET",
#'   # temporal aggregation
#'   TResolution = "day",
#'   # spatial
#'   Extent = Jotunheimen_poly,
#'   # file storing
#'   FileName = "KrigTest2_Raw",
#'   # API credentials
#'   API_User = API_User,
#'   API_Key = API_Key
#' )
#'
#' ### Covariate preparations
#' Covariates_ls <- CovariateSetup(Training = Qsoil_rast,
#'                                 Target = 0.03,
#'                                 Covariates = c("tksat", "tkdry", "csol", "k_s", "lambda", "psi", "theta_s"),
#'                                 Source = "Drive",
#'                                 Extent = Jotunheimen_poly,
#'                                 Keep_Global = TRUE)
#'
#' ### kriging itself
#' ShapeKrig <- Kriging(
#'   Data = Qsoil_rast,
#'   Covariates_training = Covariates_ls[[1]],
#'   Covariates_target = Covariates_ls[[2]],
#'   Equation = "tksat + tkdry + csol + k_s + lambda + psi + theta_s",
#'   Cores = 1,
#'   FileName = "KrigTest2",
#'   FileExtension = ".nc",
#'   Keep_Temporary = FALSE,
#'   nmax = 40,
#'   verbose = TRUE
#' )
#' }
#' @export
Kriging <- function(
    Data,
    Covariates_training,
    Covariates_target,
    Equation = NULL,
    Cores = detectCores(),
    nmax = Inf,
    Dir = getwd(),
    FileName,
    FileExtension = ".nc",
    Keep_Temporary = FALSE,
    verbose = TRUE
){
  ## Run Preparations ===============
  if(verbose){message("###### Checking your Kriging Specification")}
  ### Number of layers in data & Progress bar
  KrigIterations <- nlyr(Data) # used for krig looping
  pb <- progress_bar$new(
    format = "Kriging (:current/:total) | [:bar] Elapsed: :elapsed | Remaining: :eta",
    total = KrigIterations,    # 100
    width = getOption("width"))
  progress_layer <- 1:KrigIterations  # token reported in progress bar
  ### CRS for assignment in loop
  CRS_dat <- crs(Data)
  ### if no equation is specified, assign additive combination of variables in training covariates
  if(is.null(Equation)){Equation <- paste(terra::names(Covariates_training), collapse = " + ")}
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
  ### Remove extension from file name
  FileName <- file_path_sans_ext(FileName)

  ## Check if already executed once ===============
  FCheck1 <- Check.File(FName = paste0(FileName, "_Kriged", FileExtension), Dir = Dir, loadFun = terra::rast, load = TRUE, verbose = TRUE)
  FCheck2 <- Check.File(FName = paste0(FileName, "_StDev", FileExtension), Dir = Dir, loadFun = terra::rast, load = TRUE, verbose = TRUE)
  if(!is.null(FCheck1)){
    if(FileExtension == ".nc"){
      FCheck1 <- Meta.NC(NC = FCheck1, FName = file.path(Dir, paste0(FileName, "_Kriged.nc")), Attrs = Meta_vec, Read = TRUE)
      FCheck2 <- Meta.NC(NC = FCheck2, FName = file.path(Dir, paste0(FileName, "_StDev.nc")), Attrs = Meta_vec, Read = TRUE)
    }
    terra::time(FCheck1) <- terra::time(FCheck2) <- terra::time(Data)
    terra::varnames(FCheck1) <- terra::varnames(FCheck2) <- terra::varnames(Data)
    terra::units(FCheck1) <- terra::units(FCheck2) <- terra::units(Data)
    terra::metags(FCheck1) <- terra::metags(FCheck2) <- Meta_vec
    Krig_ls <- list(FCheck1, FCheck2)
    names(Krig_ls) <- c("Prediction", "StDev")
    unlink(Dir.Temp, recursive = TRUE)
    return(Krig_ls)
  }

  ## Catching Most Frequent Issues ===============
  Check_Product <- Check.Krig(Data = Data, CovariatesCoarse = Covariates_training, CovariatesFine = Covariates_target, KrigingEquation = KrigingEquation)
  KrigingEquation <- Check_Product[[1]] # extract KrigingEquation (this may have changed in check_Krig)
  # DataSkips <- Check_Product[[2]] # extract which layers to skip due to missing data (this is unlikely to ever come into action)
  Terms <- unique(unlist(strsplit(labels(terms(KrigingEquation)), split = ":"))) # identify which layers of data are needed

  ## Data Reformatting ===============
  if(verbose){message("###### Preparing And Reformatting your Data")}
  # (Kriging requires spatially referenced data frames, reformatting from rasters happens here)
  ### Make Training sf object
  Origin <- as.data.frame(Covariates_training, xy = TRUE, na.rm = FALSE)
  colnames(Origin)[-1:-2] <- names(Covariates_training)
  Origin <- Origin[, c(1:2, which(colnames(Origin) %in% Terms))] # retain only columns containing terms
  Origin <- st_as_sf(Origin, coords = c("x", "y"))
  ### Make Target sf object
  Target <- as.data.frame(Covariates_target, xy = TRUE)
  colnames(Target)[-1:-2] <- names(Covariates_target)
  Target <- Target[, c(1:2, which(colnames(Target) %in% Terms))] # retain only columns containing terms
  Target <- st_as_sf(Target, coords = c("x", "y"))
  ### Make data into data frame for handling in parallel (SpatRasters cannot be used in foreach)
  Data_df <- as.data.frame(Data, xy = TRUE, na.rm = FALSE)

  ## Kriging Specification ===============
  # (this will be parsed and evaluated in parallel and non-parallel evaluations further down)
  looptext <- "
  ## check if already produced this krigr
  if(!paste0(str_pad(Iter_Krige,7,'left','0'), '_data.nc') %in% list.files(Dir.Temp)){
    ### Iteration-specific data
    DataSF <- Data_df[, c(1:2, Iter_Krige+2)]
    colnames(DataSF)[-1:-2] <- 'Data'
    DataSF <- st_as_sf(DataSF, coords = c('x', 'y'))
    KrigData <- cbind(Origin, DataSF$Data)
    KrigData <- na.omit(KrigData)
    colnames(KrigData)[ncol(Origin)] <- 'Data'

    ### Try kriging
    Iter_Try <- 0 # number of tries set to 0
    kriging_result <- NULL
    while(class(kriging_result)[1] != 'autoKrige' & Iter_Try < 10){ # try kriging 10 times, this is because of a random process of variogram identification within the automap package that can fail on smaller datasets randomly when it isn't supposed to
      try(invisible(capture.output(kriging_result <- autoKrige(formula = KrigingEquation, input_data = na.omit(KrigData), new_data = Target, nmax = nmax))), silent = TRUE)
      Iter_Try <- Iter_Try +1
    }
    if(class(kriging_result)[1] != 'autoKrige'){ # give error if kriging fails
      message(paste0('Kriging failed for layer ', Iter_Krige, '. Error message produced by autoKrige function: ', geterrmessage()))
    }

    ### Make SpatRaster from Kriging result
    try(
      Pred_rast <- rast(x = cbind(st_coordinates(kriging_result$krige_output), st_drop_geometry(kriging_result$krige_output)$var1.pred), type = 'xyz'),
      silent = TRUE
    )
    try(
      StDe_rast <- rast(x = cbind(st_coordinates(kriging_result$krige_output), st_drop_geometry(kriging_result$krige_output)$var1.stdev), type = 'xyz'),
      silent = TRUE
    )
    if(!exists('Pred_rast') & !exists('StDe_rast')){
      stop('Rasterising of kriging result failed.')
    }
    terra::crs(StDe_rast) <- terra::crs(Pred_rast) <- CRS_dat # setting the crs according to the data

    ### Data writing to disk
    if(FileExtension == '.tif'){
      writeRaster(x = Pred_rast, filename = file.path(Dir.Temp, paste0(str_pad(Iter_Krige,7,'left','0'), '_data', FileExtension)), overwrite = TRUE)
      writeRaster(x = StDe_rast, filename = file.path(Dir.Temp, paste0(str_pad(Iter_Krige,7,'left','0'), '_StDev', FileExtension)), overwrite = TRUE)
    }
    if(FileExtension == '.nc'){
      writeCDF(x = Pred_rast, filename = file.path(Dir.Temp, paste0(str_pad(Iter_Krige,7,'left','0'), '_data', FileExtension)), overwrite = TRUE)
      writeCDF(x = StDe_rast, filename = file.path(Dir.Temp, paste0(str_pad(Iter_Krige,7,'left','0'), '_StDev', FileExtension)), overwrite = TRUE)
    }
  }
  ### Return
  NULL # otherwise trying to return SpatRaster
  "

  ## Kriging Execution ===============
  ## carry out kriging according to user specifications either in parallel or on a single core
  if(verbose){message("###### Commencing Kriging")}
  ### multi-core kriging ----
  if(Cores > 1){
    ## registering cluster and progress bar for foreach
    cl <- makeCluster(Cores)
    registerDoSNOW(cl)
    progress <- function(n){
      pb$tick(tokens = list(layer = progress_layer[n]))
    }
    on.exit(stopCluster(cl))
    ## executing foreach kriging
    ForeachObjects <- c("Dir.Temp", "Cores", "CRS_dat", "Data_df", "KrigingEquation", "Origin", "Target", "nmax", "FileExtension") # objects which are needed in Kriging
    foreach(Iter_Krige = 1:KrigIterations, # kriging loop over all layers in Data, with condition (%:% when(...)) to only run if current layer is not present in Dir.Temp yet
            .packages = c("terra", "sf", "stringr", "automap", "ncdf4"), # import packages necessary to each iteration
            .export = ForeachObjects,
            .options.snow = list(progress = progress))  %dopar% { # parallel kriging loop # %:% when(!paste0(str_pad(Iter_Krige,7,"left","0"), '_data.nc') %in% list.files(Dir.Temp))
              eval(parse(text=looptext)) # evaluate the kriging specification per cluster unit per layer
              Sys.sleep(0.5)
              NULL
            } # end of parallel kriging loop
  }

  ### single-core kriging ----
  if(Cores == 1){
    for(Iter_Krige in 1:KrigIterations){
      # FileExis <- paste0(str_pad(Iter_Krige,7,'left','0'), '_data', FileExtension) %in% list.files(Dir.Temp)
      # if(!FileExis){
      eval(parse(text=looptext)) # evaluate the kriging specification per layer
      # }
      Sys.sleep(0.5)
      pb$tick(tokens = list(layer = progress_layer[Iter_Krige]))
    }
  }

  ## Data Loading and Saving ===============
  ### loading kriged data back in
  Krig_rast <- rast(list.files(Dir.Temp, full.names = TRUE, pattern = "_data"))
  SE_rast <- rast(list.files(Dir.Temp, full.names = TRUE, pattern = "_StDev"))
  ### assigning time to products
  terra::time(Krig_rast) <- terra::time(SE_rast) <- terra::time(Data)
  terra::varnames(Krig_rast) <- terra::varnames(SE_rast) <- terra::varnames(Data)
  terra::units(Krig_rast) <- terra::units(SE_rast) <- terra::units(Data)
  terra::metags(Krig_rast) <- terra::metags(SE_rast) <- Meta_vec
  ### Data Saving
  if(FileExtension == ".tif"){
    writeRaster(Krig_rast, filename = file.path(Dir, paste0(FileName, "_Kriged", FileExtension)))
    writeRaster(SE_rast, filename = file.path(Dir, paste0(FileName, "_StDev", FileExtension)))
  }
  if(FileExtension == ".nc"){
    Krig_rast <- Meta.NC(NC = Krig_rast, FName = file.path(Dir, paste0(FileName, "_Kriged", FileExtension)),
                         Attrs = terra::metags(Krig_rast), Write = TRUE)
    SE_rast <- Meta.NC(NC = SE_rast, FName = file.path(Dir, paste0(FileName, "_STDev", FileExtension)),
                       Attrs = terra::metags(SE_rast), Write = TRUE)
  }

  Krig_rast <- rast(file.path(Dir, paste0(FileName, "_Kriged", FileExtension)))
  SE_rast <- rast(file.path(Dir, paste0(FileName, "_STDev", FileExtension)))
  terra::time(Krig_rast) <- terra::time(SE_rast) <- terra::time(Data)
  terra::varnames(Krig_rast) <- terra::varnames(SE_rast) <- terra::varnames(Data)
  terra::units(Krig_rast) <- terra::units(SE_rast) <- terra::units(Data)
  terra::metags(Krig_rast) <- terra::metags(SE_rast) <- Meta_vec

  ## Removing Temporary Files ===============
  if(Keep_Temporary == FALSE){ # cleanup check
    unlink(Dir.Temp, recursive = TRUE)
  }  # end of cleanup check

  ## Data Return ===============
  Krig_ls <- list(Krig_rast, SE_rast)
  names(Krig_ls) <- c("Prediction", "StDev")
  return(Krig_ls)
}
