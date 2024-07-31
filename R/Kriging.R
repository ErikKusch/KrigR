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
  ### Number of layers in data & Progress bar
  KrigIterations <- terra::nlyr(Data) # used for krig looping
  pb <- progress::progress_bar$new(
    format = "Kriging (:current/:total) | [:bar] Elapsed: :elapsed | Remaining: :eta",
    total = KrigIterations,    # 100
    width = getOption("width"))
  progress_layer <- 1:KrigIterations  # token reported in progress bar
  ### CRS for assignment in loop
  CRS_dat <- terra::crs(Data)
  ### if no equation is specified, assign additive combination of variables in training covariates
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
  ### Remove extension from file name
  FileName <- tools::file_path_sans_ext(FileName)

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
    return(Krig_ls)
  }

  ## Catching Most Frequent Issues ===============
  Check_Product <- Check.Krig(Data = Data, CovariatesCoarse = Covariates_training, CovariatesFine = Covariates_target, KrigingEquation = KrigingEquation)
  KrigingEquation <- Check_Product[[1]] # extract KrigingEquation (this may have changed in check_Krig)
  # DataSkips <- Check_Product[[2]] # extract which layers to skip due to missing data (this is unlikely to ever come into action)
  Terms <- unique(unlist(strsplit(labels(terms(KrigingEquation)), split = ":"))) # identify which layers of data are needed

  ## Data Reformatting ===============
  # (Kriging requires spatially referenced data frames, reformatting from rasters happens here)
  ### Make Training sf object
  Origin <- as.data.frame(Covariates_training, xy = TRUE, na.rm = FALSE)
  colnames(Origin)[-1:-2] <- terra::varnames(Covariates_training)
  Origin <- Origin[, c(1:2, which(colnames(Origin) %in% Terms))] # retain only columns containing terms
  Origin <- sf::st_as_sf(Origin, coords = c("x", "y"))
  ### Make Target sf object
  Target <- as.data.frame(Covariates_target, xy = TRUE)
  colnames(Target)[-1:-2] <- terra::varnames(Covariates_target)
  Target <- Target[, c(1:2, which(colnames(Target) %in% Terms))] # retain only columns containing terms
  Target <- sf::st_as_sf(Target, coords = c("x", "y"))
  ### Make data into data frame for handling in parallel (SpatRasters cannot be used in foreach)
  Data_df <- as.data.frame(Data, xy = TRUE, na.rm = FALSE)

  ## Kriging Specification ===============
  # (this will be parsed and evaluated in parallel and non-parallel evaluations further down)
  looptext <- "
  ### Iteration-specific data
  DataSF <- Data_df[, c(1:2, Iter_Krige+2)]
  colnames(DataSF)[-1:-2] <- 'Data'
  DataSF <- sf::st_as_sf(DataSF, coords = c('x', 'y'))
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
    Pred_rast <- terra::rast(x = cbind(sf::st_coordinates(kriging_result$krige_output), sf::st_drop_geometry(kriging_result$krige_output)$var1.pred), type = 'xyz'),
    silent = TRUE
  )
  try(
    StDe_rast <- terra::rast(x = cbind(sf::st_coordinates(kriging_result$krige_output), sf::st_drop_geometry(kriging_result$krige_output)$var1.stdev), type = 'xyz'),
    silent = TRUE
  )
  if(!exists('Pred_rast') & !exists('StDe_rast')){
    stop('Rasterising of kriging result failed.')
  }
  terra::crs(StDe_rast) <- terra::crs(Pred_rast) <- CRS_dat # setting the crs according to the data

  ### Data writing to disk
  if(FileExtension == '.tif'){
    terra::writeRaster(x = Pred_rast, filename = file.path(Dir.Temp, paste0(stringr::str_pad(Iter_Krige,7,'left','0'), '_data', FileExtension)), overwrite = TRUE)
    terra::writeRaster(x = StDe_rast, filename = file.path(Dir.Temp, paste0(stringr::str_pad(Iter_Krige,7,'left','0'), '_StDev', FileExtension)), overwrite = TRUE)
  }
  if(FileExtension == '.nc'){
    terra::writeCDF(x = Pred_rast, filename = file.path(Dir.Temp, paste0(stringr::str_pad(Iter_Krige,7,'left','0'), '_data', FileExtension)), overwrite = TRUE)
    terra::writeCDF(x = StDe_rast, filename = file.path(Dir.Temp, paste0(stringr::str_pad(Iter_Krige,7,'left','0'), '_StDev', FileExtension)), overwrite = TRUE)
  }

  ### Return
  NULL # otherwise trying to return SpatRaster
  "

  ## Kriging Execution ===============
  #' carry out kriging according to user specifications either in parallel or on a single core
  if(verbose){message("Commencing Kriging")}
  ### multi-core kriging ----
  if(Cores > 1){
    ## registering cluster and progress bar for foreach
    cl <- parallel::makeCluster(Cores)
    doSNOW::registerDoSNOW(cl)
    progress <- function(n){
      pb$tick(tokens = list(layer = progress_layer[n]))
    }
    on.exit(parallel::stopCluster(cl))
    ## executing foreach kriging
    ForeachObjects <- c("Dir.Temp", "Cores", "CRS_dat", "Data_df", "KrigingEquation", "Origin", "Target", "nmax", "FileExtension") # objects which are needed in Kriging
    foreach(Iter_Krige = 1:KrigIterations, # kriging loop over all layers in Data, with condition (%:% when(...)) to only run if current layer is not present in Dir.Temp yet
            .packages = c("terra", "sf", "stringr", "automap", "ncdf4"), # import packages necessary to each iteration
            .export = ForeachObjects,
            .options.snow = list(progress = progress)) %:% when(!paste0(stringr::str_pad(Iter_Krige,7,"left","0"), '_data.nc') %in% list.files(Dir.Temp)) %dopar% { # parallel kriging loop
              Ras_Krig <- eval(parse(text=looptext)) # evaluate the kriging specification per cluster unit per layer
            } # end of parallel kriging loop
  }

  ### single-core kriging ----
  if(Cores == 1){
    for(Iter_Krige in 1:KrigIterations){
      FileExis <- paste0(stringr::str_pad(Iter_Krige,7,'left','0'), '_data', FileExtension) %in% list.files(Dir.Temp)
      if(!FileExis){
        eval(parse(text=looptext)) # evaluate the kriging specification per layer
      }
      pb$tick(tokens = list(layer = progress_layer[Iter_Krige]))
    }
  }

  ## Data Loading and Saving ===============
  ### loading kriged data back in
  Krig_rast <- terra::rast(list.files(Dir.Temp, full.names = TRUE, pattern = "_data"))
  SE_rast <- terra::rast(list.files(Dir.Temp, full.names = TRUE, pattern = "_StDev"))
  ### assigning time to products
  terra::time(Krig_rast) <- terra::time(SE_rast) <- terra::time(Data)
  terra::varnames(Krig_rast) <- terra::varnames(SE_rast) <- terra::varnames(Data)
  terra::units(Krig_rast) <- terra::units(SE_rast) <- terra::units(Data)
  terra::metags(Krig_rast) <- terra::metags(SE_rast) <- Meta_vec
  ### Data Saving
  if(FileExtension == ".tif"){
    terra::writeRaster(Krig_rast, filename = file.path(Dir, paste0(FileName, "_Kriged", FileExtension)))
    terra::writeRaster(SE_rast, filename = file.path(Dir, paste0(FileName, "_StDev", FileExtension)))
  }
  if(FileExtension == ".nc"){
    Krig_rast <- Meta.NC(NC = Krig_rast, FName = file.path(Dir, paste0(FileName, "_Kriged", FileExtension)),
                        Attrs = terra::metags(Krig_rast), Write = TRUE)
    SE_rast <- Meta.NC(NC = SE_rast, FName = file.path(Dir, paste0(FileName, "_STDev", FileExtension)),
                        Attrs = terra::metags(SE_rast), Write = TRUE)
  }

  ## Removing Temporary Files ===============
  if(Keep_Temporary == FALSE){ # cleanup check
    unlink(Dir.Temp, recursive = TRUE)
  }  # end of cleanup check

  ## Data Return ===============
  Krig_ls <- list(Krig_rast, SE_rast)
  names(Krig_ls) <- c("Prediction", "StDev")
  return(Krig_ls)
}
