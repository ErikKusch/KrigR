#' Computation of Bioclimatic Variables
#'
#' This function queries download of required essential climate variables from the [Climate Data Store](https://cds.climate.copernicus.eu/#!/home) hosted by the [Copernicus Climate Change Service (C3S)](https://cds.climate.copernicus.eu/about-c3s) for retrieval of climate data and subsequent calculation of bioclimatic variables for user-defined regions and time-frames.
#'
#' @param Water_Var ERA5(Land)-contained climate variable targeting water availability information. See 'download' output of Variable_List() for possible values. Recommended values: "volumetric_soil_water_layer_1", "total_precipitation".
#' @param DataSet Which ERA5 data set to download data from. 'era5' or 'era5-land'.
#' @param Y_start Year ('YYYY') at which to start time series of downloaded data.
#' @param Y_end Year ('YYYY') at which to stop time series of downloaded data.
#' @param T_res Temporal resolution from which to obtain minimum and maximum values of temperature. 'hour' or 'day'
#' @param Extent Optional, download data according to rectangular bounding box. specify as extent() object or as a raster, a SpatialPolygonsDataFrame object, or a data.frame opbject. If Extent is a SpatialPolygonsDataFrame, this will be treated as a shapefile and the output will be cropped and masked to this shapefile. If Extent is a data.frame of geo-referenced point records, it needs to contain Lat and Lon columns as well as a non-repeating ID-column.
#' @param Buffer Optional. Identifies how big a rectangular buffer to draw around points if Extent is a data frame of points. Buffer is expressed as centessimal degrees.
#' @param ID Optional. Identifies which column in Extent to use for creation of individual buffers if Extent is a data.frame.
#' @param Dir Directory specifying where to download data to.
#' @param FileName A file name for the netcdf produced.
#' @param API_Key Character; ECMWF cds API key.
#' @param API_User Character; ECMWF cds user number.
#' @param verbose Optional, logical. Whether to report progress of the function in the console or not.
#' @param Keep_Raw Logical. Whether to keep monthly netcdf files of raw data aggregated to temporal resolution of `T_res`. Default FALSE.
#' @param Keep_Monthly Logical. Whether to keep monthly netcdf files of raw data aggregated to temporal resolution of months. Default FALSE.
#' @param Cores Numeric. How many cores to use.^This can speed up downloads of long time-series. If you want output to your console during the process, use Cores = 1. Parallel processing is carried out when Cores is bigger than 1. Default is 1.
#' @param TryDown Optional, numeric. How often to attempt the download of each individual file that the function queries from the server. This is to circumvent having to restart the entire function when encountering connectivity issues.
#' @param TimeOut Numeric. The timeout for each download in seconds. Default 36000 seconds (10 hours).
#' @param SingularDL Logical. Whether to force download of data in one call to CDS or automatically break download requests into individual monthly downloads. Default is FALSE.
#' @return A raster object containing the downloaded ERA5(-Land) data, and a NETCDF (.nc) file in the specified directory.
#' @examples
#' \dontrun{
#' BioClim (Water_Var = "volumetric_soil_water_layer_1", # could also be total_precipitation
#' Y_start = 1981,
#' Y_end = 2015,
#' T_res = "day",
#' Extent = extent(11.8,15.1,50.1,51.7),
#' DataSet = "era5-land",
#' Dir = getwd(),
#' verbose = TRUE,
#' FileName = "NULL",
#' Keep_Raw = FALSE,
#' Keep_Monthly = FALSE,
#' API_User = API_User,
#' API_Key = API_Key)
#' }
#'
#' @export
BioClim <- function(Water_Var = "volumetric_soil_water_layer_1", # could also be total_precipitation
                    Y_start = 1981,
                    Y_end = 2015,
                    T_res = "day",
                    Extent = extent(-180,180,-90,90),
                    DataSet = "era5-land",
                    Dir = getwd(),
                    verbose = TRUE,
                    FileName = "NULL",
                    Keep_Raw = FALSE,
                    Keep_Monthly = FALSE,
                    Buffer = .5,
                    ID = "ID",
                    API_User = API_User,
                    API_Key = API_Key,
                    Cores = 1,
                    TryDown = 10,
                    TimeOut = 36000,
                    SingularDL = FALSE){

  Vars <- c("2m_temperature", Water_Var)

  if(Y_end == year(Sys.Date())){
    stop("Please note that calculation of bioclimatic variables requires consideration of data sets spanning full years. Therefore, the current year cannot be included in the calculation of bioclimatic variables.")
  }

  ####### SHAPE HANDLING #######
  if(class(Extent) == "data.frame"){ # if we have been given point data
    Extent <- KrigR:::buffer_Points(Points = Extent, Buffer = Buffer, ID = ID)
  }
  if(class(Extent) == "Raster" | class(Extent) == "SpatialPolygonsDataFrame" | class(Extent) == "SpatialPolygons"){ # sanity check: ensure it is a raster of Spatialpolygonsdataframe object if not an extent object
    if(class(Extent) == "SpatialPolygonsDataFrame" | class(Extent) == "SpatialPolygons"){ # shape check
      Shape <- Extent # save the shapefile for later masking
    } # end of shape check
  } # end of sanity check


  ####### DATA RETRIEVAL #######
  ### DATE HANDLER ----
  Down_start <- lubridate::date(paste0(Y_start, "-01-01"))
  Down_end <- lubridate::date(paste0(Y_end, "-12-31"))
  T_seq <- seq(Down_start, Down_end, by = "month")
  Y_seq <- year(T_seq)
  M_seq <- str_pad(month(T_seq), 2, "left", 0)

  ## LOOP FOR EACH MONTH (immediate reducing of raster layers for storage purposes) or in one-go (reducing of download calls)
  looptext <- "
  # check name (file(s) that will be written)
  if(SingularDL){
    CheckName <- file.path(Dir, paste0(Var_down, '-', Fun_vec, 'MonthlyBC.nc'))
    TempName <- file.path(Dir, paste0(Var_down, '_Temporary.nc'))
  }else{
    CheckName <- file.path(Dir, paste0(Var_down, '-', Fun_vec, '-', Y_seq[Down_Iter], '_', M_seq[Down_Iter], 'MonthlyBC.nc'))
    TempName <- file.path(Dir, paste0(Var_down, '_Temporary_', Y_seq[Down_Iter], '_', M_seq[Down_Iter], '.nc'))
  }

  # DATA CHECK (skip this iteration if data is already downloaded)
  if(all(file.exists(CheckName))){
    if(verbose){
      message(ifelse(SingularDL, paste(Var_down, 'already processed'), paste0(Var_down, ' already processed for ', M_seq[Down_Iter], '/', Y_seq[Down_Iter])))
    }
    next()
  }
  ## DATE HANDLER
  if(SingularDL){
    month_start <- paste0(Y_start, '-01-01')
    month_end <- paste0(Y_end, '-12-31')
  }else{
    month_start <- lubridate::date(paste(Y_seq[Down_Iter], M_seq[Down_Iter], '01', sep = '-')) # set start date
    month_end <- month_start+(lubridate::days_in_month(month_start)-1) # find end date depending on days in current month
  }

  ## ensuring that water var is pulled at monthly resolution
  if(Var_Iter == 2 & Down_start >= '1981-01-01'){ # 1981 is the earliest date for monthly aggregates for both era5 and era5-land
    T_resDL <- 'month'
  }else{
    T_resDL <- T_res
  }

  ## DOWNLOAD
  if(file.exists(TempName)){
      if(verbose){
        message(ifelse(SingularDL, paste(Var_down, 'already downloaded'), paste0(Var_down, ' already downloaded for ', M_seq[Down_Iter], '/', Y_seq[Down_Iter])))
      }
      Temp_Ras <- stack(TempName)
    }else{
      Temp_Ras <- download_ERA(
        Variable = Var_down,
        DataSet = DataSet,
        Type = 'reanalysis',
        DateStart = month_start,
        DateStop = month_end,
        TResolution = T_resDL,
        TStep = 1,
        FUN = AggrFUN,
        Extent = Extent,
        Dir = Dir,
        FileName = strsplit(TempName, '/')[[1]][length(strsplit(TempName, '/')[[1]])],
        API_User = API_User,
        API_Key = API_Key,
        verbose = verbose,
        PrecipFix = PrecipFix,
        TryDown = TryDown,
        TimeOut = TimeOut,
        SingularDL = SingularDL
      )
    }
  ## PROCESSING
  if(Var_Iter == 1 | Down_start < '1981-01-01'){ # 1981 is the earliest date for monthly aggregates for both era5 and era5-land
  Counter <- 1
      for(Iter_fun in Fun_vec){
        Save_Ras <- stackApply(Temp_Ras,
                               indices = month(seq(as.POSIXct(paste(month_start, '00:00:00')), as.POSIXct(paste(month_end, '23:00:00')), by=T_res)),
                               fun = Iter_fun)

        if(Iter_fun == 'sum' & exists('Shape')){
          range <- KrigR:::mask_Shape(base.map = Save_Ras[[1]], Shape = Shape)
          Save_Ras <- mask(Save_Ras, range)
        }
        writeRaster(x = Save_Ras,
                    filename = CheckName[Counter],
                    format = 'CDF', overwrite = TRUE)
        Counter <- Counter + 1
      }
  }else{
    file.copy(from = TempName, to = CheckName)
  }
      ## DELETING RAW
      if(!isTRUE(Keep_Raw)){
        unlink(TempName)
      }
      "

  ### DATA DOWNLOAD ----
  for(Var_Iter in 1:length(Vars)){ ## LOOP FOR EACH VARIABLE

    ## VARIABLE IDENTIFICATION
    Var_down <- Vars[Var_Iter]
    if(Var_down == "total_precipitation"){PrecipFix <- TRUE}else{PrecipFix <- FALSE}

    ## PROCESSING FUNCTIONS
    Fun_vec <- 'mean' # for all water variables that are not total precip
    if(Var_down == 'total_precipitation'){Fun_vec <- 'sum'}
    if(Var_down == '2m_temperature'){Fun_vec <- c('min', 'mean', 'max')}

    if(Var_down == 'total_precipitation'){
      AggrFUN <- sum
    }else{
      AggrFUN <- mean
    }
    if(SingularDL){
      n_down <- 1
      Cores = 1
    }else{
      n_down <- length(T_seq)
    }
    n_downrep <- n_down
    if(Down_start < '1981-01-01' & Var_down == 'total_precipitation'){
      n_downrep <- length(T_seq)*2}
    if(verbose){
      message(paste("The KrigR::BioClim() function is going to stage", n_downrep, "download(s) for", Var_down, "data now."))
    }

    if(Cores > 1){ # Cores check: if parallel processing has been specified
      ForeachObjects <- c("Var_down", "Var_Iter", "Dir", "Y_seq", "M_seq", "DataSet", "PrecipFix", "API_User", "API_Key", "T_res", "Extent", "Keep_Raw", "Fun_vec", "TryDown", "TimeOut", "SingularDL", "Var_down", "Fun_vec", "AggrFUN", "verbose", "Down_start")
      cl <- makeCluster(Cores) # Assuming Cores node cluster
      registerDoParallel(cl) # registering cores
      foreach(Down_Iter = 1:n_down,
              .packages = c("KrigR"), # import packages necessary to each itteration
              .export = ForeachObjects) %:% when(!file.exists(file.path(Dir, paste0(Var_down, '-', Fun_vec[length(Fun_vec)], '-', Y_seq[Down_Iter], '_', M_seq[Down_Iter], 'MonthlyBC.nc')))) %dopar% {
                eval(parse(text=looptext))
              } # end of parallel kriging loop
      stopCluster(cl) # close down cluster
    }else{ # if non-parallel processing has been specified
      for(Down_Iter in 1:n_down){eval(parse(text=looptext))}
    } # end of non-parallel loop
  } # end of variable loop

  ### DATA LOADING ----
  setwd(Dir)
  Tair_min <- raster::stack(list.files(path = Dir, pattern = paste0(Vars[1], "-min")))
  Tair_mean <- raster::stack(list.files(path = Dir, pattern = paste0(Vars[1], "-mean")))
  Tair_max <- raster::stack(list.files(path = Dir, pattern = paste0(Vars[1], "-max")))
  Water <- raster::stack(list.files(path = Dir, pattern = paste0(Vars[2], "-", Fun_vec)))

  ### QUARTER COMPUTATION ----
  Tair_mean_quarter <- stackApply(Tair_mean, indices = rep(1:4, each = 3, length.out = nlayers(Tair_mean)), mean)
  if(Water_Var == "total_precipitation"){
    Water_quarter <- stackApply(Water, indices = rep(1:4, each = 3, length.out = nlayers(Water)), sum)
  }else{
    Water_quarter <- stackApply(Water, indices = rep(1:4, each = 3, length.out = nlayers(Water)), mean)
  }
  if(Water_Var == "total_precipitation" & exists("Shape")){
    range <- KrigR:::mask_Shape(base.map = Water_quarter[[1]], Shape = Shape)
    Water_quarter <- mask(Water_quarter, range)
  }

  ####### BIOCLIMATIC VARIABLES #######
  ### BIO1 = Annual Mean Temperature ----
  BIO1 <- raster::mean(Tair_mean)

  ### BIO2 = Mean Diurnal Range (Mean of monthly (max temp - min temp)) ----
  BIO2 <- raster::mean(Tair_max-Tair_min, na.rm = TRUE)

  ### BIO4 = Temperature Seasonality (standard deviation * 100) ----
  BIO4 <- calc(Tair_mean, sd)*100

  ### BIO5 = Max Temperature of Warmest Month ----
  BIO5 <- max(Tair_max)

  ### BIO6 = Min Temperature of Coldest Month ----
  BIO6 <- min(Tair_min)

  ### BIO7 = Temperature Annual Range (BIO5-BIO6) ----
  BIO7 <- BIO5-BIO6

  ### BIO3 = Isothermality (BIO2/BIO7) (*100) ----
  BIO3 <- BIO2/BIO7*100

  ### BIO8 = Mean Temperature of Wettest Quarter ----
  BIO8 <- raster::stackSelect(Tair_mean_quarter, raster::which.max(Water_quarter))

  ### BIO9 = Mean Temperature of Driest Quarter ----
  BIO9 <- raster::stackSelect(Tair_mean_quarter, raster::which.min(Water_quarter))

  ### BIO10 = Mean Temperature of Warmest Quarter ----
  BIO10 <- raster::stackSelect(Tair_mean_quarter, raster::which.max(Tair_mean_quarter))

  ### BIO11 = Mean Temperature of Coldest Quarter ----
  BIO11 <- raster::stackSelect(Tair_mean_quarter, raster::which.min(Tair_mean_quarter))

  ### BIO12 = Annual Precipitation ----
  if(Water_Var == "total_precipitation"){
    BIO12 <- sum(Water)/(nlayers(Water)/12)
  }else{
    BIO12 <- raster::mean(Water)
  }
  if(Water_Var == "total_precipitation" & exists("Shape")){
    range <- KrigR:::mask_Shape(base.map = BIO12, Shape = Shape)
    BIO12 <- mask(BIO12, range)
  }

  ### BIO13 = Precipitation of Wettest Month ----
  BIO13 <- max(Water)
  if(Water_Var == "total_precipitation" & exists("Shape")){
    range <- KrigR:::mask_Shape(base.map = BIO13, Shape = Shape)
    BIO13 <- mask(BIO13, range)
  }

  ### BIO14 = Precipitation of Driest Month ----
  BIO14 <- min(Water)
  if(Water_Var == "total_precipitation" & exists("Shape")){
    range <- KrigR:::mask_Shape(base.map = BIO14, Shape = Shape)
    BIO14 <- mask(BIO14, range)
  }

  ### BIO15 = Precipitation Seasonality (Coefficient of Variation) ----
  BIO15 <- calc(Water, sd)/BIO12 * 100

  ### BIO16 = Precipitation of Wettest Quarter ----
  BIO16 <- max(Water_quarter)

  ### BIO17 = Precipitation of Driest Quarter ----
  BIO17 <- min(Water_quarter)

  ### BIO18 = Precipitation of Warmest Quarter ----
  BIO18 <- raster::stackSelect(Water_quarter, raster::which.max(Tair_mean_quarter))

  ### BIO19 = Precipitation of Coldest Quarter ----
  BIO19 <- raster::stackSelect(Water_quarter, raster::which.min(Tair_mean_quarter))

  ####### EXPORT #######
  BIO_Ras <-raster::stack(BIO1, BIO2, BIO3, BIO4, BIO5, BIO6, BIO7, BIO8, BIO9,
                          BIO10, BIO11, BIO12, BIO13, BIO14, BIO15, BIO16, BIO17, BIO18, BIO19)
  names(BIO_Ras) <- paste0("BIO", 1:19)
  writeRaster(BIO_Ras, file.path(Dir, FileName), format = "CDF", overwrite = TRUE)
  if(!isTRUE(Keep_Monthly)){
    RM_fs <- list.files(Dir, pattern = "MonthlyBC.nc")
    unlink(RM_fs)
  }
  return(BIO_Ras)
}
