#' Computation of Bioclimatic Variables
#'
#' This function queries download of required essential climate variables from the [Climate Data Store](https://cds.climate.copernicus.eu/#!/home) hosted by the [Copernicus Climate Change Service (C3S)](https://cds.climate.copernicus.eu/about-c3s) for retrieval of climate data and subsequent calculation of bioclimatic variables for user-defined regions and time-frames.
#'
#' @param Temperature_Var CDS-contained climate variable targeting temperature information. Recommended values: "2m_temperature".
#' @param Temperature_DataSet Character. Which dataset to query data from. See currently supported datasets by calling \code{\link{Meta.List}}. For now, this function is conceptualised to support "reanalysis-era5-land".
#' @param Temperature_Type Either NA or Character. Which kind of sub-type to query per data set. See \code{\link{Meta.QucikFacts}} for options per dataset.
#' @param Water_Var CDS-contained climate variable targeting water availability information. Recommended values: "volumetric_soil_water_layer_X" (where X is an integer of either 1, 2, 3, 4), "total_precipitation".
#' @param Water_DataSet Character. Which dataset to query water availability data from. See currently supported datasets by calling \code{\link{Meta.List}}. For now, this function is conceptualised to support "reanalysis-era5-land".
#' @param Water_Type Either NA or Character. Which kind of sub-type to query per water availability data set. See \code{\link{Meta.QucikFacts}} for options per dataset.
#' @param Y_start Year ('YYYY') at which to start time series of downloaded data.
#' @param Y_end Year ('YYYY') at which to stop time series of downloaded data.
#' @param TZone Character. Time zone in which to represent and evaluate time dimension of data. See the output of OlsonNames() for a full overview of supported specifications. Default is UTC.
#' @param Extent Optional, download data according to desired spatial specification. If missing/unspecified, total area of queried data set is used. Can be specified either as a raster object, an sf object, a terra object, or a data.frame. If Extent is a raster or terra object, data will be queried according to rectangular extent thereof. If Extent is an sf (MULTI-)POLYGON object, this will be treated as a shapefile and the output will be cropped and masked to this shapefile. If Extent is a data.frame of geo-referenced point records, it needs to contain Lat and Lon columns around which a buffered shapefile will be created using the Buffer argument.
#' @param Buffer Optional, Numeric. Identifies how big a circular buffer to draw around points if Extent is a data.frame of points. Buffer is expressed as centessimal degrees.
#' @param Dir Character/Directory Pointer. Directory specifying where to download data to.
#' @param FileName Character. A file name for the produced file.
#' @param FileExtension Character. A file extension for the produced file. Supported values are ".nc" (default) and ".tif" (better support for metadata).
#' @param Compression Integer between 1 to 9. Applied to final .nc file that the function writes to hard drive. Same as compression argument in terra::writeCDF(). Ignored if FileExtension = ".tif".
#' @param API_Key Character; ECMWF cds API key.
#' @param API_User Character; ECMWF cds user number.
#' @param TChunkSize Numeric. Number of layers to bundle in each individual download. Default is 6000 to adhere to most restrictive CDS limits: https://cds.climate.copernicus.eu/live/limits.
#' @param Cores Numeric. How many cores to use when carrying out temporal aggregation. Default is 1.
#' @param verbose Logical. Whether to print/message function progress in console or not.
#' @param Keep_Raw Logical. Whether to retain raw downloaded data or not. Default is FALSE.
#' @param Keep_Monthly Logical. Whether to keep monthly netcdf files of raw data aggregated to temporal resolution of months. Default FALSE.
#' @param TryDown Optional, numeric. Legacy, ignored when querying data from new CDS (https://cds-beta.climate.copernicus.eu/; this happens when the package version of ecmwfr is >= 2.0.0). How often to attempt the download of each individual file that the function queries from the CDS. This is to circumvent having to restart the entire function when encountering connectivity issues.
#' @param TimeOut Numeric. Legacy, ignored when querying data from new CDS (https://cds-beta.climate.copernicus.eu/; this happens when the package version of ecmwfr is >= 2.0.0). The timeout for each download in seconds. Default 36000 seconds (10 hours).
#' @param closeConnections Logical. Whether to close all connections at the end of function execution. When executing this function often after another, this can be very useful to avoid errors.
#'
#' @return A SpatRaster object containing the queried bioclimatic data, and a NETCDF (.nc) file in the specified directory.
#'
#' The SpatRaster contains metadata/attributes as a named vector that can be retrieved with terra::metags(...):
#' \itemize{
#' \item{Citation}{ - A string which to use for in-line citation of the data product obtained with BioClim}.
#' \item{KrigRCall.X}{ - Arguments passed to the BioClim function that produced the file (API credentials are omitted from these metadata)}.
#' }
#'
#' @importFrom tools file_path_sans_ext
#' @importFrom lubridate year
#' @importFrom lubridate date
#' @importFrom stringr str_pad
#' @importFrom terra rast
#' @importFrom terra time
#' @importFrom terra nlyr
#' @importFrom terra metags
#' @importFrom terra writeRaster
#' @importFrom terra tapp
#' @importFrom terra app
#' @importFrom terra mean
#' @importFrom terra values
#' @importFrom terra subset
#'
#' @examples
#' \dontrun{
#' CN_ext <- terra::rast(system.file("extdata", "CentralNorway.nc", package = "KrigR"))
#' CN_BC <- BioClim(
#'   Temperature_Var = "2m_temperature",
#'   Temperature_DataSet = "reanalysis-era5-land",
#'   Temperature_Type = NA,
#'   Water_Var = "volumetric_soil_water_layer_1",
#'   Water_DataSet = "reanalysis-era5-land-monthly-means",
#'   Water_Type = "monthly_averaged_reanalysis",
#'   Y_start = 1970,
#'   Y_end = 1979,
#'   TZone = "CET",
#'   Extent = CN_ext,
#'   Dir = getwd(),
#'   FileName = "CN_BC",
#'   FileExtension = ".nc",
#'   Compression = 9,
#'   API_User = API_User,
#'   API_Key = API_Key,
#'   TChunkSize = 6000, TryDown = 10, TimeOut = 36000,
#'   Cores = parallel::detectCores(),
#'   verbose = TRUE,
#'   Keep_Raw = FALSE, Keep_Monthly = FALSE
#' )
#' }
#'
#' @export
BioClim <- function(
    # nolint: cyclocomp_linter.
    Temperature_Var = "2m_temperature",
    Temperature_DataSet = "reanalysis-era5-land",
    Temperature_Type = NA,
    Water_Var = "volumetric_soil_water_layer_1", # could also be total_precipitation
    Water_DataSet = "reanalysis-era5-land-monthly-means",
    Water_Type = "monthly_averaged_reanalysis",
    Y_start, Y_end, TZone = "UTC", # time-window, default set to range of dataset-type
    Extent, # spatial limitation, default set to range of dataset-type
    Buffer = 0.5, # point buffering if desired
    Dir = getwd(), FileName, FileExtension = ".nc", Compression = 9, # file storing
    API_User, API_Key, # API credentials
    TChunkSize = 6000, TryDown = 10, TimeOut = 36000, # Calls to CDS
    Cores = 1, # parallelisation
    verbose = TRUE, # verbosity
    Keep_Raw = FALSE, Keep_Monthly = FALSE, # continued file storage
    closeConnections = TRUE) {
  ## Catching Most Frequent Issues ===============
  if (closeConnections) {
    on.exit(closeAllConnections())
  }
  #--- File Name and Extension
  ### check if file name has been specified
  if (!exists("FileName")) {
    stop("Please provide a value for the FileName argument.")
  }
  FileName <- paste0(file_path_sans_ext(FileName), FileExtension)
  if (!(FileExtension %in% c(".nc", ".tif"))) {
    stop("Please specify a FileExtension of either '.tif' or '.nc'")
  }
  #--- Time Zone
  if (!(TZone %in% OlsonNames())) {
    stop("The TZone argument you have specified is not supported. Please refer to OlsonNames() for an overview of all supported specifications.")
  }

  #--- File Check
  Meta_vec <- c(Temperature_Var, Temperature_DataSet, Temperature_Type, Water_Var, Water_DataSet, Water_Type, Y_start, Y_end, TZone, as.character(quote(Extent)), Buffer, Dir, FileName)
  names(Meta_vec) <- c("Temperature_Var", "Temperature_DataSet", "Temperature_Type", "Water_Var", "Water_DataSet", "Water_Type", "Y_start", "Y_end", "TZone", "Extent", "Buffer", "Dir", "FileName")
  Meta_vec <- c(
    "Citation" = paste0("Bioclimatic variables obtained with KrigR (DOI:10.1088/1748-9326/ac48b3) on ", Sys.time()),
    "KrigRCall" = Meta_vec
  )
  FCheck <- Check.File(FName = FileName, Dir = Dir, loadFun = terra::rast, load = TRUE, verbose = TRUE)
  if (!is.null(FCheck)) {
    names(FCheck) <- paste0("BIO", 1:19)
    if (FileExtension == ".nc") {
      FCheck <- Meta.NC(NC = FCheck, FName = file.path(Dir, FileName), Attrs = Meta_vec, Read = TRUE)
    }
    terra::time(FCheck) <- as.POSIXct(terra::time(FCheck), tz = TZone) # assign the correct time zone, when loading from disk, time zone is set to UTC
    names(FCheck) <- paste0("BIO", 1:19)
    return(FCheck)
  }

  #--- API Credentials
  ### checking if API User and Key have been supplied
  if (exists("API_User") + exists("API_Key") != 2) {
    stop("Please provide a value for the API_User and API_Key arguments.")
  }
  ### making API_User into a character string
  API_User <- as.character(API_User)

  #--- Variable and product list
  Vars_ls <- list(
    list(
      Var = Temperature_Var,
      DataSet = Temperature_DataSet,
      Type = Temperature_Type
    ),
    list(
      Var = Water_Var,
      DataSet = Water_DataSet,
      Type = Water_Type
    )
  )

  #--- DataSets and Types
  MetaCheck <- lapply(lapply(Vars_ls, "[[", "DataSet"), Meta.QuickFacts)

  ## Projection mismatch
  ProjCheck <- sapply(1:length(MetaCheck), FUN = function(x) {
    MetaCheck[[x]]$Projection
  })
  if (length(unique(ProjCheck)) > 1) {
    stop("The CDS data products you have specified are stored in different projections. Please select data products via the *_DataSet arguments that use the same spatial projection.")
  }

  ## extent checking
  ExtCheck <- lapply(1:length(MetaCheck), FUN = function(x) {
    ext(MetaCheck[[x]]$CDSArguments$area)
  })
  if (!all.equal(ExtCheck[[1]], ExtCheck[[2]])) {
    stop("Please specify CDS products via the *_DataSet arguments which have matching spatial coverage.")
  }

  ## resolution mismatch
  ResCheck <- diff(
    sapply(1:length(MetaCheck), FUN = function(x) {
      MetaCheck[[x]]$SpatialResolution
    })
  )
  if (ResCheck != 0) {
    stop("Please specify CDS products via the *_DataSet arguments which have matching spatial resolution.")
  }

  #--- Year Specificiation
  if (Y_end == year(Sys.Date())) {
    stop("Please note that calculation of bioclimatic variables requires consideration of data sets spanning full years. Therefore, the current year cannot be included in the calculation of bioclimatic variables.")
  }

  ## Data Retrieval ===============
  message("###### Retrieving raw data ######")
  #--- Date handling
  Down_start <- paste0(Y_start, "-01-01 00:00")
  Down_end <- paste0(Y_end, "-12-31 23:00")
  M_seq <- seq(lubridate::date(Down_start), lubridate::date(Down_end), by = "month") + 15 # this is to ensure proper downloading of monthly recorded data despite TZone adjustments

  #--- Data download
  RawNames <- paste(file_path_sans_ext(FileName), sapply(Vars_ls, "[[", "Var"), "RAW", sep = "_")
  Raw_data <- lapply(1:length(Vars_ls), FUN = function(Var_Iter) {
    ## VARIABLE IDENTIFICATION
    Var_down <- Vars_ls[[Var_Iter]]$Var
    CumulVar <- ifelse(startsWith(prefix = "total_", Var_down), TRUE, FALSE)

    ## PROCESSING FUNCTIONS
    if (startsWith(prefix = "total_", Var_down)) {
      AggrFUN <- sum
    } else { # for all variables that are not total precip
      AggrFUN <- mean
    }

    ## CDownloadS CALL
    Raw_rast <- CDownloadS(
      Variable = Var_down,
      CumulVar = CumulVar,
      DataSet = Vars_ls[[Var_Iter]]$DataSet,
      Type = Vars_ls[[Var_Iter]]$Type,
      DateStart = Down_start,
      DateStop = Down_end,
      TZone = TZone,
      TResolution = ifelse(MetaCheck[[Var_Iter]]$TResolution == "month", "month", "day"),
      FUN = AggrFUN,
      Extent = Extent,
      Buffer = Buffer,
      Dir = Dir,
      FileName = RawNames[Var_Iter],
      FileExtension = FileExtension,
      Compression = Compression,
      API_User = API_User,
      API_Key = API_Key,
      TryDown = TryDown,
      TimeOut = TimeOut,
      TChunkSize = TChunkSize,
      Cores = Cores,
      verbose = verbose,
      Keep_Raw = FALSE,
      closeConnections = closeConnections
    )
    Raw_rast
  })

  ## Data Processing ===============
  message("###### Prcoessing raw data ######")
  #--- Month summaries
  if (startsWith(prefix = "total_", Water_Var)) {
    AggrFUN <- sum
    AggrFunName <- "sum"
  } else { # for all variables that are not total precip
    AggrFUN <- mean
    AggrFunName <- "mean"
  }
  MonthNames <- paste0(paste(file_path_sans_ext(FileName), c(rep("Temperature", 3), "Water"), "Monthly", c("mean", "min", "max", AggrFunName), sep = "_"), ".nc")
  Monthly <- list(
    list(
      Source = 1,
      AggrFUN = mean,
      Name = MonthNames[1]
    ),
    list(
      Source = 1,
      AggrFUN = min,
      Name = MonthNames[2]
    ),
    list(
      Source = 1,
      AggrFUN = max,
      Name = MonthNames[3]
    ),
    list(
      Source = 2,
      AggrFUN = AggrFUN,
      Name = MonthNames[4]
    )
  )
  names(Monthly) <- c("T_mean", "T_min", "T_max", "W_mean")
  Monthly <- lapply(Monthly, FUN = function(Monthly_Iter) {
    if (verbose) {
      print(paste("Producing", Monthly_Iter$Name))
    }
    MetaMonthly_vec <- metags(Raw_data[[Monthly_Iter$Source]])
    MetaMonthly_vec["KrigRCall.TResolution"] <- "month"
    MetaMonthly_vec["KrigRCall.FUN"] <- "Monthly_Iter$AggrFUN"
    FCheck <- Check.File(FName = Monthly_Iter$Name, Dir = Dir, loadFun = terra::rast, load = TRUE, verbose = TRUE)
    if (!is.null(FCheck)) {
      if (FileExtension == ".nc") {
        FCheck <- Meta.NC(NC = FCheck, FName = file.path(Dir, Monthly_Iter$Name), Attrs = MetaMonthly_vec, Read = TRUE)
      }
      terra::time(FCheck) <- as.POSIXct(terra::time(FCheck), tz = TZone) # assign the correct time zone, when loading from disk, time zone is set to UTC
      return(FCheck)
    }
    Monthly_rast <- Temporal.Aggr(
      CDS_rast = Raw_data[[Monthly_Iter$Source]],
      BaseResolution = "day", BaseStep = 1,
      TResolution = "month", TStep = 1, FUN = Monthly_Iter$AggrFUN,
      Cores = Cores, QueryTargetSteps = NULL, TZone = TZone, verbose = FALSE
    )
    terra::metags(Monthly_rast) <- MetaMonthly_vec
    if (FileExtension == ".tif") {
      terra::writeRaster(Monthly_rast, filename = file.path(Dir, Monthly_Iter$Name))
      Monthly_rast <- terra::rast(filename = file.path(Dir, Monthly_Iter$Name))
    }
    if (FileExtension == ".nc") {
      Monthly_rast <- Meta.NC(
        NC = Monthly_rast, FName = file.path(Dir, Monthly_Iter$Name),
        Attrs = MetaMonthly_vec, Write = TRUE,
        Compression = Compression
      )
      terra::time(Monthly_rast) <- as.POSIXct(terra::time(Monthly_rast), tz = TZone) # assign the correct time zone, when loading from disk, time zone is set to UTC
    }
    Monthly_rast
  })
  names(Monthly) <- c("T_mean", "T_min", "T_max", "W_mean")

  #--- Quarter summaries
  T_mean_quarter <- tapp(x = Monthly$T_mean, index = rep(1:4, each = 3, length.out = nlyr(Monthly$T_mean)), fun = mean, cores = Cores)
  W_mean_quarter <- tapp(x = Monthly$W_mean, index = rep(1:4, each = 3, length.out = nlyr(Monthly$W_mean)), fun = AggrFUN, cores = Cores)

  ## Bioclimatic Variables ===============
  message("###### Calculating aggregate metrics ######")
  ### BIO1 = Annual Mean Temperature ----
  BIO1 <- mean(Monthly$T_mean)

  ### BIO2 = Mean Diurnal Range (Mean of monthly (max temp - min temp)) ----
  BIO2 <- mean(Monthly$T_max - Monthly$T_min, na.rm = TRUE)

  ### BIO4 = Temperature Seasonality (standard deviation * 100) ----
  BIO4 <- app(Monthly$T_mean, sd) * 100

  ### Monthly Temperature ----
  T_mean <- values(Monthly$T_mean)
  T_max <- values(Monthly$T_max)
  T_min <- values(Monthly$T_min)
  BIO5 <- BIO6 <- T_mean_quarter[[1]]
  bio5vals <- bio6vals <- values(BIO5)[, 1]

  for (i in seq_along(bio5vals)) {
    if (is.na(bio5vals[i])) {
      next()
    }
    bio5vals[i] <- T_max[i, which.max(T_mean[i, ])]
    bio6vals[i] <- T_min[i, which.min(T_mean[i, ])]
  }

  #### BIO5 = Max Temperature of Warmest Month ----
  terra::values(BIO5) <- bio5vals

  #### BIO6 = Min Temperature of Coldest Month ----
  terra::values(BIO6) <- bio6vals

  ### BIO7 = Temperature Annual Range (BIO5-BIO6) ----
  BIO7 <- BIO5 - BIO6

  ### BIO3 = Isothermality (BIO2/BIO7) (*100) ----
  BIO3 <- BIO2 / BIO7 * 100

  ### BIO10 = Mean Temperature of Warmest Quarter ----
  BIO10 <- max(T_mean_quarter)

  ### BIO11 = Mean Temperature of Coldest Quarter ----
  BIO11 <- min(T_mean_quarter)

  ### BIO12 = Annual Precipitation ----
  if (startsWith(prefix = "total_", Water_Var)) {
    BIO12 <- sum(Monthly$W_mean) / (nlyr(Monthly$W_mean) / 12)
  } else {
    BIO12 <- mean(Monthly$W_mean)
  }

  ### BIO13 = Precipitation of Wettest Month ----
  BIO13 <- max(Monthly$W_mean)

  ### BIO14 = Precipitation of Driest Month ----
  BIO14 <- min(Monthly$W_mean)

  ### BIO15 = Precipitation Seasonality (Coefficient of Variation) ----
  BIO15 <- app(Monthly$W_mean, sd) / BIO12 * 100

  ### BIO16 = Precipitation of Wettest Quarter ----
  BIO16 <- max(W_mean_quarter)

  ### BIO17 = Precipitation of Driest Quarter ----
  BIO17 <- min(W_mean_quarter)

  ### Quarterly ----
  T_q <- values(T_mean_quarter)
  W_q <- values(W_mean_quarter)
  BIO8 <- BIO9 <- BIO18 <- BIO19 <- T_mean_quarter[[1]]
  bio8vals <- bio9vals <- bio18vals <- bio19vals <- values(BIO8)[, 1]

  for (i in seq_along(bio8vals)) {
    if (is.na(bio8vals[i])) {
      next()
    }
    bio8vals[i] <- T_q[i, which.max(W_q[i, ])]
    bio9vals[i] <- T_q[i, which.min(W_q[i, ])]
    bio18vals[i] <- W_q[i, which.max(T_q[i, ])]
    bio19vals[i] <- W_q[i, which.min(T_q[i, ])]
  }

  #### BIO8 = Mean Temperature of Wettest Quarter ----
  terra::values(BIO8) <- bio8vals

  #### BIO9 = Mean Temperature of Driest Quarter ----
  terra::values(BIO9) <- bio9vals

  ### BIO18 = Precipitation of Warmest Quarter ----
  terra::values(BIO18) <- bio18vals

  ### BIO19 = Precipitation of Coldest Quarter ----
  terra::values(BIO19) <- bio19vals

  ## Data Export ===============
  message("###### Data Export ######")
  BIO_rast <- c(
    BIO1, BIO2, BIO3, BIO4, BIO5, BIO6, BIO7, BIO8, BIO9,
    BIO10, BIO11, BIO12, BIO13, BIO14, BIO15, BIO16, BIO17, BIO18, BIO19
  )
  names(BIO_rast) <- paste0("BIO", 1:19)
  terra::metags(BIO_rast) <- Meta_vec

  ### write file
  if (FileExtension == ".tif") {
    terra::writeRaster(BIO_rast, filename = file.path(Dir, FileName))
    BIO_rast <- terra::rast(filename = file.path(Dir, FileName))
  }
  if (FileExtension == ".nc") {
    BIO_rast <- Meta.NC(
      NC = BIO_rast, FName = file.path(Dir, FileName),
      Attrs = Meta_vec, Write = TRUE,
      Compression = Compression
    )
  }
  names(BIO_rast) <- paste0("BIO", 1:19)

  ### cleaning up
  if (!Keep_Monthly) {
    unlink(file.path(Dir, MonthNames))
  }
  if (!Keep_Raw) {
    unlink(file.path(Dir, paste0(RawNames, ".nc")))
  }

  return(BIO_rast)
}
