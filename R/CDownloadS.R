#' Downloading Data from ECMWF Climate Data Store
#'
#' This function is used to obtain data from the \href{https://cds.climate.copernicus.eu/#!/home}{Climate Data Store (CDS)} hosted by the \href{https://cds.climate.copernicus.eu/about-c3s}{Copernicus Climate Change Service (C3S)}. By default, this function breaks down download calls into intervals so as to avoid submitting queries which fail, downloads queried data from \href{https://www.ecmwf.int/}{ECMWF} servers according to user-specification, and fuses the downloaded files together according to user-demands. The actual time to download is dependent on ECMWF download queues. Users need an \href{https://cds.climate.copernicus.eu/api-how-to}{API key} for download staging and accept terms and conditions for the specific queried dataset(s).
#'
#' @param Variable Character. Desired variable from queried dataset. See \code{\link{Meta.Variables}} for options per dataset.
#' @param CumulVar Logical. Some ECMWF CDS data is recorded in cumulative steps per hour/month from the 00:00 time mark per day. Setting CumulVar to TRUE converts these into records which represent the total records per hour using the \code{\link{Temporal.Cumul}} function. Monthly cumulative records express the average daily total value. Setting this argument to TRUE multiplies monthly records by the number of days per the respective month(s) to get to total records instead of average. Default is FALSE. This argument can only be set to TRUE for cumulatively recorded variables. See \code{\link{Meta.Variables}} for an overview of which variables at recorded cumulatively per dataset.
#' @param DataSet Character. Which dataset to query data from. See currently supported datasets by calling \code{\link{Meta.List}}.
#' @param Type Either NA or Character. Which kind of sub-type to query per data set. See \code{\link{Meta.QucikFacts}} for options per dataset.
#' @param DateStart Character. Date ('YYYY-MM-DD HH:SS') at which to start time series of downloaded data.
#' @param DateStop Character. Date ('YYYY-MM-DD HH:SS') at which to stop time series of downloaded data.
#' @param TZone Character. Time zone in which to represent and evaluate time dimension of data. See the output of OlsonNames() for a full overview of supported specifications. Default is UTC.
#' @param TResolution Character. Temporal resolution of final product. 'hour', 'day', 'month', or 'year'.
#' @param TStep Numeric. Which time steps to consider for temporal resolution. For example, specify bi-monthly data records by setting TResolution to 'month' and TStep to 2.
#' @param FUN A raster calculation argument as passed to `terra::tapp()`. This controls what kind of data to obtain for temporal aggregates of reanalysis data. Specify 'mean' (default) for mean values, 'min' for minimum values, and 'max' for maximum values, among others.
#' @param Extent Optional, download data according to desired spatial specification. If missing/unspecified, total area of queried data set is used. Can be specified either as a raster object, an sf object, a terra object, or a data.frame. If Extent is a raster or terra object, data will be queried according to rectangular extent thereof. If Extent is an sf (MULTI-)POLYGON object, this will be treated as a shapefile and the output will be cropped and masked to this shapefile. If Extent is a data.frame of geo-referenced point records, it needs to contain Lat and Lon columns around which a buffered shapefile will be created using the Buffer argument.
#' @param Buffer Optional, Numeric. Identifies how big a circular buffer to draw around points if Extent is a data.frame of points. Buffer is expressed as centessimal degrees.
#' @param Dir Character/Directory Pointer. Directory specifying where to download data to.
#' @param FileName Character. A file name for the produced file.
#' @param FileExtension Character. A file extension for the produced file. Suggested values are ".nc" (default) and ".tif" (better support for metadata).
#' @param API_Key Character; ECMWF cds API key.
#' @param API_User Character; ECMWF cds user number.
#' @param TryDown Optional, numeric. How often to attempt the download of each individual file that the function queries from the CDS. This is to circumvent having to restart the entire function when encountering connectivity issues.
#' @param TimeOut Numeric. The timeout for each download in seconds. Default 36000 seconds (10 hours).
#' @param TChunkSize Numeric. Number of layers to bundle in each individual download. Default is 6000 to adhere to most restrictive CDS limits: https://cds.climate.copernicus.eu/live/limits.
#' @param Cores Numeric. How many cores to use when carrying out temporal aggregation. Default is 1.
#' @param verbose Logical. Whether to print/message function progress in console or not.
#' @param Keep_Raw Logical. Whether to retain raw downloaded data or not. Default is FALSE.
#' @param Save_Final Logical. Whether to write the final SpatRaster to the hard drive. Default is TRUE.
#'
#' @importFrom tools file_path_sans_ext
#' @importFrom terra rast
#' @importFrom terra ext
#' @importFrom terra time
#' @importFrom terra nlyr
#' @importFrom terra varnames
#' @importFrom terra units
#' @importFrom terra metags
#' @importFrom terra writeRaster
#'
#' @return A SpatRaster object containing the downloaded, cropped/masked, and subsequently temporally aggregated data, and a file (either .nc or .tif) in the specified directory.
#'
#' The SpatRaster contains metadata/attributes as a named vector that can be retrieved with terra::metags(...):
#' \itemize{
#' \item{Citation}{ - A string which to use for in-line citation of the data product obtained with CDownloadS}.
#' \item{KrigRCall.X}{ - Arguments passed to the CDownloadS function that produced the file (API credentials are omitted from these metadata)}.
#' }
#'
#' \strong{ATTENTION:} If data is loaded again from disk at a later point with a different function, take note that the time zone will have to be set anew and existing time parameters in the .nc contents will need to be converted to the desired time zone. Likewise, citation and KrigR-call metadata will not be loaded properly from a .nc when loading data through a different function. CDownloads() handles these .nc specific issues when loading .nc files created previously with CDownloadS from disk.
#'
#' @examples
#' \dontrun{
#' ## Raw data for one month of full globe
#' RawGlobe_rast <- CDownloadS(
#' 	Variable = "2m_temperature",
#' 	DataSet = "reanalysis-era5-land-monthly-means",
#' 	Type = "monthly_averaged_reanalysis",
#' 	# time-window, default set to range of dataset-type
#' 	DateStart = "1995-01-01 00:00",
#' 	DateStop = "1995-01-01 23:00",
#' 	TZone = "CET",
#' 	# temporal aggregation
#' 	TResolution = "month",
#' 	TStep = 1,
#' 	# file storing
#' 	FileName = "RawGlobe",
#' 	# API credentials
#' 	API_User = API_User,
#' 	API_Key = API_Key
#' )
#' terra::plot(RawGlobe_rast)
#'
#' ## Monthly air temperature aggregated to bi-annual maximum by SpatRaster
#' CDS_rast <- terra::rast(system.file("extdata", "CentralNorway.nc", package="KrigR"))
#' BiAnnAirTemp_rast <- CDownloadS(
#' 	Variable = "2m_temperature",
#' 	DataSet = "reanalysis-era5-land-monthly-means",
#' 	Type = "monthly_averaged_reanalysis",
#' 	# time-window, default set to range of dataset-type
#' 	DateStart = "1995-01-01 00:00",
#' 	DateStop = "1996-12-31 23:00",
#' 	TZone = "EET",
#' 	# temporal aggregation
#' 	TResolution = "year",
#' 	TStep = 2,
#' 	# spatial
#' 	Extent = CDS_rast,
#' 	# file storing
#' 	FileName = "BiAnnAirTemp",
#' 	# API credentials
#' 	API_User = API_User,
#' 	API_Key = API_Key
#' )
#' terra::plot(BiAnnAirTemp_rast)
#'
#' ## Hourly back-calculated precipitation aggregated to daily averages by shapefiles
#' data("Jotunheimen_poly")
#' Jotunheimen_poly
#' DailyBackCPrecip_rast <- CDownloadS(
#'   Variable = "total_precipitation",
#'   CumulVar = TRUE,
#'   # time-window, default set to range of dataset-type
#'   DateStart = "1995-01-01 00:00",
#'   DateStop = "1995-01-03 23:00",
#'   TZone = "CET",
#'   # temporal aggregation
#'   TResolution = "day",
#'   # spatial
#'   Extent = Jotunheimen_poly,
#'   # file storing
#'   FileName = "DailyBackCPrecip",
#'   # API credentials
#'   API_User = API_User,
#'   API_Key = API_Key
#' )
#' terra::plot(DailyBackCPrecip_rast)
#'
#' ## 6-hourly ensemble member spread sum for air temperature by buffered points
#' data("Mountains_df")
#' EnsembleSpreadSum6hour_rast <- CDownloadS(
#' 	Variable = "2m_temperature",
#' 	DataSet = "reanalysis-era5-single-levels",
#' 	Type = "ensemble_spread",
#' 	# time-window, default set to range of dataset-type
#' 	DateStart = "1995-01-01 00:00:00",
#' 	DateStop = "1995-01-01 21:00:00",
#' 	TZone = "UTC",
#' 	# temporal aggregation
#' 	TResolution = "hour",
#' 	TStep = 6,
#' 	FUN = sum,
#' 	# spatial
#' 	Extent = Mountains_df,
#' 	Buffer = 0.2,
#' 	# file storing
#' 	FileName = "EnsembleSpreadSum6hour",
#' 	FileExtension = ".tif",
#' 	# API credentials
#' 	API_User = API_User,
#' 	API_Key = API_Key,
#' 	Keep_Raw = TRUE
#' )
#' terra::plot(EnsembleSpreadSum6hour_rast)
#' }
#' @export
CDownloadS <- function(Variable = NULL, # which variable
                       CumulVar = FALSE, # cumulative variable?
                       DataSet = "reanalysis-era5-land", # data set
                       Type = NA, # type of data set
                       DateStart, DateStop, TZone = "UTC", # time-window, default set to range of dataset-type
                       TResolution = "month", TStep = 1, FUN = 'mean', # temporal aggregation
                       Extent, # spatial limitation, default set to range of dataset-type
                       Buffer = 0.5, # point buffering if desired
                       Dir = getwd(), FileName, FileExtension = ".nc", # file storing
                       API_User, API_Key, # API credentials
                       TryDown = 10, TimeOut = 36000, # Calls to CDS
                       TChunkSize = 6000,
                       Cores = 1, # parallelisation
                       verbose = TRUE, # verbosity
                       Keep_Raw = FALSE,
                       Save_Final = TRUE
                       ){
  ## Catching Most Frequent Issues ===============
  #--- API Credentials
  ### checking if API User and Key have been supplied
  if(exists("API_User") + exists("API_Key") != 2){
    stop("Please provide a value for the API_User and API_Key arguments.")
  }
  ### making API_User into a character string
  API_User <- as.character(API_User)
  #--- Data set & Type
  ### checking if a supported data set has been queried
  if(!(DataSet %in% Meta.List())){
    stop("Please specify a supported dataset as the DataSet argument. Your options are:",
         "\n", paste(Meta.List(), collapse = (" \n")))
    }
  if(!(Type %in% Meta.QuickFacts(dataset = DataSet)$Type)){
    stop("Please specify a Type argument that is supported by your chosen data set. Your options are:",
         "\n", paste(Meta.QuickFacts(dataset = DataSet)$Type, collapse = (" \n")),
         "\n !! If you are seeing an NA on the above line, note that this is not an error. Please specify NA as the Type.")
  }
  #--- File Name and Extension
  ### check if file name has been specified
  if(!exists("FileName")){stop("Please provide a value for the FileName argument.")}
  FileName <- paste0(file_path_sans_ext(FileName), FileExtension)
  if(!(FileExtension %in% c(".nc", ".tif"))){stop("Please specify a FileExtension of either '.tif' or '.nc'")}

  #--- Time Zone
  if(!(TZone %in% OlsonNames())){stop("The TZone argument you have specified is not supported. Please refer to OlsonNames() for an overview of all supported specifications.")}

  ## The Request =================================
  if(verbose){message("###### CDS Request & Data Download")}

  ### Building =====
  if(verbose){print("Building request")}

  #--- Name resolving
  VarPos <- which(rowSums(Meta.Variables(dataset = DataSet)[,1:2] == Variable) != 0)
  QueryVariable <- Meta.Variables(dataset = DataSet)[VarPos,"CDSname"]

  #--- Extent resolving; formatting as SpatExtent object
  if(missing(Extent)){Extent <- ext(Meta.QuickFacts(dataset = DataSet)$CDSArguments$area)} ## assign maximum extent for dataset if not specified
  if(class(Extent)[1] == "data.frame"){
    Extent <- Buffer.pts(USER_pts = Make.SpatialPoints(USER_df = Extent),
                         USER_buffer = Buffer)
  }
  QuerySpace <- Ext.Check(Extent)
  QueryExtent <- QuerySpace$Ext[c(4,1,3,2)] #N,W,S,E
  Extent <- QuerySpace$SpatialObj # terra/sf version of input extent to be used for easy cropping and masking

  #--- Base Dataset Information
  BaseResolution <- Meta.QuickFacts(dataset = DataSet)$TResolution
  BaseStep <- Meta.QuickFacts(dataset = DataSet)$TStep[
    na.omit(match(Type, Meta.QuickFacts(dataset = DataSet)$Type))]
  BaseStart <- Meta.QuickFacts(dataset = DataSet)$TStart

  if(BaseResolution == "hour" & CumulVar){
    DateStop <- as.character(as.POSIXct(DateStop, tz = TZone)+1*60*60*24) # add one day to hourly pulls when cumulVar is turned on as an extra layer of data is needed for proper backcalculation
  }

  #--- Time windows
  Dates_df <- Make.UTC(DatesVec = c(
    as.POSIXct(DateStart, tz = TZone),
    as.POSIXct(DateStop, tz = TZone))
  )
  QueryTimeWindows <- Make.RequestWindows(Dates_df = Dates_df,
                                          BaseTResolution = BaseResolution,
                                          BaseTStep = 24/BaseStep,
                                          BaseTStart = BaseStart,
                                          TChunkSize = TChunkSize,
                                          DataSet = DataSet
  )
  QueryTimes <- QueryTimeWindows$QueryTimes
  QueryTimeWindows <- QueryTimeWindows$QueryTimeWindows

  #--- Aggregation Check
  QueryTargetSteps <- TemporalAggregation.Check(
    QuerySeries = paste(unlist(lapply(QueryTimeWindows, as.character)), QueryTimes),
    DateStart = Dates_df$UTC[1],
    DateStop = Dates_df$UTC[2],
    TResolution = TResolution,
    BaseTResolution = BaseResolution,
    TStep = TStep,
    BaseTStep = BaseStep
  )

  ### Checking =====
  if(verbose){print("Checking request validity")}

  #--- Metadata check - can the queried dataset-type deliver the queried data?
  MetaCheck_ls <- Meta.Check(DataSet = DataSet,
                             Type = Type,
                             VariableCheck = QueryVariable,
                             CumulativeCheck = CumulVar,
                             ExtentCheck = QueryExtent,
                             DateCheck = Dates_df,
                             AggrCheck = list(TStep, TResolution),
                             QueryTimes = QueryTimes)

  ## File/Call metadata
  KrigRCall <- match.call()
  KrigRCall <- KrigRCall[!(names(KrigRCall) %in% c("API_Key", "API_User"))]
  Meta_vec <- as.character(KrigRCall)
  names(Meta_vec) <- names(KrigRCall)
  Meta_vec <- c(
    "Citation" = paste0(MetaCheck_ls$QueryDataSet, " data (DOI:", Meta.DOI("reanalysis-era5-land-monthly-means"), ") obtained with KrigR (DOI:10.1088/1748-9326/ac48b3) on ", Sys.time()),
    "KrigRCall" = Meta_vec
  )

  #--- File check, if already a file with this name present then load from disk
  FCheck <- Check.File(FName = FileName, Dir = Dir, loadFun = terra::rast, load = TRUE, verbose = TRUE)
  if(!is.null(FCheck)){
    if(FileExtension == ".nc"){
      FCheck <- Meta.NC(NC = FCheck, FName = file.path(Dir, FileName), Attrs = Meta_vec, Read = TRUE)
    }
    terra::time(FCheck) <- as.POSIXct(terra::time(FCheck), tz = TZone) # assign the correct time zone, when loading from disk, time zone is set to UTC
    return(FCheck)
  }

  ### Executing =====
  if(verbose){print("Executing request - Notice that time windows may vary slightly at this step due to timezone conversions. This will be resolved automatically.")}
  #--- API credentials
  Register.Credentials(API_User, API_Key)
  #--- Make list of CDS Requests
  Requests_ls <- Make.Request(QueryTimeWindows,
                              MetaCheck_ls$QueryDataSet, MetaCheck_ls$QueryType, MetaCheck_ls$QueryVariable,
                              QueryTimes, QueryExtent, MetaCheck_ls$QueryFormat,
                              Dir, verbose = TRUE, API_User, API_Key)
  ## work an on.exit in here to allow restarting downloads themselves without new queries
  #--- Execution of requests
  Execute.Requests(Requests_ls, Dir, API_User, API_Key, TryDown, verbose = TRUE)

  ## The Data =================================
  if(verbose){message("###### Data Checking & Limitting & Aggregating")}
  TempFs <- file.path(Dir, unlist(lapply(strsplit(names(Requests_ls), " "), "[[", 2)))
  ## Checking =====
  if(verbose){print("Checking for known data issues")}
  #--- layers
  NLyrCheck <- unlist(lapply(TempFs, FUN = function(LayerCheckIter){
    nlyr(rast(LayerCheckIter))
  }))
  NLyrIssue <- which(NLyrCheck != unlist(lapply(QueryTimeWindows, length)))
  if(length(NLyrIssue) > 0){
    stop("Download of ", paste(basename(TempFs[NLyrIssue]), collapse = ", "), " produced file(s) of incorrect amount of layers. You may want to delete these files and try again. If the error persists. Please consult your queue on CDS: https://cds.climate.copernicus.eu/cdsapp#!/yourrequests. Alternatively, you may want to consult the corresponding download query/queries used behind the scenes:", paste(capture.output(str(Requests_ls[NLyrIssue])), collapse = "\n"))
  }

  #--- Loading data
  CDS_rast <- rast(TempFs)
  terra::time(CDS_rast) <- as.POSIXct(terra::time(CDS_rast), tz = TZone) # assign time in queried timezone
  ## subset to desired time
  CDS_rast <- CDS_rast[[which(!(terra::time(CDS_rast) < Dates_df$IN[1] | terra::time(CDS_rast) > Dates_df$IN[2]))]]

  ## Spatial =====
  if(verbose){print("Spatial Limiting")}
  CDS_rast <- Handle.Spatial(CDS_rast, Extent)

  ## Temporal =====
  if(verbose){print("Temporal Aggregation")}
  #--- Cumulative Fix
  CDS_rast <- Temporal.Cumul(CDS_rast, CumulVar, BaseResolution, BaseStep, TZone)

  #--- Temporal aggregation
  CDS_rast <- Temporal.Aggr(CDS_rast, BaseResolution, BaseStep,
                            TResolution, TStep, FUN, Cores, QueryTargetSteps, TZone)

  ## Exports =================================
  if(verbose){message("###### Data Export & Return")}

  ### Assign additional information
  terra::varnames(CDS_rast) <- MetaCheck_ls$QueryVariable
  terra::units(CDS_rast) <- MetaCheck_ls$QueryUnit
  terra::metags(CDS_rast) <- Meta_vec

  ### write file
  if(Save_Final){
    if(FileExtension == ".tif"){
      terra::writeRaster(CDS_rast, filename = file.path(Dir, FileName))
    }
    if(FileExtension == ".nc"){
      CDS_rast <- Meta.NC(NC = CDS_rast, FName = file.path(Dir, FileName),
                          Attrs = terra::metags(CDS_rast), Write = TRUE)
    }
  }

  ### unlink temporary files
  if(!Keep_Raw){
    unlink(TempFs)
  }

  ### return object
  return(CDS_rast)
}
