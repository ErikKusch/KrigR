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
#' @param Dir Directory specifying where to download data to.
#' @param FileName A file name for the netcdf produced. Default is a combination parameters in the function call.
#' @param API_Key Character; ECMWF cds API key.
#' @param API_User Character; ECMWF cds user number.
#' @param TryDown Optional, numeric. How often to attempt the download of each individual file that the function queries from the server. This is to circumvent having to restart the entire function when encountering connectivity issues.
#' @param verbose Optional, logical. Whether to report progress of the function in the console or not.
#' @param Cores Numeric. How many cores to use.^This can speed up downloads of long time-series. If you want output to your console during the process, use Cores = 1. Parallel processing is carried out when Cores is bigger than 1. Default is 1.
#' @param TimeOut Numeric. The timeout for each download in seconds. Default 36000 seconds (10 hours).
#' @param ... Additional arguments used for parsing more information to the ecmwfr download call such as pressure_level = 1 for download of pressure_level data for the ERA5 reanalysis
#' @return A raster object containing the downloaded ERA5(-Land) data, and a NETCDF (.nc) file in the specified directory.
#' @examples
#' \dontrun{
#'
#' }
#' @export
CDownloadS <- function(Variable = NULL, # which variable
                       CumulVar = FALSE, # cumulative variable?
                       Type = NA, # type of data set
                       DataSet = "reanalysis-era5-land", # data set
                       DateStart, DateStop, TZone, # time-window, default set to range of dataset-type
                       TResolution = "month", TStep = 1, FUN = 'mean', # temporal aggregation
                       Extent, # spatial limitation, default set to range of dataset-type
                       Buffer = 0.5, # point buffering if desired
                       Dir = getwd(), FileName, # file storing
                       API_User, API_Key, # API credentials
                       TryDown = 10, TimeOut = 36000, # Calls to CDS
                       TChunkSize = 12000,
                       Cores = 1, # parallelisation
                       verbose = TRUE # verbosity
                       # ... # optional arguments per dataset-type
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
         "\n !! If you are seeing an NA ont he above line, note that this is not an error. Please specify NA as the Type.")
  }
  #--- File Name
  ### check if file name has been specified
  if(!exists("FileName")){stop("Please provide a value for the FileName argument.")}
  FileName <- paste0(tools::file_path_sans_ext(FileName), ".nc")

  ## The Request =================================
  if(verbose){message("###### CDS Request & Data Download")}

  ### Building =====
  if(verbose){print("Building request")}

  #--- Name resolving
  VarPos <- which(rowSums(Meta.Variables(dataset = DataSet)[,1:2] == Variable) != 0)
  QueryVariable <- Meta.Variables(dataset = DataSet)[VarPos,"CDSname"]

  #--- Extent resolving; formatting as SpatExtent object
  if(class(Extent)[1] == "data.frame"){
    Extent <- Buffer.pts(USER_pts = Make.SpatialPoints(USER_df = Extent),
                         USER_buffer = Buffer)
  }
  QueryExtent <- Check.Ext(Extent)[c(4,1,3,2)] #N,W,S,E

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
    QuerySeries = unlist(lapply(QueryTimeWindows, as.character)),
    DateStart = Dates_df$UTC[1],
    DateStop = Dates_df$UTC[2],
    TResolution = TResolution,
    BaseTResolution = BaseResolution,
    TStep = TStep,
    BaseTStep = BaseStep
  )

  ### Checking =====
  if(verbose){print("Checking request validity")}
  #--- File check, if already a file with this name present then load from disk
  FCheck <- Check.File(FName = FileName, Dir = Dir, loadFun = terra::rast, load = TRUE, verbose = TRUE)
  if(!is.null(FCheck)){
    return(FCheck) #!! check if this works as intended and stops the execution without an error
  }
  #--- Metadata check - can the queried dataset-type deliver the queried data?
  MetaCheck_ls <- Meta.Check(DataSet = DataSet,
                             Type = Type,
                             VariableCheck = QueryVariable,
                             CumulativeCheck = CumulVar,
                             ExtentCheck = QueryExtent,
                             DateCheck = Dates_df,
                             AggrCheck = list(TStep, TResolution),
                             QueryTimes = QueryTimes)

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
    terra::nlyr(terra::rast(LayerCheckIter))
  }))
  NLyrIssue <- which(NLyrCheck != unlist(lapply(QueryTimeWindows, length)))
  if(length(NLyrIssue) > 0){
    stop("Download of ", paste(basename(TempFs[NLyrIssue]), collapse = ", "), "produced file(s) of incorrect amount of layers. You may want to delete these files and try again. If the error persists. Please consult your queue on CDS: https://cds.climate.copernicus.eu/cdsapp#!/yourrequests. Alternatively, you may want to consult the corresponding download query/queries used behind the scenes:", paste(capture.output(str(Requests_ls[NLyrIssue])), collapse = "\n"))
  }

  #--- Loading data
  CDS_rast <- terra::rast(TempFs)
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
  KrigRCall <- match.call()
  KrigRCall <- KrigRCall[!(names(KrigRCall) %in% c("API_Key", "API_User"))]
  Meta_vec <- c(paste0(MetaCheck_ls$QueryDataSet, "data (DOI:", Meta.DOI("reanalysis-era5-land-monthly-means"), ") obtained with KrigR (DOI:10.1088/1748-9326/ac48b3) on ", Sys.time()), as.character(KrigRCall))
  names(Meta_vec) <- c("Citation", names(KrigRCall))
  terra::metags(CDS_rast) <- Meta_vec

  ### write file
  terra::writeCDF(x = CDS_rast, filename = file.path(Dir, FileName),
           varname = MetaCheck_ls$QueryVariable, unit = MetaCheck_ls$QueryUnit)

  ### unlink temporary files
  unlink(TempFs)

  ### return object
  return(CDS_rast)
}
