### REGISTER METADATA LIST =====================================================
#' Create a .txt tile holding the names of all supported data sets and their types
#'
#' To be run only by the developer when adding support for new data sets and types.
#'
#' @param Dir directory in which metadata files (.RData objects) are stored locally
#' @return Nothing. But does write a .txt file into the specified directory.
Meta.Register <- function(Dir = file.path(getwd(), "metadata")){
  sink(file = file.path(Dir, "metadata.txt"))
  cat(list.files(Dir, ".rds"), sep = "\n")
  sink()
}

### READ METADATA LIST =========================================================
#' List out all supported data sets
#'
#' Provide an overview of all data sets for which metadata files are present.
#'
#' @param URL Path to where metadata files reside. Should not be changed from default.
#' @return A vector of supported datasets.
#' @examples
#' Meta.List()
#'
#' @export
Meta.List <- function(URL = "https://raw.githubusercontent.com/ErikKusch/KrigR/Development/metadata" ## change this to github repo for these data once ready
                      ){
  tools::file_path_sans_ext(read.table(file.path(URL, "metadata.txt"))[,1])
}

### READ METADATA FACTS ========================================================
#' Data set overview
#'
#' Read and return metadata for specific data set.
#'
#' @param URL Path to where metadata files reside. Should not be changed from default.
#' @param dataset Character. Name of data set. Usually a set of words separated by dashes. See possible datasets by calling `Meta.List()`.
#' @return List. Contains information of data set, type, variables, resolution, citation, etc.
#' @examples
#' Meta.Read()
#'
#' @export
Meta.Read <- function(URL = "https://raw.githubusercontent.com/ErikKusch/KrigR/Development/metadata", ## change this to github repo for these data once ready
                      dataset = "reanalysis-era5-land"){
  load(url(
    paste0(
    "https://github.com/ErikKusch/KrigR/blob/Development/metadata/",
    dataset,
    ".RData?raw=true"
    )
  ))
  get(ls()[ls() == gsub(dataset, pattern = "-", replacement = "_")])
}

### DATASET VARIABLES ==========================================================
#' Variables available within data set
#'
#' Read and return overview of variables available for specific data set.
#'
#' @param dataset Character. Name of data set. Usually a set of words separated by dashes. See possible datasets by calling `Meta.List()`.
#' @return Data frame. Contains five columns: (1) Variable (clear name), (2) CDSname (name required for CDS query), (3) Description (plain text description of variable, scraped from CDS webpage), (4) Unit (unit of measurement), and (5) Cumulative (logical, indexing whether a variable is recorded cummulatively or not).
#' @examples
#' Meta.Variables()
#'
#' @export
Meta.Variables <- function(dataset = "reanalysis-era5-land"){
  Meta.Read(dataset = dataset)$Variables
}

### DATASET CITATION ===========================================================
#' DOI of data set
#'
#' Read and return DOI of data set for easy citation.
#'
#' @param dataset Character. Name of data set. Usually a set of words separated by dashes. See possible datasets by calling `Meta.List()`.
#' @return Character. DOI string for data set.
#' @examples
#' Meta.DOI()
#'
#' @export
Meta.DOI <- function(dataset = "reanalysis-era5-land"){
  Meta.Read(dataset = dataset)$Citation
}

### DATASET QUICK FACTS ========================================================
#' Fact sheet overview of data set
#'
#' Read and return short overview of data set characteristics, supported types, extent, time frames and required arguments.
#'
#' @param dataset Character. Name of data set. Usually a set of words separated by dashes. See possible datasets by calling `Meta.List()`.
#' @return List. Contains (1) DataSet (data set string), (2) Type (character, supported types of the data set), (3) URL (character, url of CDS webpage corresponding to data set), (3) Description (character, plain text description of data set scraped from CDS), (4) TResolution (character, base temporal resolution of each layer in data set), (5) TStep (numeric, vector of time step between layers in data set corresponding to Type), (6) TStart (POSIXct, date and time at which first layer is available), (7) TEnd (POSIXct or character, date and time at which first layer is available), (7) Projection (crs of data set), (8) SpatialResolution (numeric, resolution of data set in space in degrees), (9) CDS arguments (list, required arguments for CDS call beyond standard arguments and also reporting default/options for common CDS query arguments)
#' @examples
#' Meta.QuickFacts()
#'
#' @export
Meta.QuickFacts <- function(dataset = "reanalysis-era5-land"){
  Meta.Read(dataset = dataset)[c("DataSet", "Type", "URL", "Description",
                                 "TResolution", "TStep", "TStart", "TEnd",
                                 "Projection", "SpatialResolution",
                                 "CDSArguments")]
}

### CDS QUERY VALIDATION AGAINST DATA SET METADATA =============================
#' Fact sheet overview of data set
#'
#' Read and return short overview of data set characteristics, supported types, extent, time frames and required arguments.
#'
#' @param dataset Character. Name of data set. Usually a set of words separated by dashes. See possible datasets by calling `Meta.List()`.
#' @param Type = NA, VariableCheck, CumulativeCheck, ExtentCheck,  DateCheck, AggrCheck, QueryTimes
#' @return List. Contains (1) DataSet (data set string), (2) Type (character, supported types of the data set), (3) URL (character, url of CDS webpage corresponding to data set), (3) Description (character, plain text description of data set scraped from CDS), (4) TResolution (character, base temporal resolution of each layer in data set), (5) TStep (numeric, vector of time step between layers in data set corresponding to Type), (6) TStart (POSIXct, date and time at which first layer is available), (7) TEnd (POSIXct or character, date and time at which first layer is available), (7) Projection (crs of data set), (8) SpatialResolution (numeric, resolution of data set in space in degrees), (9) CDS arguments (list, required arguments for CDS call beyond standard arguments and also reporting default/options for common CDS query arguments)
#' @examples
#' Meta.Check(DataSet = "reanalysis-era5-land", Type = NA, VariableCheck = "2m_temperature", CumulativeCheck = FALSE, ExtentCheck = c(53.06, 9.87, 49.89, 15.03), DateCheck = data.frame(IN = c(as.POSIXct("1995-01-01 CET"), as.POSIXct("2005-01-01 23:00:00 CET")), UTC = c(as.POSIXct("1994-12-31 23:00:00 UTC"), as.POSIXct("2005-01-01 22:00:00 UTC"))), AggrCheck = list(1, "hour"), QueryTimes = c('00:00', '03:00', '06:00', '09:00', '12:00', '15:00', '18:00', '21:00'))
#'
Meta.Check <- function(DataSet = "reanalysis-era5-land", Type = NA, VariableCheck, CumulativeCheck, ExtentCheck,  DateCheck, AggrCheck, QueryTimes){
  #' Variable
  ### if a variable not in the data set has been specified
  if(length(VariableCheck) == 0){stop("Please specify a variable provided by the data set. Your can be retrieved with the function call: ", "\n", "Meta.Variables(dataset = '", DataSet, "')")}
  #' Cumulative
  ### if the cumulative back-calculation is attempting to be applied to a non-cumulative variable
  CumVar <- Meta.Variables(dataset = DataSet)$Cumulative[which(Meta.Variables(dataset = DataSet)$CDSname == VariableCheck)]
  if(CumulativeCheck & !CumVar){
    stop("You have specified to back-calculation of cumulative data for a non-cumulatively recorded variable. This would produce nonsense data. Please specify CumulVar = FALSE instead. For an overview of which variables are recorded cumulatively for the data set you are querying, please consider the function call:", "\n", "Meta.Variables(dataset = '", DataSet, "')")
  }
  #' Extent
  ### if an extent outside the data product has been specified
  DataExt <- terra::ext(Meta.QuickFacts(dataset = DataSet)$CDSArguments$area)[c(4,1,3,2)] #N,W,S,E
  if(
    (
      # ymax
      (ExtentCheck[1] > DataExt[1]) +
      # xmin
      (ExtentCheck[2] < DataExt[2]) +
      # ymin
      (ExtentCheck[3] < DataExt[3]) +
      # ymax
      (ExtentCheck[4] > DataExt[4])
    ) != 0
  ){
    stop("Please specify an area using the Extent argument that is contained within the data set. The data set covers the area defined by the following extent:",
         "\n", terra::ext(Meta.QuickFacts(dataset = DataSet)$CDSArguments$area), " in ", Meta.QuickFacts(dataset = DataSet)$Projection)
  }
  #' Time
  #' Time Zone
  if(format(DateCheck$IN[1], "%Z") != format(DateCheck$IN[2], "%Z")){
    stop("Please provide the DateStart and DateStop Arguments using the same time zone.")
  }
  #'  Window
  ### check if time window is exceeded
  CheckStart <- DateCheck$UTC[1] < Meta.QuickFacts(dataset = DataSet)$TStart
  if(class(Meta.QuickFacts(dataset = DataSet)$TEnd)[1] == "POSIXct"){
    CheckEnd <- (DateCheck$UTC[2] > Meta.QuickFacts(dataset = DataSet)$TEnd)
  }else{
    CheckEnd <- FALSE
    warning("Cannot validate user-specified end date (DateStop) because specified data set is being updated regularly (",
            strsplit(Meta.QuickFacts(dataset = DataSet)$TEnd, split = "; ")[[1]][2], "). User-specification may lead to an error.")
  }
  if(CheckStart + CheckEnd != 0){
    stop("The time window you have specified is not supported by the data set. The data set makes data available from ",
         Meta.QuickFacts(dataset = DataSet)$TStart, " until ", Meta.QuickFacts(dataset = DataSet)$TEnd)
  }
  #'  Aggregation Match
  ### check if desired aggregation is supported
  SuppRes <- c("hour", "day", "month", "year")
  BaseStep <- BaseStep <- Meta.QuickFacts(dataset = DataSet)$TStep[
    na.omit(match(Type, Meta.QuickFacts(dataset = DataSet)$Type))]
  if(Meta.QuickFacts(dataset = DataSet)$TResolution != AggrCheck[[2]] |
     BaseStep != AggrCheck[[1]]){ # if this is TRUE, we need to check if aggregation works

    ## specification of a temporal resolution finer than the data?
    if(which(SuppRes == AggrCheck[[2]]) < which(SuppRes == Meta.QuickFacts(dataset = DataSet)$TResolution)){
      stop("You have specified a temporal aggregation at a scale finer than what the data set reports natively (",
           Meta.QuickFacts(dataset = DataSet)$TResolution, "). Please specify the same or a coarser temporal resolution for the TResolution argument. Supported options are '", paste(SuppRes, collapse = "', '"), "'.")
    }

    ## specification of tsteps that cannot be achieved with the data?
    if(Meta.QuickFacts(dataset = DataSet)$TResolution == AggrCheck[[2]] &
       ((AggrCheck[[1]] /BaseStep) %%1!=0)){
      stop("You have specified a temporal aggregation that cannot be achieved with the data. When specifying the same temporal resolution as the data (you have specified TResolution = ", AggrCheck[[2]], "), the TStep must be a multiple of the base temporal resolution of the data (", BaseStep, " for DataSet = ", DataSet, " and Type = ", Type, ").")
    }

    ## specification of daily, monthly or annual aggregates but not setting tstart or tend to beginning or end of day/month/year?
    if(AggrCheck[[2]] == "day" &
       (as.numeric(substr(QueryTimes[1], 0, 2)) != 0 |
        as.numeric(substr(QueryTimes[length(QueryTimes)], 0, 2)) != 23)){
      stop("You have specified (multi-)daily temporal aggregation but are querying a time window which does not start at 00:00 and/or does not terminate at 23:00. Please ensure that you set the argument DateStart and DateStop accordingly.")
    }

    ## these may fail when querying  monthly raw data
    MustStartMonth <- as.POSIXct(paste(
      paste(format(DateCheck$IN[1], "%Y"), format(DateCheck$IN[1], "%m"), "01", sep = "-"),
      "00:00:00", tz = format(DateCheck$IN[2], "%Z")
    ))
    MustEndMonth <- as.POSIXct(paste(
      paste(format(DateCheck$IN[2], "%Y"), format(DateCheck$IN[2], "%m"),
            lubridate::days_in_month(DateCheck$IN[2]), sep = "-"),
      "24:00:00", tz = format(DateCheck$IN[2], "%Z")))
    if(AggrCheck[[2]] == "month" &
       (DateCheck$IN[1] != MustStartMonth |
        DateCheck$IN[2] != MustEndMonth)){
      stop("You have specified (multi-)monthly temporal aggregation but are querying a time window which does not start at the first day of a month at 00:00 and/or does not terminate on the last day of a month at 24:00. Please ensure that you set the argument DateStart and DateStop accordingly.")
    }

    MustStartYear <- as.POSIXct(paste(
      paste(format(DateCheck$IN[1], "%Y"), "01-01", sep = "-"),
      "00:00:00", tz = format(DateCheck$IN[2], "%Z")
    ))
    MustEndYear <- as.POSIXct(paste(
      paste(format(DateCheck$IN[2], "%Y"), "12-31", sep = "-"),
      "23:00:00", tz = format(DateCheck$IN[2], "%Z")))
    if(AggrCheck[[2]] == "year" &
       (DateCheck$IN[1] != MustStartYear |
        DateCheck$IN[2] != MustEndYear)){
      stop("You have specified (multi-)yearly temporal aggregation but are querying a time window which does not start at the first of day of a year at 00:00 and/or does not terminate on the last day of a year at 23:00. Please ensure that you set the argument DateStart and DateStop accordingly.")
    }
  }

  #' Format, assign default file type for download
  QueryFormat <- Meta.QuickFacts(dataset = DataSet)$CDSArguments$format[1]

  #' Report back
  list(
    QueryDataSet = DataSet,
    QueryType = Type,
    QueryVariable = VariableCheck,
    QueryFormat = QueryFormat,
    QueryUnit = Meta.Variables(dataset = DataSet)$Unit[which(Meta.Variables(dataset = DataSet)$CDSname == VariableCheck)])
}


