### FILE EXISTENCE CHECKING ====================================================
#' Checking if a file already exists
#'
#' If a file already exists in a given place, load that file
#'
#' @param FName File name without file ending
#' @param Dir Directory where to look for file
#' @param loadFun function with which to load filetype of FName
#' @param load Logical. Whether to load the data or not
#' @param verbose whether to report what is happening
#' @return Either a data object or NULL
#' @examples
#' \dontrun{
#'
#' }
#' @export
Check.File <- function(FName, Dir = getwd(), loadFun, load = TRUE, verbose = TRUE){
  FNAME <- file.path(Dir, FName)
  file <- NULL
  if(file.exists(FNAME)){
    if(verbose){print(paste0("A file with the name ", FName, " already exists in ", Dir,
                             ". Loading this file for you from the disk."))}
    if(load){
      file <- sapply(FNAME, loadFun)
    }else{
      file <- "Present. Not Loaded."
      }
  }
  return(file)
}

### TEMPORAL AGGREGATION CHECK =================================================
#' Checking temporal aggregation can use all queried data
#'
#' Error message if specified aggregation and time window clash.
#'
#' @param QuerySeries Character. Vector of dates/times queried for download.
#' @param DateStart UTC start date.
#' @param DateStop UTC stop date.
#' @param TResolution User-specified temporal resolution for aggregation.
#' @param BaseTResolution Dataset-specific native temporal resolution.
#' @param TStep User-specified time step for aggregation.
#' @param BaseTStep Dataset-specific native time step.
#' @return Character - target resolution formatted steps in data.
#'
Check.TemporalAggregation <- function(
    QuerySeries,
    DateStart,
    DateStop,
    TResolution,
    BaseTResolution,
    TStep,
    BaseTStep
){
  # limit query series to what will be retained
  QuerySeries <- QuerySeries[as.POSIXct(QuerySeries, tz = "UTC") >= DateStart & as.POSIXct(QuerySeries, tz = "UTC") <= DateStop]
  ## extract format of interest
  Form <- substr(TResolution, 1, 1)
  Form <- ifelse(Form %in% c("h", "y"), toupper(Form), Form)

  ## extract desired format
  QueryTargetFormat <- format(as.POSIXct(QuerySeries, tz = "UTC"), paste0("%", Form))
  QueryTargetSteps <- unique(QueryTargetFormat)

  ## check clean division
  if((length(QueryTargetSteps) / TStep) %%1!=0){
    stop("Your specified time range does not allow for a clean integration of your selected time steps. You specified a time series of raw data with a length of ", length(QueryTargetFormat), " (", BaseTResolution, " intervals of length", BaseTStep, ") and time steps of ", TStep, ". Applying your desired temporal aggregation of ", TResolution, " intervals of length ", TStep, " works out to ", round(length(QueryTargetSteps) / TStep, 3), " intervals. Please fix this so the specified time range can be cleanly divided into aggregation intervals.")
  }

  return(QueryTargetSteps)
}
