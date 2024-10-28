### DATE REFORMATTING ==========================================================
#' Resolve time zones as requested by user and UTC format with which to query from CDS
#'
#' Create UTC counterparts of user-input dates for CDS queries
#'
#' @param DatesVec A vector of POSIXct objects
#'
#' @return A data frame on input dates respective to user-queried timezone and their UTC counterparts.
#'
#' @examples
#' IN_DateStart <- as.POSIXct("1995-01-01 00:00", tz = "CET")
#' IN_DateStop <- as.POSIXct("2005-01-01 23:00", tz = "CET")
#' Dates_df <- Make.UTC(DatesVec = c(IN_DateStart, IN_DateStop))
#' Dates_df
#'
#' @export
Make.UTC <- function(DatesVec = NULL) {
  data.frame(IN = DatesVec,
    UTC = do.call(c, lapply(DatesVec, FUN = function(x) {
      as.POSIXct(x, tz = "UTC")
    }))
  )
}
### QUERY SEPARATING INTO TIME WINDOWS =========================================
#' Creating time windows for CDS queries
#'
#' Make a list holding date ranges for which to make individual CDS queries
#'
#' @param Dates_df A two-column data frame (column names: "IN" and "UTC") holding POSIXct elements. Created with \code{\link{Make.UTC}}.
#' @param BaseTResolution Character. Base temporal resolution of queried data on CDS
#' @param BaseTStep Numeric. Base time steps of queried data on CDS
#' @param BaseTStart POSIXct. Base starting date and time of queried data on CDS
#' @param TChunkSize Numeric. Maximum amount of layers to include in each query
#' @param DataSet Character. Name of data set. Usually a set of words separated by dashes. See possible datasets by calling \code{\link{Meta.List}}.
#'
#' @importFrom stringr str_pad
#' @importFrom stringr str_c
#'
#' @return List. Contains:
#' \itemize{
#' \item{QueryTimeWindows}{List of dates for individual CDS queries as used by \code{\link{Make.Request}}.}.
#' \item{QueryTimes}{Character. Layers of data in the raw data set}.
#' }
#'
#' @examples
#' IN_DateStart <- as.POSIXct("1995-01-01 00:00", tz = "CET")
#' IN_DateStop <- as.POSIXct("2005-01-01 23:00", tz = "CET")
#' Dates_df <- Make.UTC(DatesVec = c(IN_DateStart, IN_DateStop))
#' Make.RequestWindows(Dates_df = Dates_df,
#'                     BaseTResolution = "hour",
#'                     BaseTStep = 24
#'                     BaseTStart = as.POSIXct("1950-01-01 00:01", tz = "UTC")
#'                     TChunkSize = 12000)
#'
Make.RequestWindows <- function(Dates_df, BaseTResolution, BaseTStep, BaseTStart, TChunkSize, DataSet) {
  ## reformat input
  DateStart <- Dates_df$UTC[1]
  DateStop <- Dates_df$UTC[2]
  if (BaseTResolution == "month") {
    DateStart <- as.POSIXct(paste0(format(DateStart, "%Y-"), "01-01 00:00"), tz = "UTC") # ensure that first of first month in first queried year is used for sequence creation to avoid month skips
    DateStop <- as.POSIXct(paste0(format(DateStop, "%Y-"), "12-31 23:00"), tz = "UTC") # ensure that last of last month in last queried year is used for sequence creation to avoid month skips
  }

  ## checking chunksize specification
  if ((TChunkSize / BaseTStep) %% 1 != 0) {
    stop(
      "Please specify a TChunkSize (currently = ", TChunkSize,
      ") that is a multiple of the base temporal resolution of the data you queried from CDS (curently = ", BaseTStep, ")."
    )
  }

  ## checking alignment of queried data with raw data
  if (BaseTResolution == "hour" && BaseTStep != 24) {
    # when we are pulling from non-1-hourly records, check whether specified start-date aligns with date layers in raw data
    StartCheck <- difftime(DateStart, Meta.QuickFacts(dataset = DataSet)$TStart, units = "hour") / BaseTStep
    EndCheck <- difftime(DateStop, Meta.QuickFacts(dataset = DataSet)$TStart, units = "hour") / BaseTStep
    AlignCheck <- (as.numeric(StartCheck) %% 1 == 0 || as.numeric(EndCheck) %% 1 == 0)
  }

  ## making query time call
  if (!(BaseTResolution %in% c("hour", "month"))) {
    stop("Non-hour or -month base resolutions not supported yet")
  }
  if (BaseTResolution == "hour") {
    if (BaseTStep == 24) {
      QueryTimes <- str_pad(str_c(0:23, "00", sep = ":"), 5, "left", "0") ## this is used for telling CDS which layers we want per day
    } else {
      QueryTimes <- str_pad(str_c(
        seq(
          from = as.numeric(format(Meta.QuickFacts(dataset = DataSet)$TStart, "%H")),
          to = 23,
          by = 24 / BaseTStep
        ),
        "00",
        sep = ":"
      ), 5, "left", "0") ## this is used for telling CDS which layers we want per day, relevant for ensemble_mean and ensemble_spread for example, which are recorded at 3-hour intervals starting at 00:00 per day
    }
  }
  if (BaseTResolution == "month") {
    QueryTimes <- "00:00" ## this is used for telling CDS which layers we want per day
  }


  ## check alignment with non-1-BaseTStep data products
  if (exists("AlignCheck")) {
    if (!AlignCheck) {
      stop(
        "You have specified download of a data set whose raw layers are provided at a temporal resolution = ", BaseTResolution, " at intervalstime steps = ", BaseTStep, ".",
        "\n Either one or both of the the time-window defining dates (DateStart and DateStop arguments) you have specified, once converted to UTC (", DateStart, " and ", DateStop, ") do not align with the structure of the raw data which requires querying of data to start and terminate at any of the following UTC hours of the day: ", paste(QueryTimes, collapse = "; "), ". Please adjust your date specification accordingly."
      )
    }
  }

  ## making request ranges
  if (BaseTResolution == "month") {
    BaseTStep <- 1 # do not repeat each month, hence set this to 1
  }
  T_RequestRange <- seq(from = DateStart, to = DateStop, by = BaseTResolution)
  T_RequestDates <- as.Date(rep(unique(format(T_RequestRange, "%Y-%m-%d")), each = BaseTStep))
  list(
    QueryTimeWindows = split(T_RequestDates, ceiling(seq_along(T_RequestDates) / TChunkSize)),
    QueryTimes = QueryTimes
  )
}

### BACK-CALCULATION OF CUMULATIVE VARIABLES ===================================
#' Make cumulatively stored records into sequential ones
#'
#' Takes a SpatRaster of cumulatively stored records and returns a SpatRaster of sequential counterparts
#'
#' @param CDS_rast SpatRaster
#' @param CumulVar Logical. Whether to apply cumulative back-calculation
#' @param BaseResolution Character. Base temporal resolution of data set
#' @param BaseStep Numeric. Base time step of data set
#' @param TZone Character. Time zone for queried data.
#' @param verbose Logical. Whether to print/message function progress in console or not.
#'
#' @importFrom terra rast
#' @importFrom terra nlyr
#' @importFrom terra subset
#' @importFrom terra time
#' @importFrom lubridate days_in_month
#' @importFrom pbapply pblapply
#'
#' @return A SpatRaster
#'
Temporal.Cumul <- function(CDS_rast, CumulVar, BaseResolution, BaseStep, TZone, verbose = TRUE) { # nolint: cyclocomp_linter.
  Era5_ras <- CDS_rast
  if (verbose && CumulVar) {
    print("Disaggregation of cumulative records")
  }
  if (CumulVar && BaseResolution == "hour") {
    if (BaseStep != 1) {
      stop("Back-calculation of hourly cumulative variables only supported for 1-hour interval data. The data you have specified reports hourly data in intervals of ", BaseStep, ".")
    }
    ## removing non-needed layers
    RemovalLyr <- c(1, (nlyr(Era5_ras) - 22):nlyr(Era5_ras)) # need to remove first layer and last 23 for backcalculation
    Era5_ras <- subset(Era5_ras, RemovalLyr, negate = TRUE)
    ## back-calculation
    #' break apart sequence by UTC days and apply back-calculation per day in pblapply loop, for loop for each hour in each day
    DataDays <- ceiling(1:nlyr(Era5_ras) / 24)
    DissagDays <- unique(DataDays)
    Era5_ls <- pblapply(DissagDays, FUN = function(DissagDay_Iter) {
      counter <- 1
      Interior_ras <- Era5_ras[[which(DataDays == DissagDay_Iter)]]
      Interior_ls <- as.list(rep(NA, nlyr(Interior_ras)))
      names(Interior_ls) <- terra::time(Interior_ras)
      for (i in 1:nlyr(Interior_ras)) {
        if (counter == 1) {
          Interior_ls[[i]] <- Interior_ras[[i]]
        }
        if (counter == 24) {
          Interior_ls[[i]] <- Interior_ras[[i]] - sum(rast(Interior_ls[1:(1 + counter - 2)]))
        }
        if (counter != 24 & counter != 1) {
          Interior_ls[[i]] <- Interior_ras[[i + 1]] - Interior_ras[[i]]
        }
        counter <- counter + 1
      }
      rast(Interior_ls)
    })
    ## finishing off object
    Ret_ras <- rast(Era5_ls)
    Era5_ras <- Ret_ras
    warning("You toggled on the CumulVar option in the function call. Hourly records have been converted from cumulative aggregates to individual hourly records.")
  }
  ## multiply by number of days per month
  if (CumulVar && BaseResolution == "month") {
    Days_in_Month_vec <- days_in_month(terra::time(CDS_rast))
    if (grepl("ensemble_members", Type)) {
      Days_in_Month_vec <- rep(Days_in_Month_vec, each = 10)
    }
    Era5_ras <- Era5_ras * Days_in_Month_vec
    warning("You toggled on the CumulVar option in the function call. Monthly records have been multiplied by the amount of days per respective month.")
  }
  return(Era5_ras)
}

### TEMPORAL AGGREGATION =======================================================
#' Carry out temporal aggregation
#'
#' Takes a SpatRaster and user-specifications of temporal aggregation and carries it out
#'
#' @param CDS_rast SpatRaster
#' @param BaseResolution Character. Base temporal resolution of data set
#' @param BaseStep Numeric. Base time step of data set
#' @param TResolution Character. User-specified temporal resolution
#' @param TStep Numeric. User-specified time step
#' @param FUN User-defined aggregation function
#' @param Cores Numeric. Number of cores for parallel processing
#' @param QueryTargetSteps Character. Target resolution steps
#' @param TZone Character. Time zone for queried data.
#' @param verbose Logical. Whether to print/message function progress in console or not.
#'
#' @importFrom terra time
#' @importFrom terra tapp
#' @importFrom terra app
#'
#' @return A SpatRaster
#'
Temporal.Aggr <- function(CDS_rast, BaseResolution, BaseStep,
                          TResolution, TStep, FUN, Cores, QueryTargetSteps, TZone, verbose = TRUE) {
  if (verbose) {
    print("Temporal Aggregation")
  }
  if (BaseResolution == TResolution && BaseStep == TStep) {
    Final_rast <- CDS_rast # no temporal aggregation needed
  } else {
    TimeDiff <- sapply(terra::time(CDS_rast), FUN = function(xDate) {
      length(seq(
        from = terra::time(CDS_rast)[1],
        to = xDate,
        by = TResolution
      )) - 1
    })
    AggrIndex <- floor(TimeDiff / TStep) + 1

    Form <- substr(TResolution, 1, 1)
    Form <- ifelse(Form %in% c("h", "y"), toupper(Form), Form)
    LayerFormat <- format(terra::time(CDS_rast), paste0("%", Form))

    if (length(unique(AggrIndex)) == 1) { ## this is to avoid a warning message thrown by terra
      Final_rast <- app(
        x = CDS_rast,
        cores = Cores,
        fun = FUN
      )
    } else {
      Final_rast <- tapp(
        x = CDS_rast,
        index = AggrIndex,
        cores = Cores,
        fun = FUN
      )
    }

    if (TResolution == "year") {
      terra::time(Final_rast) <- as.POSIXct(
        paste0(LayerFormat[!duplicated(AggrIndex)], "-01-01"),
        tz = TZone
      )
    }
    if (TResolution == "month") {
      terra::time(Final_rast) <- as.POSIXct(
        paste0(format(terra::time(CDS_rast)[!duplicated(AggrIndex)], "%Y-%m"), "-01"),
        tz = TZone
      )
    }
    if (TResolution == "day") {
      terra::time(Final_rast) <- as.POSIXct(
        format(terra::time(CDS_rast)[!duplicated(AggrIndex)], "%Y-%m-%d"),
        tz = TZone
      )
    }
    if (TResolution == "hour") {
      terra::time(Final_rast) <- as.POSIXct(
        terra::time(CDS_rast)[!duplicated(AggrIndex)],
        tz = TZone
      )
    }
  }
  return(Final_rast)
}

### TEMPORAL AGGREGATION CHECK =================================================
#' Checking temporal aggregation can use all queried data
#'
#' Error message if specified aggregation and time window clash.
#'
#' @param QuerySeries Character. Vector of dates/times queried for download. Created by \code{\link{Make.RequestWindows}}.
#' @param DateStart UTC start date.
#' @param DateStop UTC stop date.
#' @param TResolution User-specified temporal resolution for aggregation.
#' @param BaseTResolution Dataset-specific native temporal resolution.
#' @param TStep User-specified time step for aggregation.
#' @param BaseTStep Dataset-specific native time step.

#' @return Character - target resolution formatted steps in data.
#'
TemporalAggregation.Check <- function(
  QuerySeries,
  DateStart,
  DateStop,
  TResolution,
  BaseTResolution,
  TStep,
  BaseTStep
) {
  ## check clean division
  if (BaseTResolution == TResolution) { ## this comes into play for hourly aggregates of ensemble data
    if ((TStep / BaseTStep) %% 1 != 0) {
      stop("Your specified time range does not allow for a clean integration of your selected time steps. You specified a time series of raw data with a length of ", length(QueryTargetFormat), " (", BaseTResolution, " intervals of length ", BaseTStep, "). Applying your desired temporal aggregation of ", TResolution, " intervals of length ", TStep, " works out to ", round(TStep / BaseTStep, 3), " intervals. Please fix this so the specified time range can be cleanly divided into aggregation intervals.")
    }
    QueryTargetSteps <- paste("Ensembling at base resolution, Factor =", TStep / BaseTStep)
    return(QueryTargetSteps)
  }

  # limit query series to what will be retained
  QuerySeries <- QuerySeries[as.POSIXct(QuerySeries, tz = "UTC") >= DateStart & as.POSIXct(QuerySeries, tz = "UTC") <= DateStop]
  ## extract format of interest
  Form <- substr(TResolution, 1, 1)
  Form <- ifelse(Form %in% c("h", "y"), toupper(Form), Form)

  ## extract desired format
  QueryTargetFormat <- format(as.POSIXct(QuerySeries, tz = "UTC"), paste0("%", Form))
  QueryTargetSteps <- unique(QueryTargetFormat)

  if ((length(QueryTargetSteps) / TStep) %% 1 != 0) {
    stop("Your specified time range does not allow for a clean integration of your selected time steps. You specified a time series of raw data with a length of ", length(QueryTargetFormat), " (", BaseTResolution, " intervals of length ", BaseTStep, "). Applying your desired temporal aggregation of ", TResolution, " intervals of length ", TStep, " works out to ", round(length(QueryTargetSteps) / TStep, 3), " intervals. Please fix this so the specified time range can be cleanly divided into aggregation intervals.")
  }
  return(QueryTargetSteps)
}
