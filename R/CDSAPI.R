### CDS API Credentials ========================================================
#' Register CDS API Credentials
#'
#' Just checks if provided API user and Key have already been added to keychain and adds them if necessary.
#'
#' @param API_User Character. CDS API User
#' @param API_Key Character. CDS API Key
#'
#' @importFrom ecmwfr wf_get_key
#' @importFrom ecmwfr wf_set_key
#'
#' @return No R object. An addition to the keychain if necessary.
#'
#' @seealso \code{\link{Make.Request}}, \code{\link{Execute.Requests}}.
#'
Register.Credentials <- function(API_User, API_Key){
  API_Service = "cds"
  KeyRegisterCheck <- tryCatch(ecmwfr::wf_get_key(user = API_User, service = API_Service),
                               error = function(e){e})
  if(any(class(KeyRegisterCheck) == "simpleError")){
    ecmwfr::wf_set_key(user = API_User,
                       key = as.character(API_Key),
                       service = API_Service)
  }
}
### FORMING CDS Requests =======================================================
#' Form CDS Requests
#'
#' Loops over time windows of defined size and creates a list of CDS requests.
#'
#' @param QueryTimeWindows List. List of date ranges created by \code{\link{Make.RequestWindows}}.
#' @param QueryDataSet Character. Dataset specified by user.
#' @param QueryType Character. Dataset type specified by user.
#' @param QueryVariable Character. CDS internal variable name.
#' @param QueryTimes Character. Layers of data in the raw data set
#' @param QueryExtent Character. Extent object created by Check.Ext(Extent)[c(4,1,3,2)]
#' @param QueryFormat Character. File format queried by user
#' @param Dir Directory pointer. Where to store CDS request outcomes.
#' @param verbose Logical. Whether to print/message function progress in console or not.
#' @param API_User Character. CDS API User
#' @param API_Key Character. CDS API Key
#'
#' @importFrom ecmwfr wf_request
#'
#' @return List. Each element holding either (1) a list object representing a CDS request or (2) the value NA indicating that a file of this name is already present.
#'
#' @seealso \code{\link{Make.RequestWindows}}, \code{\link{Register.Credentials}}, \code{\link{Execute.Requests}}.
#'
Make.Request <- function(QueryTimeWindows, QueryDataSet, QueryType, QueryVariable,
                         QueryTimes, QueryExtent, QueryFormat, Dir = getwd(), verbose = TRUE,
                         API_User, API_Key){
  #' Make list of CDS Requests
  Requests_ls <- lapply(1:length(QueryTimeWindows), FUN = function(requestID){
    FName <- paste("TEMP", QueryVariable, stringr::str_pad(requestID, 5, "left", "0"), sep = "_")
    if(grepl("month", QueryType)){ # monthly data needs to be specified with year, month fields
      list('dataset_short_name' = QueryDataSet,
           'product_type'   = QueryType,
           'variable'       = QueryVariable,
           'year'           = unique(as.numeric(format(as.POSIXct(QueryTimeWindows[[requestID]]), "%Y"))),
           'month'          = unique(as.numeric(format(QueryTimeWindows[[requestID]], "%m"))),
           'time'           = QueryTimes,
           'area'           = QueryExtent,
           'format'         = QueryFormat,
           'target'         = FName
           # ,
           # 'grid'           = QueryGrid
      )
    }else{
      list('dataset_short_name' = QueryDataSet,
           'product_type'   = QueryType,
           'variable'       = QueryVariable,
           'date'           = paste0(
             head(QueryTimeWindows[[requestID]], n = 1),
             "/",
             tail(QueryTimeWindows[[requestID]], n = 1)),
           'time'           = QueryTimes,
           'area'           = QueryExtent,
           'format'         = QueryFormat,
           'target'         = FName
           # ,
           # 'grid'           = QueryGrid
      )
    }

  })
  ## making list names useful for request execution updates to console
  Iterators <- paste0("[", 1:length(Requests_ls), "/", length(Requests_ls), "] ")
  FNames <- unlist(lapply(Requests_ls, "[[", "target"))
  Dates <- unlist(lapply(lapply(Requests_ls, "[[", "date"), gsub, pattern = "/", replacement = " - "))
  if(length(Dates) == 0){ # this happens for monthly data queries
    Dates <- unlist(lapply(lapply(Requests_ls, "[[", "year"), FUN = function(x){
      paste0(head(x, 1), " - ", tail(x, 1))
    }))
  }
  names(Requests_ls) <- paste0(Iterators, FNames, " (UTC: ", Dates, ")")
  ## check if files are already present
  FCheck <- sapply(FNames, Check.File, Dir = Dir, loadFun = "terra::rast", load = FALSE,
                   verbose = FALSE)
  if(length(names(unlist(FCheck))) > 0){
    Requests_ls[match(names(unlist(FCheck)), FNames)] <- NA
  }
  Requests_ls

  if(verbose){print("## Staging CDS Requests")}
  for(requestID in 1:length(Requests_ls)){ ## looping over CDS requests
    if(verbose){print(names(Requests_ls)[requestID])}
    if(class(Requests_ls[[requestID]]) == "logical"){
      # if(verbose){print("File with this name is already present.")}
      next()
    }
    API_request <- suppressMessages({
      ecmwfr::wf_request(user = API_User,
                         request = Requests_ls[[requestID]],
                         transfer = FALSE,
                         path = Dir,
                         verbose = verbose,
                         time_out = TimeOut)
    })
    Requests_ls[[requestID]]$API_request <- API_request
  }
  Requests_ls
}

### EXECUTING CDS REQUESTS  ====================================================
#' Execute CDS Requests
#'
#' Loops over list of fully formed ecmwfr requests and executes these on CDS.
#'
#' @param Requests_ls List. ecmwfr-ready CDS requests formed with \code{\link{Make.Request}}.
#' @param Dir Character. Directory where to save raw data.
#' @param API_User Character. CDS API User
#' @param API_Key Character. CDS API Key
#' @param TryDown Numeric. How often to retry a failing request/download
#' @param verbose Logical. Whether to print/message function progress in console or not.
#'
#' @importFrom ecmwfr wf_transfer
#' @importFrom httr DELETE
#' @importFrom httr authenticate
#' @importFrom httr add_headers
#'
#' @return No R object. An addition to the keychain if necessary.
#'
#' @seealso \code{\link{Register.Credentials}}, \code{\link{Make.Request}}.
#'
Execute.Requests <- function(Requests_ls, Dir, API_User, API_Key, TryDown, verbose = TRUE){
  if(verbose){print("## Listening for CDS Requests")}
  for(requestID in 1:length(Requests_ls)){ ## looping over CDS requests
    if(verbose){print(names(Requests_ls)[requestID])}
    if(class(Requests_ls[[requestID]]) == "logical"){
      if(verbose){print("File with this name is already present.")}
      next()
    }
    API_request <- Requests_ls[[requestID]]$API_request
    FileDown <- list(state = "queued")
    Down_try <- 0
    while(FileDown$state != "completed" & Down_try <= TryDown){
      ## console output that shows the status of the request on CDS
      if(verbose){
        if(FileDown$state == "queued"){
          for(rep_iter in 1:10){
            cat(rep(" ", 100))
            flush.console()
            cat('\r', "Waiting for CDS to start processing the query", rep(".", rep_iter))
            flush.console()
            Sys.sleep(0.5)
          }
        }
        if(FileDown$state == "running"){
          for(rep_iter in 1:10){
            cat(rep(" ", 100))
            flush.console()
            cat('\r', "CDS is processing the query", rep(".", rep_iter))
            flush.console()
            Sys.sleep(0.5)
          }
        }
      }
      ## download file for current request when ready
      FileDown <- tryCatch(ecmwfr::wf_transfer(url = API_request$get_url(),
                                               user = API_User,
                                               service = "cds",
                                               verbose = TRUE,
                                               path = Dir,
                                               filename = API_request$get_request()$target),
                           error = function(e){e}
      )
      if(Down_try == TryDown){
        stop("Download of CDS query result continues to fail after ", Down_try, " trys. The most recent error message is: \n", FileDown,  "Assess issues at https://cds.climate.copernicus.eu/cdsapp#!/yourrequests.")
      }
      if(any(class(FileDown) == "simpleError")){
        FileDown <- list(state = "queued")
        Down_try <- Down_try+1
      }
    }
    if(FileDown$state == "completed"){
      delete <- httr::DELETE(
        API_request$get_url(),
        httr::authenticate(API_User, API_Key),
        httr::add_headers(
          "Accept" = "application/json",
          "Content-Type" = "application/json")
      )
    }
  }
}
