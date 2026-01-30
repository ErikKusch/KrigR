test_that("Make.Request formats dates and requests correctly for daily data and calls wf_request", {
  spy_env <- new.env()
  spy_env$calls <- list()

  local_mocked_bindings(
    wf_request = function(user, request, transfer, path, verbose) {
      spy_env$calls <- append(
        spy_env$calls,
        list(list(
          user = user,
          request = request,
          transfer = transfer,
          path = path,
          verbose = verbose
        ))
      )
      return(list(status = "mocked"))
    },
    Check.File = function(...) NULL
  )

  ChunkDates <- seq(
    as.POSIXct("2023-01-01", tz = "UTC"),
    as.POSIXct("2023-01-05", tz = "UTC"),
    by = "day"
  )
  QueryTimeWindows <- list(ChunkDates)
  TestDir <- tempdir()

  res <- Make.Request(
    QueryTimeWindows = QueryTimeWindows,
    QueryDataSet = "reanalysis-era5-single-levels",
    QueryType = "reanalysis",
    QueryVariable = "2m_temperature",
    QueryTimes = "00:00",
    QueryExtent = c(90, -180, -90, 180),
    QueryFormat = "grib",
    Dir = TestDir,
    verbose = FALSE,
    API_User = "test@user.com",
    API_Key = "123"
  )

  # Strict name checking
  expect_match(names(res)[1], "^\\[1/1\\] TEMP_2m_temperature_00001 \\(UTC: 2023-01-01 - 2023-01-05\\)$")

  req_obj <- res[[1]]
  expect_equal(req_obj$year, "2023")
  expect_equal(req_obj$month, "01")
  expect_setequal(req_obj$day, c("01", "02", "03", "04", "05"))
  expect_equal(req_obj$dataset_short_name, "reanalysis-era5-single-levels")

  expect_length(spy_env$calls, 1)
  last_call <- spy_env$calls[[1]]

  expect_equal(last_call$user, "test@user.com")
  expect_equal(last_call$path, TestDir)
  expect_false(last_call$transfer)

  req_obj_clean <- req_obj
  req_obj_clean$API_request <- NULL
  expect_equal(last_call$request, req_obj_clean)
})

test_that("Make.Request formats dates and requests correctly for monthly data", {
  spy_env <- new.env()
  spy_env$calls <- list()

  local_mocked_bindings(
    wf_request = function(user, request, transfer, path, verbose) {
      spy_env$calls <- append(spy_env$calls, list(list(request = request)))
      return(list(status = "mocked"))
    },
    Check.File = function(...) NULL
  )

  ChunkDates <- seq(
    as.POSIXct("2023-01-01", tz = "UTC"),
    as.POSIXct("2023-12-01", tz = "UTC"),
    by = "month"
  )
  QueryTimeWindows <- list(ChunkDates)

  res <- Make.Request(
    QueryTimeWindows = QueryTimeWindows,
    QueryDataSet = "reanalysis-era5-single-levels-monthly-means",
    QueryType = "monthly_averaged_reanalysis",
    QueryVariable = "2m_temperature",
    QueryTimes = "00:00",
    QueryExtent = c(90, -180, -90, 180),
    QueryFormat = "grib",
    Dir = tempdir(),
    verbose = FALSE,
    API_User = "test@user.com",
    API_Key = "123"
  )

  expect_match(names(res)[1], "UTC: 2023-01 - 2023-12")

  req_obj <- res[[1]]
  expect_equal(req_obj$year, "2023")
  expected_months <- sprintf("%02d", 1:12)
  expect_setequal(req_obj$month, expected_months)
  expect_null(req_obj$day)

  expect_length(spy_env$calls, 1)

  req_obj_clean <- req_obj
  req_obj_clean$API_request <- NULL
  expect_equal(spy_env$calls[[1]]$request, req_obj_clean)
})

test_that("Make.Request checks file existence correctly", {
  spy_env <- new.env()
  spy_env$check_calls <- list()
  spy_env$wf_calls <- list()

  local_mocked_bindings(
    wf_request = function(...) {
      spy_env$wf_calls <- append(
        spy_env$wf_calls,
        list(list(request = list(...)$request))
      )
      return(list(status = "mocked"))
    },
    Check.File = function(FName, Dir, loadFun, load, verbose) {
      spy_env$check_calls <- append(
        spy_env$check_calls,
        list(list(FName = FName, loadFun = loadFun, load = load))
      )
      if (grepl("00001", FName)) {
        return("Present. Not Loaded.")
      } else {
        return(NULL)
      }
    }
  )

  Chunk1 <- seq(
    as.POSIXct("2023-01-01", tz = "UTC"),
    as.POSIXct("2023-01-02", tz = "UTC"),
    by = "day"
  )
  Chunk2 <- seq(
    as.POSIXct("2023-02-01", tz = "UTC"),
    as.POSIXct("2023-02-02", tz = "UTC"),
    by = "day"
  )
  QueryTimeWindows <- list(Chunk1, Chunk2)

  res <- Make.Request(
    QueryTimeWindows = QueryTimeWindows,
    QueryDataSet = "test-dataset",
    QueryType = "reanalysis",
    QueryVariable = "var",
    QueryTimes = "00:00",
    QueryExtent = c(90, -180, -90, 180),
    QueryFormat = "grib",
    Dir = tempdir(),
    verbose = FALSE,
    API_User = "user",
    API_Key = "key"
  )

  expect_true(is.na(res[[1]]))
  expect_false(all(is.na(res[[2]])))
  expect_type(res[[2]], "list")

  # Verify Check.File arguments
  expect_length(spy_env$check_calls, 2)
  expect_equal(spy_env$check_calls[[1]]$loadFun, "terra::rast")
  expect_false(spy_env$check_calls[[1]]$load)

  expect_length(spy_env$wf_calls, 1)
  called_req <- spy_env$wf_calls[[1]]$request
  expect_equal(called_req$month, "02")
})

test_that("Make.Request handles single date correctly without date range", {
  local_mocked_bindings(
    wf_request = function(...) return(list(status = "mocked")),
    Check.File = function(...) NULL
  )

  ChunkDates <- seq(
    as.POSIXct("2023-06-15", tz = "UTC"),
    as.POSIXct("2023-06-15", tz = "UTC"),
    by = "day"
  )
  QueryTimeWindows <- list(ChunkDates)

  res <- Make.Request(
    QueryTimeWindows = QueryTimeWindows,
    QueryDataSet = "reanalysis-era5-single-levels",
    QueryType = "reanalysis",
    QueryVariable = "2m_temperature",
    QueryTimes = "00:00",
    QueryExtent = c(90, -180, -90, 180),
    QueryFormat = "grib",
    Dir = tempdir(),
    verbose = FALSE,
    API_User = "test@user.com",
    API_Key = "123"
  )

  expect_match(names(res)[1], "\\(UTC: 2023-06-15\\)$")
  expect_false(grepl(" - ", names(res)[1]))

  req_obj <- res[[1]]
  expect_equal(req_obj$day, "15")
})

test_that("Make.Request omits product_type when QueryType is NA", {
  local_mocked_bindings(
    wf_request = function(...) return(list(status = "mocked")),
    Check.File = function(...) NULL
  )

  ChunkDates <- seq(
    as.POSIXct("2023-01-01", tz = "UTC"),
    as.POSIXct("2023-01-02", tz = "UTC"),
    by = "day"
  )
  QueryTimeWindows <- list(ChunkDates)

  res <- Make.Request(
    QueryTimeWindows = QueryTimeWindows,
    QueryDataSet = "test-dataset",
    QueryType = NA,
    QueryVariable = "var",
    QueryTimes = "00:00",
    QueryExtent = c(90, -180, -90, 180),
    QueryFormat = "grib",
    Dir = tempdir(),
    verbose = FALSE,
    API_User = "test@user.com",
    API_Key = "123"
  )

  req_obj <- res[[1]]
  expect_null(req_obj$product_type)
  expect_false("product_type" %in% names(req_obj))
})

test_that("Make.Request uses FIterStart parameter correctly for file naming", {
  local_mocked_bindings(
    wf_request = function(...) return(list(status = "mocked")),
    Check.File = function(...) NULL
  )

  ChunkDates <- seq(
    as.POSIXct("2023-01-01", tz = "UTC"),
    as.POSIXct("2023-01-02", tz = "UTC"),
    by = "day"
  )
  QueryTimeWindows <- list(ChunkDates, ChunkDates)

  res <- Make.Request(
    QueryTimeWindows = QueryTimeWindows,
    QueryDataSet = "test-dataset",
    QueryType = "reanalysis",
    QueryVariable = "var",
    QueryTimes = "00:00",
    QueryExtent = c(90, -180, -90, 180),
    QueryFormat = "grib",
    Dir = tempdir(),
    verbose = FALSE,
    API_User = "test@user.com",
    API_Key = "123",
    FIterStart = 5
  )

  expect_match(names(res)[1], "\\[5/6\\]")
  expect_match(names(res)[2], "\\[6/6\\]")
  expect_match(names(res)[1], "TEMP_var_00005")
  expect_match(names(res)[2], "TEMP_var_00006")

  req_obj1 <- res[[1]]
  req_obj2 <- res[[2]]
  expect_equal(req_obj1$target, "TEMP_var_00005")
  expect_equal(req_obj2$target, "TEMP_var_00006")
})

test_that("Make.Request handles multiple chunks correctly", {
  spy_env <- new.env()
  spy_env$calls <- list()

  local_mocked_bindings(
    wf_request = function(...) {
      spy_env$calls <- append(
        spy_env$calls,
        list(list(request = list(...)$request))
      )
      return(list(status = "mocked"))
    },
    Check.File = function(...) NULL
  )

  Chunk1 <- seq(
    as.POSIXct("2023-01-01", tz = "UTC"),
    as.POSIXct("2023-01-05", tz = "UTC"),
    by = "day"
  )
  Chunk2 <- seq(
    as.POSIXct("2023-02-01", tz = "UTC"),
    as.POSIXct("2023-02-05", tz = "UTC"),
    by = "day"
  )
  Chunk3 <- seq(
    as.POSIXct("2023-03-01", tz = "UTC"),
    as.POSIXct("2023-03-05", tz = "UTC"),
    by = "day"
  )
  QueryTimeWindows <- list(Chunk1, Chunk2, Chunk3)

  res <- Make.Request(
    QueryTimeWindows = QueryTimeWindows,
    QueryDataSet = "test-dataset",
    QueryType = "reanalysis",
    QueryVariable = "var",
    QueryTimes = "00:00",
    QueryExtent = c(90, -180, -90, 180),
    QueryFormat = "grib",
    Dir = tempdir(),
    verbose = FALSE,
    API_User = "test@user.com",
    API_Key = "123"
  )

  expect_length(res, 3)
  expect_length(spy_env$calls, 3)

  expect_match(names(res)[1], "\\[1/3\\]")
  expect_match(names(res)[2], "\\[2/3\\]")
  expect_match(names(res)[3], "\\[3/3\\]")

  expect_match(names(res)[1], "2023-01-01 - 2023-01-05")
  expect_match(names(res)[2], "2023-02-01 - 2023-02-05")
  expect_match(names(res)[3], "2023-03-01 - 2023-03-05")

  expect_equal(spy_env$calls[[1]]$request$month, "01")
  expect_equal(spy_env$calls[[2]]$request$month, "02")
  expect_equal(spy_env$calls[[3]]$request$month, "03")
})

test_that("Make.Request handles verbose output correctly", {
  local_mocked_bindings(
    wf_request = function(...) return(list(status = "mocked")),
    Check.File = function(...) NULL
  )

  ChunkDates <- seq(
    as.POSIXct("2023-01-01", tz = "UTC"),
    as.POSIXct("2023-01-02", tz = "UTC"),
    by = "day"
  )
  QueryTimeWindows <- list(ChunkDates)

  expect_output(
    Make.Request(
      QueryTimeWindows = QueryTimeWindows,
      QueryDataSet = "test-dataset",
      QueryType = "reanalysis",
      QueryVariable = "var",
      QueryTimes = "00:00",
      QueryExtent = c(90, -180, -90, 180),
      QueryFormat = "grib",
      Dir = tempdir(),
      verbose = TRUE,
      API_User = "test@user.com",
      API_Key = "123"
    ),
    "Staging CDS Requests"
  )
})

test_that("Make.Request handles all months for within-year range", {
  local_mocked_bindings(
    wf_request = function(...) return(list(status = "mocked")),
    Check.File = function(...) NULL
  )

  ChunkDates <- seq(
    as.POSIXct("2023-01-01", tz = "UTC"),
    as.POSIXct("2023-12-01", tz = "UTC"),
    by = "month"
  )
  QueryTimeWindows <- list(ChunkDates)

  res <- Make.Request(
    QueryTimeWindows = QueryTimeWindows,
    QueryDataSet = "reanalysis-era5-single-levels-monthly-means",
    QueryType = "monthly_averaged_reanalysis",
    QueryVariable = "2m_temperature",
    QueryTimes = "00:00",
    QueryExtent = c(90, -180, -90, 180),
    QueryFormat = "grib",
    Dir = tempdir(),
    verbose = FALSE,
    API_User = "test@user.com",
    API_Key = "123"
  )

  req_obj <- res[[1]]
  expect_equal(req_obj$year, "2023")
  expect_length(req_obj$month, 12)
  expect_null(req_obj$day)
})















test_that("Make.Request preserves API_request object in result", {
  mock_api_response <- list(
    status = "completed",
    request_id = "test-request-123",
    content = "test-data"
  )

  local_mocked_bindings(
    wf_request = function(...) return(mock_api_response),
    Check.File = function(...) NULL
  )

  ChunkDates <- seq(
    as.POSIXct("2023-01-01", tz = "UTC"),
    as.POSIXct("2023-01-02", tz = "UTC"),
    by = "day"
  )
  QueryTimeWindows <- list(ChunkDates)

  res <- Make.Request(
    QueryTimeWindows = QueryTimeWindows,
    QueryDataSet = "test-dataset",
    QueryType = "reanalysis",
    QueryVariable = "var",
    QueryTimes = "00:00",
    QueryExtent = c(90, -180, -90, 180),
    QueryFormat = "grib",
    Dir = tempdir(),
    verbose = FALSE,
    API_User = "test@user.com",
    API_Key = "123"
  )

  req_obj <- res[[1]]
  expect_true("API_request" %in% names(req_obj))
  expect_equal(req_obj$API_request$status, "completed")
  expect_equal(req_obj$API_request$request_id, "test-request-123")
})
