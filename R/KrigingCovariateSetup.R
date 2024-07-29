#' Downloading DEM data from USGS servers
#'
#' This function downloads and rescales the median statistic of the Global Multi-resolution Terrain Elevation Data (GMTED2010) data from the servers of the U.S. Geological Survey (USGS) available at \url{https://topotools.cr.usgs.gov/gmted_viewer/gmted2010_global_grids.php}. The data is downloaded at 30 arc-sec latitude/longitude grid cells and subsequently resampled to match Train_ras and Target_res. This data is the default for kriging within this package.
#'
#' @param Train_ras A raster file containing the data which is to be downscaled. GMTED2010 data is then resampled to match this.
#' @param Target_res The target resolution for the kriging step (i.e. wich resolution to downscale to). An object as specified/produced by raster::res() or a single number (GMTED2010 data will be aggregated) or a raster which the data should be comparable to after kriging (GMTED2010 data will be resampled).
#' @param Shape Optional, a SpatialPolygonsDataFrame or data.frame object. If Shape is a SpatialPolygonsDataFrame, this will be treated as a shapefile and the output will be cropped and masked to this shapefile. If Shape is a data.frame of geo-referenced point records, it needs to contain Lat and Lon columns as well as a non-repeating ID-column.
#' @param Buffer Optional. Identifies how big a rectangular buffer to draw around points if Shape is a data frame of points. Buffer is expressed as centessimal degrees.
#' @param ID Optional. Identifies which column in Shape to use for creation of individual buffers if Shape is a data.frame.
#' @param Dir Directory specifying where to download data to.
#' @param Keep_Temporary Logical, whether to delete individual, global, 30 arc-sec files or keep them to be reused in later analyses.
#' @param Source Character. Whether to attempt download from the official USGS data viewer (Source = "USGS") or a static copy of the data set on a private drive (Source = "Drive"). Default is "USGS". Use this if the USGS viewer is unavailable.
#'
#' @importFrom httr GET
#' @importFrom httr write_disk
#' @importFrom httr progress
#' @importFrom terra rast
#'
#' @return A list containing two raster object ready to be used as covariates for kriging, and two NETCDF (.nc) files in the specified directory.
#'
#' @examples
#' \dontrun{
#'
#' }
#' @export
KrigingCovariateSetup <- function(Training = NULL,Target = NULL,
                                  Covariates = c("GMTED2010", "HWSD"),
                                  Source = c("Origin", "Drive"),
                                  Extent,
                                  Buffer = 0.5,
                                  Dir = getwd(),
                                  Keep_Global = FALSE,
                                  FileExtension = ".nc"
                                  ){
  ## Catching Most Frequent Issues ===============
  #--- Covariate Specifications
  if(class(Covariates) == "character"){
    if(sum(!(Covariates %in% c("GMTED2010", "HWSD"))) > 0){
      stop("Please specify a valid covariate data set. You may supply either the character string 'GMTED2010' or 'HWSD', or a SpatRaster object.")
    }
    if("HWSD" %in% Covariates){
      stop("HWSD download currently not supported. We are working on it.")
    }
    if(length(Source) > 1){
      stop("Please specify only one source type for the covariate data that will be downloaded. You may specify either 'Origin' or 'Drive'.")
    }
    if(!(Source %in% c("Origin", "Drive"))){
      stop("Please specify a valid source type for the covariate data that will be downloaded. You may specify either 'Origin' or 'Drive'.")
    }
  }

  ## Links for Data download ===============
  Links_df <- data.frame(
    Cov = rep(c("GMTED2010", "HWSD"), each = 2),
    Source = rep(c("Origin", "Drive")),
    UnzippedFile = rep(c("mn30_grd/w001001.adf", "NULL"), each = 2),
    DOI = rep(c("10.3133/ofr20111073", "NULL"), each = 2),
    Link = c(
      "https://edcintl.cr.usgs.gov/downloads/sciweb1/shared/topo/downloads/GMTED/Grid_ZipFiles/mn30_grd.zip", # Link to GMTED2010
      "https://www.dropbox.com/s/whkje7jc401xuwx/GMTED2010.zip?raw=1", # Link to DropBox with GMTED2010
      NULL, # Link to Harmonized World Soil Database v2.0
      NULL # Link to DropBox with GMTED2010
    )
  )

  ## Data Download (skipped if own SpatRaster supplied) ===============
  if(class(Covariates) == "character"){
    CovariatesIn <- Covariates
    ### Directory for raw files
    Dir.Covs <- file.path(Dir, "KrigingCovariateSetup")
    if(!dir.exists(Dir.Covs)){dir.create(Dir.Covs)}

    ### Data downloads
    Data_ls <- lapply(Covariates, FUN = function(Cov_iter){

      #### Figure out which link to use
      Match_vec <- sapply(1:nrow(Links_df), FUN = function(x){
        sum(Links_df$Cov[x] == Cov_iter, Links_df$Source[x] == Source)
      })

      #### Store link and name for download
      Link <- Links_df$Link[Match_vec == 2]
      Name <- Links_df$Cov[Match_vec == 2]
      UnzippedFile <- Links_df$UnzippedFile[Match_vec == 2]
      DOI <- Links_df$DOI[Match_vec == 2]
      FName <- paste0(Name, FileExtension)

      #### Metadata
      Meta_vec <- paste0(Name, " data (DOI: ", DOI, ") downloaded: ", Sys.time(), " using KrigR from ", Source)
      names(Meta_vec) <- "Citation"

      #### Check if data is already present
      Data <- Check.File(FName = FName, Dir = Dir, loadFun = terra::rast, load = TRUE, verbose = FALSE)
      if(!is.null(Data) & FileExtension == ".nc"){
        Data <- Meta.NC(NC = Data, FName = file.path(Dir, FName), Attrs = Meta_vec, Read = TRUE)
      }

      if(is.null(Data)){
        #### Downloading data
        message("Downloading ", Name, " covariate data.") # inform user of download in console
        httr::GET(Link,
                  httr::write_disk(file.path(Dir.Covs, paste0(Name, ".zip"))),
                  httr::progress(), overwrite = TRUE)

        #### Unzipping data
        unzip(file.path(Dir.Covs, paste0(Name, ".zip")), # which file to unzip
              exdir = Dir.Covs) # where to unzip to

        #### Loading data
        Data <- terra::rast(file.path(Dir.Covs, UnzippedFile))
        if(class(terra::values(Data)[,1]) == "integer"){
          print("Reformatting integer data into continuous numeric data.")
          Data <- Data+0 # +0 to avoid integer reading in faulty way, https://gis.stackexchange.com/questions/398061/reading-rasters-in-r-using-terra-package
        }
        terra::metags(Data) <- Meta_vec

        #### Saving data as single file
        if(FileExtension == ".tif"){
          terra::writeRaster(Data, filename = file.path(Dir, FName))
        }
        if(FileExtension == ".nc"){
          Data <- Meta.NC(NC = Data, FName = file.path(Dir, FName),
                              Attrs = terra::metags(Data), Write = TRUE)
        }

      }else{
        message("Raw ", Name, " covariate data already downloaded.")
      }
      Data
    })
    Covariates <- do.call(c, Data_ls)
    unlink(Dir.Covs, recursive = TRUE)
  }

  ## Spatial Limitting ===============
  ### Extent Handling
  if(missing("Extent")){ ## assign maximum extent of supplied data and covariates (only when no extent is specified)
    Extent <-terra::ext(
      ifelse(terra::ext(Training)[1] > terra::ext(Covariates)[1], terra::ext(Training)[1], terra::ext(Covariates)[1]),
      ifelse(terra::ext(Training)[2] < terra::ext(Covariates)[2], terra::ext(Training)[2], terra::ext(Covariates)[2]),
      ifelse(terra::ext(Training)[3] > terra::ext(Covariates)[3], terra::ext(Training)[3], terra::ext(Covariates)[3]),
      ifelse(terra::ext(Training)[4] < terra::ext(Covariates)[4], terra::ext(Training)[4], terra::ext(Covariates)[4])
    )
    }
  if(class(Extent)[1] == "data.frame"){
    Extent <- Buffer.pts(USER_pts = Make.SpatialPoints(USER_df = Extent),
                         USER_buffer = Buffer)
  }
  QuerySpace <- Ext.Check(Extent)
  Extent <- QuerySpace$SpatialObj # terra/sf version of input extent to be used for easy cropping and masking
  ### Cropping and Masking
  Training <- Handle.Spatial(BASE = Training, Shape = Extent)
  Covariates <- Handle.Spatial(BASE = Covariates, Shape = Extent)

  ## Spatial Aggregation/Resampling ===============
  ### Sanity Check
  if(class(Target) == "numeric"){
    Target_res <- Target[1]
  }else{
    Target_res <- terra::res(Target)
  }
  if(Target_res < terra::res(Covariates)[1]){
    stop(paste0("You have specified resolution(s) to be finer than ", res(GMTED2010_ras), " (native GMTED2010 reslution). Please download higher-resolution DEM data instead."))
  }
  ### Resampling
  Cov_train <- terra::resample(Covariates, Training)
  if(class(Extent)[1] == "SpatRaster"){
    Cov_target <- terra::resample(Covariates, Extent)
  }else{
    Cov_target <- suppressWarnings(terra::aggregate(Covariates, fact = Target_res[1]/res(Covariates)[1]))
  }
  ### Masking
  Cov_train <- Handle.Spatial(Cov_train, Extent)
  Cov_target <- Handle.Spatial(Cov_target, Extent)

  ## Data Saving & Export ===============
  if(FileExtension == ".tif"){
    terra::writeRaster(x = Cov_train, filename = file.path(Dir, "Covariates_Train.tif"), overwrite = TRUE)
    terra::writeRaster(x = Cov_target, filename = file.path(Dir, "Covariates_Target.tif"), overwrite = TRUE)
  }
  if(FileExtension == ".nc"){
    terra::writeCDF(x = Cov_train, filename = file.path(Dir, "Covariates_Train.nc"), overwrite = TRUE)
    terra::writeCDF(x = Cov_target, filename = file.path(Dir, "Covariates_Target.nc"), overwrite = TRUE)
  }

  ## Cleaning up files ===============
  if(!Keep_Global){ # cleanup check
    unlink(list.files(Dir, pattern = paste0(CovariatesIn, FileExtension), full.names = TRUE))
  }

  ## Return data ===============
  return(list(Cov_train, Cov_target))
}
