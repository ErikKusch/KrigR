#' Preparing Covariate Data for Use in Kriging
#'
#' This function is used to setup products of covariate data ready for use in Kriging. This functiuonality can either be applied to user-supplied covariate data or ready-made data products such as the Harmised World Soil Data Base and the median statistic of the Global Multi-resolution Terrain Elevation Data (GMTED2010; available at \url{https://topotools.cr.usgs.gov/gmted_viewer/gmted2010_global_grids.php}). In case of the latter, the data is downloaded at 30 arc-sec latitude/longitude grid cells and subsequently resampled to match training and target resolutions specified by the user.
#'
#' @param Training A SpatRaster file containing the data which is to be downscaled. Covariate data will be resampled to match this.
#' @param Target Either numeric or a SpatRaster. If numeric, a single number representing the target resolution for the kriging step (i.e. wich resolution to downscale to). If a SpatRaster, data that the covariates and kriged products should align with. In case of a numeric input, covariate data is aggregated as closely as possible to desired resolution. If a SpatRaster, covariate data is resampled to match desired output directly.
#' @param Covariates Either character or a SpatRaster. If character, obtain frequently used and provably useful covariate data (i.e., GMTED2010 and HWSD) and prepare for use in Kriging. Supported character values are "GMTED2010" and "HWSD". Note that currently, HWSD data download is not functional. If a SpatRaster, a user-supplied set of covariate data to be prepared for use in Kriging.
#' @param Source Character. Only comes into effect when Covariates argument is specified as a character. Whether to attempt download of covariate data from the official sources (Source = "Origin") or a static copy of the data set on a private drive (Source = "Drive"). Default is "Origin".
#' @param Extent Optional, prepare covariate data according to desired spatial specification. If missing/unspecified, maximal area of supplied data and covariat sets is used. Can be specified either as a raster object, an sf object, a terra object, or a data.frame. If Extent is a raster or terra object, covariates will be prepared according to rectangular extent thereof. If Extent is an sf (MULTI-)POLYGON object, this will be treated as a shapefile and the output will be cropped and masked to this shapefile. If Extent is a data.frame of geo-referenced point records, it needs to contain Lat and Lon columns around which a buffered shapefile will be created using the Buffer argument.
#' @param Buffer Optional, Numeric. Identifies how big a circular buffer to draw around points if Extent is a data.frame of points. Buffer is expressed as centessimal degrees.
#' @param Dir Character/Directory Pointer. Directory specifying where to download data to.
#' @param Keep_Global Logical. Only comes into effect when Covariates argument is specified as a character. Whether to retain raw downloaded covariate data or not. Default is FALSE.
#' @param FileExtension Character. A file extension for the produced files. Supported values are ".nc" (default) and ".tif" (better support for metadata).
#'
#' @importFrom httr GET
#' @importFrom httr write_disk
#' @importFrom httr progress
#' @importFrom terra rast
#' @importFrom terra res
#' @importFrom terra ext
#' @importFrom terra values
#' @importFrom terra metags
#' @importFrom terra resample
#' @importFrom terra aggregate
#' @importFrom terra writeRaster
#' @importFrom terra writeCDF
#'
#' @return A list containing two SpatRaster objects (Training and Target) ready to be used as covariates for kriging, and two files called Covariates_Target and Covariates_Train in the specified directory.
#'
#' The SpatRasters produced and stored when specifying the Covariates argument as a character string and setting the Keep_Global argument to TRUE contain metadata/attributes as a named vector that can be retrieved with terra::metags(...):
#' \itemize{
#' \item{Citation}{ - A string which to use for in-line citation of the data product.}
#' }
#'
#' @examples
#' \dontrun{
#'
#' }
#' @export
KrigingCovariateSetup <- function(Training = NULL,Target = NULL,
                                  Covariates = "GMTED2010",
                                  Source = "Origin",
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
  TrainName <- file.path(Dir, paste0("Covariates_Train", FileExtension))
  TargetName <- file.path(Dir, paste0("Covariates_Target", FileExtension))
  if(FileExtension == ".tif"){
    terra::writeRaster(x = Cov_train, filename = TrainName, overwrite = TRUE)
    terra::writeRaster(x = Cov_target, filename = TargetName, overwrite = TRUE)
  }
  if(FileExtension == ".nc"){
    terra::writeCDF(x = Cov_train, filename = TrainName, overwrite = TRUE)
    terra::writeCDF(x = Cov_target, filename = TargetName, overwrite = TRUE)
  }

  ## Cleaning up files ===============
  if(!Keep_Global){ # cleanup check
    unlink(list.files(Dir, pattern = paste0(CovariatesIn, FileExtension), full.names = TRUE))
  }

  ## Return data ===============
  return(list(Training = terra::rast(TrainName),
              Target = terra::rast(TargetName)
              )
         )
}
