#' Preparing Covariate Data for Use in Kriging
#'
#' This function is used to setup products of covariate data ready for use in Kriging. This functionality can either be applied to user-supplied covariate data or ready-made data products such as the global dataset of soil hydraulic and thermal parameters for earth system modeling (available at \url{http://globalchange.bnu.edu.cn/research/soil4.jsp}) and the median statistic of the Global Multi-resolution Terrain Elevation Data (GMTED2010; available at \url{https://topotools.cr.usgs.gov/gmted_viewer/gmted2010_global_grids.php}). In case of the latter, the data is downloaded at 30 arc-sec latitude/longitude grid cells and subsequently resampled to match training and target resolutions specified by the user.
#'
#' @param Training A SpatRaster file containing the data which is to be downscaled. Covariate data will be resampled to match this.
#' @param Target Either numeric or a SpatRaster. If numeric, a single number representing the target resolution for the kriging step (i.e. wich resolution to downscale to). If a SpatRaster, data that the covariates and kriged products should align with. In case of a numeric input, covariate data is aggregated as closely as possible to desired resolution. If a SpatRaster, covariate data is resampled to match desired output directly.
#' @param Covariates Either character or a SpatRaster. If character, obtain frequently used and provably useful covariate data (i.e., GMTED2010 and soil data) and prepare for use in Kriging. Supported character values are "GMTED2010", "tksat", "tkdry", "csol", "k_s", "lambda", "psi", and "theta_s". If a SpatRaster, a user-supplied set of covariate data to be prepared for use in Kriging.
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
#' @importFrom terra varnames
#' @importFrom terra sources
#' @importFrom tools file_path_sans_ext
#'
#' @return A list containing two SpatRaster objects (Training and Target) ready to be used as covariates for kriging, and two files called Covariates_Target and Covariates_Train in the specified directory.
#'
#' The SpatRasters produced and stored when specifying the Covariates argument as a character string and setting the Keep_Global argument to TRUE contain metadata/attributes as a named vector that can be retrieved with terra::metags(...):
#' \itemize{
#' \item{Citation}{ - A string which to use for in-line citation of the data product.}
#' }
#'
#' @seealso \code{\link{Kriging}}.
#'
#' @examples
#' \dontrun{
#' ## Rectangular Covariate data according to input data
#' CDS_rast <- terra::rast(system.file("extdata", "CentralNorway.nc", package="KrigR"))
#' Covariates_ls <- CovariateSetup(Training = CDS_rast,
#'                                        Target = 0.01,
#'                                        Covariates = "GMTED2010",
#'                                        Keep_Global = TRUE,
#'                                        FileExtension = ".nc")
#' terra::plot(Covariates_ls[[1]])
#' terra::plot(Covariates_ls[[2]])
#'
#' ## Shapefile-limited covariate data
#' data("Jotunheimen_poly")
#' CDS_rast <- terra::rast(system.file("extdata", "CentralNorway.nc", package="KrigR"))
#' Covariates_ls <- CovariateSetup(Training = CDS_rast,
#'                                        Target = 0.01,
#'                                        Covariates = "GMTED2010",
#'                                        Extent = Jotunheimen_poly,
#'                                        Keep_Global = TRUE,
#'                                        FileExtension = ".nc")
#' terra::plot(Covariates_ls[[1]])
#' terra::plot(Covariates_ls[[2]])
#'
#' ## buffered-point-limited covariate data
#' data("Mountains_df")
#' CDS_rast <- terra::rast(system.file("extdata", "CentralNorway.nc", package="KrigR"))
#' Covariates_ls <- CovariateSetup(Training = CDS_rast,
#'                                        Target = 0.01,
#'                                        Covariates = c("tksat", "tkdry", "csol", "k_s", "lambda", "psi", "theta_s"),
#'                                        Source = "Drive",
#'                                        Extent = Mountains_df,
#'                                        Buffer = 0.2,
#'                                        Keep_Global = TRUE,
#'                                        FileExtension = ".nc")
#' terra::plot(Covariates_ls[[1]])
#' terra::plot(Covariates_ls[[2]])
#' }
#' @export
CovariateSetup <- function(Training,
                           Target,
                           Covariates = "GMTED2010",
                           Source = "Origin",
                           Extent,
                           Buffer = 0.5,
                           Dir = getwd(),
                           Keep_Global = FALSE,
                           FileExtension = ".nc"
){
  ## Catching Most Frequent Issues ===============
  if(class(Covariates) == "character"){
    if(sum(!(Covariates %in% c("GMTED2010", "tksat", "tkdry", "csol", "k_s", "lambda", "psi", "theta_s"))) > 0){
      stop("Please specify a valid covariate data set. You may supply either a character string (allowed values are: 'GMTED2010', 'tksat', 'tkdry', 'csol', 'k_s', 'lambda', 'psi' and 'theta_s') or a SpatRaster object.")
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
    Cov = rep(c("GMTED2010",
                "tksat", "tkdry", "csol", "k_s", "lambda", "psi", "theta_s"), each = 2),
    Source = rep(c("Origin", "Drive"), 8),
    UnzippedFile = rep(c("mn30_grd/w001001.adf",
                         paste0(c("tksatu", "tkdry", "csol", "k_s", "lambda", "psi_s", "theta_s"), "_l1.nc")), each = 2),
    DOI = rep(c("10.3133/ofr20111073", "10.1175/JHM-D-12-0149.1"), c(2, 14)),
    Link = c(
      "https://edcintl.cr.usgs.gov/downloads/sciweb1/shared/topo/downloads/GMTED/Grid_ZipFiles/mn30_grd.zip", # Link to GMTED2010
      "https://www.dropbox.com/scl/fi/io099a5lrxawdc6v9wy0b/GMTED2010.zip?rlkey=izqwy2uhebvh4ypgaiwkiuk6l&st=br70b3jt&dl=1", # Link to DropBox with GMTED2010
      ## tksat
      "http://globalchange.bnu.edu.cn/download/data/worldptf/tksat.zip", # Origin
      "https://www.dropbox.com/scl/fi/k8ly97961yukgsw3dyzxy/tksat.zip?rlkey=v03z7vjx122nhj156xd3bnfyj&st=piieeigq&dl=1", # DropBox
      ## tkdry
      "http://globalchange.bnu.edu.cn/download/data/worldptf/tkdry.zip", # Origin
      "https://www.dropbox.com/scl/fi/866sny1x7393a36naqfez/tkdry.zip?rlkey=pgqulsdoz2p3qznyvo5eqt7yw&st=s01p43us&dl=1", # DropBox
      ## csol
      "http://globalchange.bnu.edu.cn/download/data/worldptf/csol.zip", # Origin
      "https://www.dropbox.com/scl/fi/a8cbzy8zn38zw6ou8l580/csol.zip?rlkey=3nv321henvysppav8l3ln5ja6&st=40sjwly9&dl=1", # DropBox
      ## k_s
      "http://globalchange.bnu.edu.cn/download/data/worldptf/k_s.zip", # Origin
      "https://www.dropbox.com/scl/fi/87kak98esfe4b3srd8vwk/k_s.zip?rlkey=2ktmnl1rhakbx9xvroxb2emcz&st=goevwluk&dl=1", # DropBox
      ## lambda
      "http://globalchange.bnu.edu.cn/download/data/worldptf/lambda.zip", # Origin
      "https://www.dropbox.com/scl/fi/uqp3g3pcak96qxkvnc1uw/lambda.zip?rlkey=tchnkjxpgf4wz0xpfmna7h8we&st=njbd097a&dl=1", # DropBox
      ## psi
      "http://globalchange.bnu.edu.cn/download/data/worldptf/psi.zip", # Origin
      "https://www.dropbox.com/scl/fi/mrh4jql8c5nytli5n32um/psi.zip?rlkey=km89j9t3rum83o56d81qf8nyi&st=sqd7ua6v&dl=1", # DropBox
      ## theta_s
      "http://globalchange.bnu.edu.cn/download/data/worldptf/theta_s.zip", # Origin
      "https://www.dropbox.com/scl/fi/9a2z1k0i91awqczgpi378/theta_s.zip?rlkey=zanege1p7tevp1b1k01vb277b&st=48neb5te&dl=1" # DropBox
    )
  )

  ## Data Download (skipped if own SpatRaster supplied) ===============
  if(class(Covariates) == "character"){
    message("###### Downloading global covariate data")
    CovariatesIn <- Covariates
    ### Directory for raw files
    Dir.Covs <- file.path(Dir, "CovariateSetup")
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
        print(paste(Name, "covariate data.")) # inform user of download in console
        httr::GET(Link,
                  httr::write_disk(file.path(Dir.Covs, paste0(Name, ".zip"))),
                  httr::progress(), overwrite = TRUE)

        #### Unzipping data
        if(Name == "GMTED2010"){
          unzip(file.path(Dir.Covs, paste0(Name, ".zip")), # which file to unzip
                exdir = Dir.Covs) # where to unzip to
        }else{
          unzip(file.path(Dir.Covs, paste0(Name, ".zip")), # which file to unzip
                files = UnzippedFile, exdir = Dir.Covs) # where to unzip to
        }


        #### Loading data
        Data <- terra::rast(file.path(Dir.Covs, UnzippedFile))
        if(class(terra::values(Data)[,1]) == "integer"){
          print("Reformatting integer data into continuous numeric data. Necessary for GMTED2010 data.")
          Data <- Data+0 # +0 to avoid integer reading in faulty way, https://gis.stackexchange.com/questions/398061/reading-rasters-in-r-using-terra-package
        }
        terra::metags(Data) <- Meta_vec
        terra::varnames(Data) <- Name

        #### Saving data as single file
        if(FileExtension == ".tif"){
          terra::writeRaster(Data, filename = file.path(Dir, FName))
        }
        if(FileExtension == ".nc"){
          Data <- Meta.NC(NC = Data, FName = file.path(Dir, FName),
                          Attrs = terra::metags(Data), Write = TRUE)
        }

        Data <- Check.File(FName = FName, Dir = Dir, loadFun = terra::rast, load = TRUE, verbose = FALSE)
        if(FileExtension == ".nc"){
          Data <- Meta.NC(NC = Data, FName = file.path(Dir, FName), Attrs = Meta_vec, Read = TRUE)
        }
      }else{
        print(paste(Name, "covariate data already downloaded."))
      }
      Data
    })

    if("GMTED2010" %in% CovariatesIn & any(c("tksat", "tkdry", "csol", "k_s", "lambda", "psi", "theta_s") %in% CovariatesIn)){
      message("###### Aligning data from different sources with one another")
      MinExt <- apply(abs(do.call(rbind, lapply(Data_ls, FUN = function(x){as.vector(ext(x))}))), 2, min)
      Data_ls <- lapply(Data_ls, FUN = function(x){
        print(basename(tools::file_path_sans_ext(terra::sources(x))))
        crop(x, ext(-MinExt[1], MinExt[2], -MinExt[3], MinExt[4])) # I can simply to -/+ here because this data is global
      })
    }

    Covariates <- do.call(c, Data_ls)
    unlink(Dir.Covs, recursive = TRUE)
  }
  VarNames <- terra::varnames(Covariates)

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

  ## Spatial Aggregation/Resampling ===============
  ### Cropping and Masking
  Covariates <- Handle.Spatial(Covariates, ext(Extent))

  ### Sanity Check
  if(class(Target) == "numeric"){
    Target_res <- Target[1]
  }else{
    Target_res <- terra::res(Target)
  }
  if(Target_res < terra::res(Covariates)[1]){
    stop(paste0("You have specified resolution(s) to be finer than ", res(Covariates), " (native covariate reslution). Please provide higher-resolution data instead."))
  }
  ### Resampling
  message("###### Resampling Data")
  Cov_train <- terra::resample(Covariates, Training)
  if(class(Extent)[1] == "SpatRaster"){
    Cov_target <- terra::resample(Covariates, Extent)
  }else{
    Cov_target <- suppressWarnings(terra::aggregate(Covariates, fact = Target_res[1]/terra::res(Covariates)[1]))
  }

  ### Cropping and Masking
  # Training <- Handle.Spatial(BASE = Training, Shape = Extent)
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
  TrainRet <- terra::rast(TrainName)
  TargetRet <- terra::rast(TargetName)
  names(TrainRet) <- names(TargetRet) <- VarNames

  return(
    list(Training = TrainRet,
         Target = TargetRet)
  )
}
