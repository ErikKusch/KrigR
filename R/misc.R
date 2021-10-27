#' Sanity checks before Kriging commences
#'
#' This function is called upon in the krigR function and performs sanity checks for some of the most common error sources in krigin thereby attempting to return more sensible error messages to the user than what is returned by default.
#'
#' @param Data A raster object containing the data to be kriged.
#' @param CovariatesCoarse A raster object containing covariates for kriging at training resolution.
#' @param CovariatesFine A raster object containing covariates for kriging at target resolution.
#' @param KrigingEquation A formula object obtained from a character vector via as.formula() specifying the covariates to be used in kriging. The covariates used have to be present and named as layers in CovariatesCoarse and CovariatesFine.
#'
#' @return A list containing a potentially altered KrigingEquation if needed as well as an identifier for data layers which need to be skipped when kriging due to a variety of reasons.
#'
check_Krig <- function(Data, CovariatesCoarse, CovariatesFine, KrigingEquation){
  ### RESOLUTIONS ----
  if(res(CovariatesFine)[1] < res(Data)[1]/10){
    warning("It is not recommended to use kriging for statistical downscaling of more than one order of magnitude. You are currently attempting this. Kriging will proceed.")
  }
  if(all.equal(res(CovariatesCoarse)[1], res(Data)[1]) != TRUE){
    stop(paste0("The resolution of your data (", res(Data)[1], ") does not match the resolution of your covariate data (", res(CovariatesCoarse)[1], ") used for training the kriging model. Kriging can't be performed!" ))
  }
  ### EXTENTS ----
  # if(extent(Data) == extent(-180, 180, -90, 90)){
  #   stop("You are attempting to use kriging at a global extent. For reasons of computational expense and identity of relationships between covariates and variables not being homogenous across the globe, this is not recommended. Instead, try kriging of latitude bands if global kriging is really your goal.")
  # }
  if(!all.equal(extent(CovariatesCoarse), extent(Data))){
    stop("The extents of your data and training covariates don't match. Kriging can't be performed!")
  }

  ### DATA AVAILABILITY ----
  DataSkips <- NULL # data layers without enough data to be skipped in kriging
  Data_vals <- base::colSums(matrix(!is.na(values(Data)), ncol = nlayers(Data))) # a value of 0 indicates a layer only made of NAs
  if(length(which(Data_vals < 2)) > 0){
    if(length(which(Data_vals < 2)) != nlayers(Data)){
      warning(paste0("Layer(s) ", paste(which(Data_vals == 0), collapse=", "), " of your data do(es) not contain enough data. Kriging will result in a raster identical do the input for this layer."))
      DataSkips <- which(Data_vals < 2)
    }else{
      stop("Your Data does not contain enough values. Kriging can't be performed!")
    }
  }
  CovCo_vals <- base::colSums(matrix(!is.na(values(CovariatesCoarse)), ncol = nlayers(CovariatesCoarse))) # a value of 0 indicates a layer only made of NAs
  if(length(which(CovCo_vals < 2)) > 0){
    if(length(which(CovCo_vals < 2)) != nlayers(CovariatesCoarse)){
      warning(paste0("Layer(s) ", paste(which(CovCo_vals < 2), collapse=", "), " of your covariates at training resolution do(es) not contain enough data. This/these layer(s) is/are dropped. The Kriging equation might get altered."))
      CovariatesCoarse <- CovariatesCoarse[[-which(CovCo_vals < 2)]]
    }else{
      stop("Your covariate data at training resolution does not contain enough values. Kriging can't be performed!")
    }
  }
  CovFin_vals <- base::colSums(matrix(!is.na(values(CovariatesFine)), ncol = nlayers(CovariatesFine))) # a value of 0 indicates a layer only made of NAs
  if(length(which(CovFin_vals < 2)) > 0){
    if(length(which(CovFin_vals < 2)) != nlayers(CovariatesFine)){
      warning(paste0("Layer(s) ", paste(which(CovFin_vals == 0), collapse=", "), " of your covariates at target resolution do(es) not contain enough data. This/these layer(s) is/are dropped."))
      CovariatesFine <- CovariatesFine[[-which(CovFin_vals < 2)]]
    }else{
      stop("Your covariate data at target resolution does not contain enough values. Kriging can't be performed!")
    }
  }
  ### EQUATION ----
  Terms <- unlist(strsplit(labels(terms(KrigingEquation)), split = ":")) # identify parameters called to in formula
  Terms_Required <- unique(Terms) # isolate double-references (e.g. due to ":" indexing for interactions)
  Terms_Present <- Reduce(intersect, list(Terms_Required, names(CovariatesCoarse), names(CovariatesFine))) # identify the terms that are available and required
  if(sum(Terms_Required %in% Terms_Present) != length(Terms_Required)){
    if(length(Terms_Present) == 0){ # if none of the specified terms were found
      KrigingEquation <- paste0("Data ~ ", paste(names(CovariatesCoarse), collapse = "+"))
      warn <- paste("None of the terms specified in your KrigingEquation are present in the covariate data sets. The KrigingEquation has been altered to include all available terms in a linear model:", KrigingEquation)
    }else{ # at least some of the specified terms were found
      KrigingEquation <- paste0("Data ~ ", paste(Terms_Present, collapse = "+"))
      warn <- paste("Not all of the terms specified in your KrigingEquation are present in the covariate data sets. The KrigingEquation has been altered to include all available and specified terms in a linear model:", KrigingEquation)
    }
    Cotinue <- menu(c("Yes", "No"), title=paste(warn, "Do you wish to continue using the new formula?"))
    if(Cotinue == 2){ # break operation if user doesn't want this
      stop("Kriging terminated by user due to formula issues.")
    }
  }
  ### NA DATA IN LAYERS ----
  # CovariatesFine <- CovariatesFine[[which(names(CovariatesFine) %in% Terms_Present)]] # only look at layers that the krigignequation targets
  # if(nlayers(CovariatesFine) > 1){
  #   MaskedPix <- length(which(values(sum(CovariatesFine, na.rm = TRUE)) != 0)) # number of non-masked pixels in which data is present in at least one layer
  #   MissingPix <- length(which(!is.na(values(sum(CovariatesFine, na.rm = FALSE))))) # number of pixels in which all layers have data
  #   if(MissingPix < MaskedPix){ # when there are any pixels for which data is absent for at least one layer
  #     stop("One or more more of your target covariate layers is missing data in locations where data is present for other layers. Please either fill these pixels with data or omit terms targeting these layers from your Kriging equation.")
  #   }
  # }
  return(list(as.formula(KrigingEquation), DataSkips))
}

#' Summary of Raster file characteristics
#'
#' This function is called upon in the krigR function and summarizes Raster characteristics without carrying along the raster file itself. This is used to create lists tracking calls to the function krigR without bloating them too much.
#'
#' @param Object_ras A raster object.
#'
#' @return A list containing information about the input raster.
#'
SummarizeRaster <- function(Object_ras = NULL){
  Summary_ls <- list(Class = class(Object_ras),
                     Dimensions = list(nrow = nrow(Object_ras),
                                       ncol = ncol(Object_ras),
                                       ncell = ncell(Object_ras)),
                     Extent = Object_ras@extent,
                     CRS = crs(Object_ras),
                     layers = names(Object_ras))
  return(Summary_ls)
}

#' Square Buffers Around Point Data
#'
#' Allow for drawing of buffer zones around point-location data for downloading and kriging of spatial data around point-locations. Overlapping individual buffers are merged.
#'
#' @param Points A data.frame containing geo-referenced points with Lat and Lon columns
#' @param Buffer Identifies how big a rectangular buffer to draw around points. Expressed as centessimal degrees.
#' @param ID Identifies which column in to use for creation of individual buffers.
#'
#' @return A shape made up of individual square buffers around point-location input.
#'
buffer_Points <- function(Points = NULL, Buffer = .5, ID = "ID"){
  # set the radius for the plots
  radius <- Buffer # radius in meters
  # define the plot edges based upon the plot radius.
  yPlus <- Points$Lat+radius
  xPlus <- Points$Lon+radius
  yMinus <- Points$Lat-radius
  xMinus <- Points$Lon-radius
  # calculate polygon coordinates for each plot centroid.
  square=cbind(xMinus,yPlus,  # NW corner
               xPlus, yPlus,  # NE corner
               xPlus,yMinus,  # SE corner
               xMinus,yMinus, # SW corner
               xMinus,yPlus)  # NW corner again - close ploygon
  # Extract the plot ID information
  ID = Points[,ID]
  # create spatial polygons from coordinates
  polys <- SpatialPolygons(mapply(function(poly, id)
  {
    xy <- matrix(poly, ncol=2, byrow=TRUE)
    Polygons(list(Polygon(xy)), ID=id)
  },
  split(square, row(square)), ID),
  proj4string = CRS(as.character("+proj=utm +zone=11 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))
  )
}



#' Range Masking with Edge Support
#'
#' Creating a raster mask identifying all cells in the original raster (`base.map`) which are at least partially covered by the supplied shapefile (`Shape`).
#'
#' @param base.map A raster within which coverage should be identified
#' @param Shape A polygon(-collection) whose coverage of the raster object is to be found.
#'
#' @return A raster layer.
#'
mask_Shape <- function(base.map = NULL, Shape = NULL){
  base.map[] <- NA
  stars.base.map <- stars::st_as_stars(base.map)
  # Subset shape file
  select.ranges <- sf::st_as_sf(Shape)
  # Cast polygon as lines instead
  select.ranges.lines <- sf::st_cast(select.ranges, "MULTILINESTRING")
  select.ranges.lines$STARS <- 1
  # Get centroids (FAST!)
  range <- fasterize(select.ranges, base.map, fun = "first", background = 0)
  # Get edges (slower than fasterize but faster than rasterize)
  range.edges <- stars::st_rasterize(select.ranges.lines, stars.base.map, options = "ALL_TOUCHED=TRUE")
  if(class(as.vector(range.edges[[1]])) == "stars"| class(as.vector(range.edges[[1]])) == "numeric"){
    range.edges <- as.vector(range.edges[[1]])
    range.edges <- ifelse(is.na(range.edges), 0, 1)
    # Merge
    range[] <- ifelse(range[] + range.edges, 1, 0)
  }
  range[range==0] <- NA # set all cells which the shape doesn't touch to NA
  return(range)
}

#' Sanity checks before Kriging commences
#'
#' This function is called upon in the krigR function and performs sanity checks for some of the most common error sources in krigin thereby attempting to return more sensible error messages to the user than what is returned by default.
#'
#' @param Data A raster object containing the data to be kriged.
#' @param CovariatesCoarse A raster object containing covariates for kriging at training resolution.
#' @param CovariatesFine A raster object containing covariates for kriging at target resolution.
#' @param KrigingEquation A formula object obtained from a character vector via as.formula() specifying the covariates to be used in kriging. The covariates used have to be present and named as layers in CovariatesCoarse and CovariatesFine.
#'
#' @return A list containing a potentially altered KrigingEquation if needed as well as an identifier for data layers which need to be skipped when kriging due to a variety of reasons.
#'
#'@export
check_Krig <- function(Data, CovariatesCoarse, CovariatesFine, KrigingEquation){
  ### RESOLUTIONS ----
  if(res(CovariatesFine)[1] < res(Data)[1]/10){
    warning("It is not recommended to use kriging for statistical downscaling of more than one order of magnitude. You are currently attempting this. Kriging will proceed.")
  }
  if(all.equal(res(CovariatesCoarse)[1], res(Data)[1]) != TRUE){
    stop(paste0("The resolution of your data (", res(Data)[1], ") does not match the resolution of your covariate data (", res(CovariatesCoarse)[1], ") used for training the kriging model. Kriging can't be performed!" ))
  }
  ### EXTENTS ----
  # if(extent(Data) == extent(-180, 180, -90, 90)){
  #   stop("You are attempting to use kriging at a global extent. For reasons of computational expense and identity of relationships between covariates and variables not being homogenous across the globe, this is not recommended. Instead, try kriging of latitude bands if global kriging is really your goal.")
  # }
  if(!all.equal(extent(CovariatesCoarse), extent(Data))){
    stop("The extents of your data and training covariates don't match. Kriging can't be performed!")
  }

  ### DATA AVAILABILITY ----
  DataSkips <- NULL # data layers without enough data to be skipped in kriging
  Data_vals <- base::colSums(matrix(!is.na(values(Data)), ncol = nlayers(Data))) # a value of 0 indicates a layer only made of NAs
  if(length(which(Data_vals < 2)) > 0){
    if(length(which(Data_vals < 2)) != nlayers(Data)){
      warning(paste0("Layer(s) ", paste(which(Data_vals == 0), collapse=", "), " of your data do(es) not contain enough data. Kriging will result in a raster identical do the input for this layer."))
      DataSkips <- which(Data_vals < 2)
    }else{
      stop("Your Data does not contain enough values. Kriging can't be performed!")
    }
  }
  CovCo_vals <- base::colSums(matrix(!is.na(values(CovariatesCoarse)), ncol = nlayers(CovariatesCoarse))) # a value of 0 indicates a layer only made of NAs
  if(length(which(CovCo_vals < 2)) > 0){
    if(length(which(CovCo_vals < 2)) != nlayers(CovariatesCoarse)){
      warning(paste0("Layer(s) ", paste(which(CovCo_vals < 2), collapse=", "), " of your covariates at training resolution do(es) not contain enough data. This/these layer(s) is/are dropped. The Kriging equation might get altered."))
      CovariatesCoarse <- CovariatesCoarse[[-which(CovCo_vals < 2)]]
    }else{
      stop("Your covariate data at training resolution does not contain enough values. Kriging can't be performed!")
    }
  }
  CovFin_vals <- base::colSums(matrix(!is.na(values(CovariatesFine)), ncol = nlayers(CovariatesFine))) # a value of 0 indicates a layer only made of NAs
  if(length(which(CovFin_vals < 2)) > 0){
    if(length(which(CovFin_vals < 2)) != nlayers(CovariatesFine)){
      warning(paste0("Layer(s) ", paste(which(CovFin_vals == 0), collapse=", "), " of your covariates at target resolution do(es) not contain enough data. This/these layer(s) is/are dropped."))
      CovariatesFine <- CovariatesFine[[-which(CovFin_vals < 2)]]
    }else{
      stop("Your covariate data at target resolution does not contain enough values. Kriging can't be performed!")
    }
  }
  ### EQUATION ----
  Terms <- unlist(strsplit(labels(terms(KrigingEquation)), split = ":")) # identify parameters called to in formula
  Terms_Required <- unique(Terms) # isolate double-references (e.g. due to ":" indexing for interactions)
  Terms_Present <- Reduce(intersect, list(Terms_Required, names(CovariatesCoarse), names(CovariatesFine))) # identify the terms that are available and required
  if(sum(Terms_Required %in% Terms_Present) != length(Terms_Required)){
    if(length(Terms_Present) == 0){ # if none of the specified terms were found
      KrigingEquation <- paste0("Data ~ ", paste(names(CovariatesCoarse), collapse = "+"))
      warn <- paste("None of the terms specified in your KrigingEquation are present in the covariate data sets. The KrigingEquation has been altered to include all available terms in a linear model:", KrigingEquation)
    }else{ # at least some of the specified terms were found
      KrigingEquation <- paste0("Data ~ ", paste(Terms_Present, collapse = "+"))
      warn <- paste("Not all of the terms specified in your KrigingEquation are present in the covariate data sets. The KrigingEquation has been altered to include all available and specified terms in a linear model:", KrigingEquation)
    }
    Cotinue <- menu(c("Yes", "No"), title=paste(warn, "Do you wish to continue using the new formula?"))
    if(Cotinue == 2){ # break operation if user doesn't want this
      stop("Kriging terminated by user due to formula issues.")
    }
  }
  ### NA DATA IN LAYERS ----
  # CovariatesFine <- CovariatesFine[[which(names(CovariatesFine) %in% Terms_Present)]] # only look at layers that the krigignequation targets
  # if(nlayers(CovariatesFine) > 1){
  #   MaskedPix <- length(which(values(sum(CovariatesFine, na.rm = TRUE)) != 0)) # number of non-masked pixels in which data is present in at least one layer
  #   MissingPix <- length(which(!is.na(values(sum(CovariatesFine, na.rm = FALSE))))) # number of pixels in which all layers have data
  #   if(MissingPix < MaskedPix){ # when there are any pixels for which data is absent for at least one layer
  #     stop("One or more more of your target covariate layers is missing data in locations where data is present for other layers. Please either fill these pixels with data or omit terms targeting these layers from your Kriging equation.")
  #   }
  # }
  return(list(as.formula(KrigingEquation), DataSkips))
}

#' Summary of Raster file characteristics
#'
#' This function is called upon in the krigR function and summarizes Raster characteristics without carrying along the raster file itself. This is used to create lists tracking calls to the function krigR without bloating them too much.
#'
#' @param Object_ras A raster object.
#'
#' @return A list containing information about the input raster.
#'
#'@export
SummarizeRaster <- function(Object_ras = NULL){
  Summary_ls <- list(Class = class(Object_ras),
                     Dimensions = list(nrow = nrow(Object_ras),
                                       ncol = ncol(Object_ras),
                                       ncell = ncell(Object_ras)),
                     Extent = Object_ras@extent,
                     CRS = crs(Object_ras),
                     layers = names(Object_ras))
  return(Summary_ls)
}

#' Square Buffers Around Point Data
#'
#' Allow for drawing of buffer zones around point-location data for downloading and kriging of spatial data around point-locations. Overlapping individual buffers are merged.
#'
#' @param Points A data.frame containing geo-referenced points with Lat and Lon columns
#' @param Buffer Identifies how big a rectangular buffer to draw around points. Expressed as centessimal degrees.
#' @param ID Identifies which column in to use for creation of individual buffers.
#'
#' @return A shape made up of individual square buffers around point-location input.
#'
#' @export
buffer_Points <- function(Points = NULL, Buffer = .5, ID = "ID"){
  # set the radius for the plots
  radius <- Buffer # radius in meters
  # define the plot edges based upon the plot radius.
  yPlus <- Points$Lat+radius
  xPlus <- Points$Lon+radius
  yMinus <- Points$Lat-radius
  xMinus <- Points$Lon-radius
  # calculate polygon coordinates for each plot centroid.
  square=cbind(xMinus,yPlus,  # NW corner
               xPlus, yPlus,  # NE corner
               xPlus,yMinus,  # SE corner
               xMinus,yMinus, # SW corner
               xMinus,yPlus)  # NW corner again - close ploygon
  # Extract the plot ID information
  if(any(is.na(Points[,ID]))){stop("Your ID column in your point-location data frame contains NA values. We cannot process these.")}
  ID = Points[,ID]
  # create spatial polygons from coordinates
  polys <- SpatialPolygons(mapply(function(poly, id)
  {
    xy <- matrix(poly, ncol=2, byrow=TRUE)
    Polygons(list(Polygon(xy)), ID=id)
  },
  split(square, row(square)), ID),
  proj4string = CRS(as.character("+proj=utm +zone=11 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))
  )
}



#' Range Masking with Edge Support
#'
#' Creating a raster mask identifying all cells in the original raster (`base.map`) which are at least partially covered by the supplied shapefile (`Shape`).
#'
#' @param base.map A raster within which coverage should be identified
#' @param Shape A polygon(-collection) whose coverage of the raster object is to be found.
#'
#' @return A raster layer.
#'
#' @export
mask_Shape <- function(base.map = NULL, Shape = NULL){
  base.map[] <- NA
  stars.base.map <- stars::st_as_stars(base.map)
  # Subset shape file
  select.ranges <- sf::st_as_sf(Shape)
  # Cast polygon as lines instead
  select.ranges.lines <- sf::st_cast(select.ranges, "MULTILINESTRING")
  select.ranges.lines$STARS <- 1
  # Get centroids (FAST!)
  range <- fasterize(select.ranges, base.map, fun = "first", background = 0)
  # Get edges (slower than fasterize but faster than rasterize)
  range.edges <- stars::st_rasterize(select.ranges.lines, stars.base.map, options = "ALL_TOUCHED=TRUE")
  if(class(as.vector(range.edges[[1]])) == "stars"| class(as.vector(range.edges[[1]])) == "numeric"){
    range.edges <- as.vector(range.edges[[1]])
    range.edges <- ifelse(is.na(range.edges), 0, 1)
    # Merge
    range[] <- ifelse(range[] + range.edges, 1, 0)
  }
  range[range==0] <- NA # set all cells which the shape doesn't touch to NA
  return(range)
}
