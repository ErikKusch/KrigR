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
