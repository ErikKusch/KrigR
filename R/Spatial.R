### POINT LOCATIONS ============================================================
#' Transform data frame-type inputs into sf
#'
#' Transform data frame with ID for querying functionality around point-loactions to SpatialPoints
#'
#' @param USER_df A data.frame containing geo-referenced points with Lat and Lon columns
#' @importFrom sf st_as_sf
#' @return An sf POINT object.
#' @examples
#' \dontrun{
#' data("Mountains_df")
#' Make.SpatialPoints(Mountains_df)
#' }
#' @export
Make.SpatialPoints <- function(USER_df){
  USER_df <- data.frame(USER_df) ## attempt to catch tibbles or data.tables
  if(sum(c("Lat", "Lon") %in% colnames(USER_df)) != 2){stop("Please provide your geo-locations with a Lat and a Lon column (named exactly like such).")}
  sf::st_as_sf(USER_df, coords = c("Lon", "Lat"), remove = FALSE)
}
### EXTENT CHECKING ============================================================
#' Check extent specification
#'
#' Try to convert user input into a SpatExtent object. Supports inputs of classes belonging to the packages raster, terra, sf, and sp
#'
#' @param USER_ext User-supplied Extent argument in download_ERA function call
#' @importFrom methods getClass
#' @importFrom terra ext
#' @importFrom raster extent
#' @importFrom sf st_as_sf
#' @return A SpatExtent object.
#' @examples
#' \dontrun{
#' ## raster
#' Check.Ext(raster::extent(c(9.87, 15.03, 49.89, 53.06)))
#' ## terra
#' Check.Ext(terra::ext(c(9.87, 15.03, 49.89, 53.06)))
#' ## sf
#' set.seed(42)
#' nb_pt <- 10
#' dd <- data.frame(x = runif(nb_pt, 9.87, 15.03), y = runif(nb_pt, 49.89, 53.06), val = rnorm(nb_pt))
#' sf <- sf::st_as_sf(dd, coords = c("x","y"))
#' Check.Ext(sf)
#' ## sp
#' Check.Ext(as(sf, "Spatial"))
#' }
#' @export
Check.Ext <- function(USER_ext){
  ## find package where USER_ext class originates
  class_name <- class(USER_ext)
  class_def <- getClass(class_name)
  package_name <- class_def@package

  ## sanity check if USER_ext is supported
  SupportedPackages <- c("raster", "terra", "sf", "sp")
  if(!(package_name %in% SupportedPackages)){
    stop("Please specify the Extent argument as an object defined either with classes found in the raster, terra, or sf packages")
  }

  ## Transform into SpatExtent class
  if(package_name == "raster"){
    OUT_ext <- terra::ext(raster::extent(USER_ext))
  }
  if(package_name == "terra" | package_name == "sf"){
    OUT_ext <- terra::ext(USER_ext)
  }
  if(package_name == "sp"){
    USER_ext <- sf::st_as_sf(USER_ext)
    OUT_ext <- terra::ext(USER_ext)
  }

  ## Round digits and return
  OUT_ext <- round(OUT_ext, 3)
  return(OUT_ext)
}

### POINT BUFFERING ============================================================
#' Square Buffers Around Point Data
#'
#' Allow for drawing of buffer zones around point-location data for downloading and kriging of spatial data around point-locations. Overlapping individual buffers are merged.
#'
#' @param USER_pts An sf POINT object
#' @param USER_buffer Size of buffer in degrees
#' @importFrom sf st_buffer
#' @importFrom sf st_union
#' @return An sf polygon made up of individual square buffers around point-location input.
#' @examples
#' \dontrun{
#' data("Mountains_df")
#' User_pts <- Make.SpatialPoints(Mountains_df)
#' Buffer.pts(User_pts, USER_buffer = 0.5)
#' }
#' @export
Buffer.pts <- function(USER_pts, USER_buffer = .5){
  sf::st_union(sf::st_buffer(USER_pts, USER_buffer, endCapStyle = "SQUARE"))
}

### CROPPING & MASKING =========================================================
#' Range Masking with Edge Support
#'
#' Creating a mask identifying all cells in the original raster (`Base`) which are at least partially covered by the supplied shapefile (`Shape`).
#'
#' @param BASE A SpatRaster within which coverage should be identified
#' @param Shape A polygon(-collection) whose coverage of the raster object is to be found.
#' @importFrom stars st_as_stars
#' @importFrom sf st_cast
#' @return A raster layer.
#' @examples
#' \dontrun{
#' data("Jotunheimen_ras")
#' data("Jotunheimen_poly")
#' Mask.Shape(Jotunheimen_ras, Jotunheimen_poly)
#' }
#' @export
Handle.Spatial <- function(BASE, Shape){
  ret_rast <- terra::crop(BASE, Shape)
  if(class(Shape)[1] == "sf"){
    ret_rast <- terra::mask(ret_rast, Shape, touches = TRUE)
  }
  return(ret_rast)
}
