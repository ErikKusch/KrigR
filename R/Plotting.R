### Raw Data ==========================================================
#' Visualise raster data and overlay sf polygons if desired.
#'
#' Use the ggplot2 plotting engine to easily create visualisations of raster data - like the ones obtained using CDownloadS(...) - and overlay sf polygon data if desired.
#'
#' @param SpatRast SpatRast object to visualise.
#' @param SF Optional. SF object which to overlay.
#' @param Size Optional. Size of SF overlay.
#' @param Shape Optional. Shape of SF overlay if points.
#' @param Dates Optional. Character vector of labels to apply to each layer of the SpatRast. By default, the content of the terra::time() field of the supplied SpatRast object.
#' @param Legend Colour label legend.
#' @param COL Colour palette.
#' @param ncol Number of columns for panel arrangement of plots
#'
#' @importFrom viridis inferno
#' @importFrom terra time
#' @importFrom tidyr gather
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 geom_raster
#' @importFrom ggplot2 theme_bw
#' @importFrom ggplot2 facet_wrap
#' @importFrom ggplot2 labs
#' @importFrom ggplot2 scale_fill_gradientn
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 unit
#' @importFrom ggplot2 geom_sf
#'
#' @return A ggplot2 object visualising a raster.
#'
#' @seealso \code{\link{CDownloadS}}.
#'
#' @examples
#' SpatRast <- terra::rast(system.file("extdata", "CentralNorway.nc", package = "KrigR"))[[1:2]]
#' data("Jotunheimen_poly")
#' SF <- Jotunheimen_poly
#' Plot.SpatRast(SpatRast = SpatRast, SF = SF)
#'
#' @export
Plot.SpatRast <- function(SpatRast, SF, Dates, Legend = "Air Temperature [K]", COL = viridis::inferno(100), Size = 1, Shape = 1, ncol = 1) {
  if (missing(Dates)) {
    Dates <- as.character(terra::time(SpatRast))
  }
  Raw_df <- as.data.frame(SpatRast, xy = TRUE) # turn raster into dataframe
  colnames(Raw_df)[c(-1, -2)] <- Dates # set colnames
  Raw_df <- tidyr::gather(data = Raw_df, key = Values, value = "value", colnames(Raw_df)[c(-1, -2)]) #  make ggplot-ready
  Raw_plot <- ggplot() + # create plot
    geom_raster(data = Raw_df, aes(x = x, y = y, fill = value)) + # plot the covariate data
    theme_bw() +
    facet_wrap(~ factor(Values, levels = Dates), ncol = ncol) +
    labs(x = "Longitude", y = "Latitude") + # make plot more readable
    scale_fill_gradientn(name = Legend, colours = COL, na.value = "transparent") + # add colour and legend
    theme(plot.margin = unit(c(0, 0, 0, 0), "cm")) + # reduce margins (for fusing of plots)
    theme(legend.key.size = unit(1.5, "cm"))
  if (!missing(SF)) { # if a shape has been designated
    Raw_plot <- Raw_plot + geom_sf(data = SF, colour = "black", fill = "NA", size = Size, shape = Shape) # add shape
  }
  return(Raw_plot)
} # export the plot

### Covariate Data ==========================================================
#' Visualise covariate raster data and overlay sf polygons if desired.
#'
#' Use the ggplot2 plotting engine to easily create visualisations of covariate raster data - like the ones obtained using CovariateSetup(...) - and overlay sf polygon data if desired.
#'
#' @param Covariates List of length 2. Containing training resolution covariate SpatRast in slot 1 and target resolution covariate SpatRast in slot 2.
#' @param SF Optional. SF object which to overlay.
#' @param Size Optional. Size of SF overlay.
#' @param Shape Optional. Shape of SF overlay if points.
#' @param COL Colour palette.
#'
#' @importFrom viridis cividis
#' @importFrom terra nlyr
#' @importFrom tidyr gather
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 geom_raster
#' @importFrom ggplot2 theme_bw
#' @importFrom ggplot2 facet_wrap
#' @importFrom ggplot2 labs
#' @importFrom ggplot2 scale_fill_gradientn
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 unit
#' @importFrom ggplot2 geom_sf
#' @importFrom cowplot plot_grid
#'
#' @return A ggplot2 object visualising a raster.
#'
#' @seealso \code{\link{CovariateSetup}}.
#'
#' @examples
#' Cov_train <- terra::rast(system.file("extdata", "Covariates_Train.nc", package = "KrigR"))
#' Cov_target <- terra::rast(system.file("extdata", "Covariates_Target.nc", package = "KrigR"))
#' names(Cov_train) <- names(Cov_target) <- "GMTED2010 [m]"
#' Covariates <- list(Training = Cov_train, Target = Cov_target)
#' data("Jotunheimen_poly")
#' SF <- Jotunheimen_poly
#' Plot.Covariates(Covariates = Covariates, SF = SF)
#'
#' @export
Plot.Covariates <- function(Covariates, SF, COL = viridis::cividis(100), Size = 1, Shape = 1) {
  Plots_ls <- as.list(rep(NA, nlyr(Covariates[[1]]))) # create as many plots as there are covariates variables
  for (Variable in 1:nlyr(Covariates[[1]])) { # loop over all covariate variables
    Covariates_Iter <- list(Covariates[[1]][[Variable]], Covariates[[2]][[Variable]]) # extract the data for this variable
    Covariates_Iter <- lapply(Covariates_Iter, FUN = function(x) {
      Cov_df <- as.data.frame(x, xy = TRUE) # turn raster into dataframe
      gather(data = Cov_df, key = Values, value = "value", colnames(Cov_df)[c(-1, -2)]) #  make ggplot-ready
    })
    Covariates_Iter[[1]][, 3] <- "Native"
    Covariates_Iter[[2]][, 3] <- "Target"
    Cov_df <- do.call(rbind, Covariates_Iter)
    Plots_ls[[Variable]] <- ggplot() + # create plot
      geom_raster(data = Cov_df, aes(x = x, y = y, fill = value)) + # plot the covariate data
      theme_bw() +
      facet_wrap(~Values) +
      labs(x = "Longitude", y = "Latitude") + # make plot more readable
      scale_fill_gradientn(name = names(Covariates[[1]][[Variable]]), colours = COL, na.value = "transparent") + # add colour and legend
      theme(plot.margin = unit(c(0, 0, 0, 0), "cm")) + # reduce margins (for fusing of plots)
      theme(legend.key.size = unit(1.5, "cm"))
    if (!missing(SF)) { # if a shape has been designated
      Plots_ls[[Variable]] <- Plots_ls[[Variable]] + geom_sf(data = SF, colour = "black", fill = "NA", size = Size, shape = Shape) # add shape
    }
    # } # end of resolution loop
  } # end of variable loop
  if (nlyr(Covariates[[1]]) > 1) {
    ggPlot <- plot_grid(plotlist = Plots_ls, ncol = 1, labels = "AUTO") # fuse the plots into one big plot
    return(ggPlot)
  } else {
    return(Plots_ls[[1]])
  }
} # export the plot

### Kriged Data ==========================================================
#' Visualise kriged raster data and associated uncertainties and overlay sf polygons if desired.
#'
#' Use the ggplot2 plotting engine to easily create visualisations of kriged raster data and associated uncertainties raster data - like the ones obtained using Kriging(...) - and overlay sf polygon data if desired.
#'
#' @param Krigs List of length 2. Containing kriging prediction SpatRast in slot 1 and kriging standard error SpatRast in slot 2.
#' @param SF Optional. SF object which to overlay.
#' @param Size Optional. Size of SF overlay.
#' @param Shape Optional. Shape of SF overlay if points.
#' @param Dates Optional. Character vector of labels to apply to each layer of the SpatRast. By default, the content of the terra::time() field of the supplied SpatRast objects in list.
#' @param Legend Colour label legend.
#'
#' @importFrom viridis inferno
#' @importFrom viridis viridis
#' @importFrom terra time
#' @importFrom tidyr gather
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 geom_raster
#' @importFrom ggplot2 theme_bw
#' @importFrom ggplot2 facet_wrap
#' @importFrom ggplot2 labs
#' @importFrom ggplot2 scale_fill_gradientn
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 unit
#' @importFrom ggplot2 geom_sf
#' @importFrom cowplot plot_grid
#'
#' @return A ggplot2 object visualising a raster.
#'
#' @seealso \code{\link{Kriging}}.
#'
#' @examples
#' CDS_rast <- terra::rast(system.file("extdata", "CentralNorway.nc", package = "KrigR"))
#' Cov_train <- terra::rast(system.file("extdata", "Covariates_Train.nc", package = "KrigR"))
#' Cov_target <- terra::rast(system.file("extdata", "Covariates_Target.nc", package = "KrigR"))
#' names(Cov_train) <- names(Cov_target) <- "GMTED2010 [m]"
#'
#' ### kriging itself
#' ExtentKrig <- Kriging(
#'   Data = CDS_rast[[1:2]],
#'   Covariates_training = Cov_train,
#'   Covariates_target = Cov_target,
#'   Equation = "GMTED2010",
#'   Cores = 2,
#'   FileName = "KrigTest1",
#'   FileExtension = ".nc",
#'   Keep_Temporary = TRUE,
#'   nmax = 40,
#'   verbose = TRUE
#' )
#'
#' Plot.Kriged(Krigs = ExtentKrig)
#'
#' @export
Plot.Kriged <- function(Krigs, SF, Dates, Legend = "Air Temperature [K]", Size = 1, Shape = 1) {
  if (missing(Dates)) {
    Dates <- as.character(terra::time(Krigs[[1]]))
  }
  Type_vec <- c("Prediction", "Standard Error") # these are the output types of krigR
  Colours_ls <- list(inferno(100), rev(viridis(100))) # we want separate colours for the types
  Plots_ls <- as.list(NA, NA) # this list will be filled with the output plots
  for (Plot in 1:2) { # loop over both output types
    Krig_df <- as.data.frame(Krigs[[Plot]], xy = TRUE) # turn raster into dataframe
    colnames(Krig_df)[c(-1, -2)] <- paste(Type_vec[Plot], Dates) # set colnames
    Krig_df <- gather(data = Krig_df, key = Values, value = "value", colnames(Krig_df)[c(-1, -2)]) # make ggplot-ready
    Plots_ls[[Plot]] <- ggplot() + # create plot
      geom_raster(data = Krig_df, aes(x = x, y = y, fill = value)) + # plot the kriged data
      facet_wrap(~Values) + # split raster layers up
      theme_bw() +
      labs(x = "Longitude", y = "Latitude") + # make plot more readable
      scale_fill_gradientn(name = Legend, colours = Colours_ls[[Plot]], na.value = "transparent") + # add colour and legend
      theme(plot.margin = unit(c(0, 0, 0, 0), "cm")) + # reduce margins (for fusing of plots)
      theme(legend.key.size = unit(1, "cm"))
    if (!missing(SF)) { # if a shape has been designated
      Plots_ls[[Plot]] <- Plots_ls[[Plot]] + geom_sf(data = SF, colour = "black", fill = "NA", size = Size, shape = Shape) # add shape
    }
  } # end of type-loop
  ggPlot <- plot_grid(plotlist = Plots_ls, ncol = 1, labels = "AUTO") # fuse the plots into one big plot
  return(ggPlot)
} # export the plot

### Bioclimatic Data ==========================================================
#' Visualise bioclimatic raster data and overlay sf polygons if desired.
#'
#' Use the ggplot2 plotting engine to easily create visualisations of biolcimatic raster data - like the ones obtained using BioClim(...) - and overlay sf polygon data if desired.
#'
#' @param BioClims SpatRast object to visualise.
#' @param Which Numeric. Which bioclimatic variable(s) to visualise.
#' @param SF Optional. SF object which to overlay.
#' @param Size Optional. Size of SF overlay.
#' @param Shape Optional. Shape of SF overlay if points.
#' @param Water_Var Optional, character. Name of water availability variable in the bioclimatic variables.
#' @param ncol Number of columns for panel arrangement of plots
#'
#' @importFrom terra nlyr
#' @importFrom viridis inferno
#' @importFrom viridis mako
#' @importFrom cowplot plot_grid
#'
#' @return A ggplot2 object visualising a raster.
#'
#' @seealso \code{\link{BioClim}}.
#'
#' @examples
#' BC_rast <- terra::rast(system.file("extdata", "CN_BC.nc", package = "KrigR"))
#' Plot.BioClim(BioClims = BC_rast, Water_Var = "Soil Moisture (0-7cm)")
#'
#' @export
Plot.BioClim <- function(BioClims, Which = 1:19, SF, Water_Var = "Water Availability", ncol = 3, Size = 1, Shape = 1) {
  if (missing(SF)) {
    SF <- NULL
  }
  if (nlyr(BioClims) != 19) {
    stop("The raster data you supplied does not contain 19 layers. Please supply raster data containing 19 layers corresponding to the 19 bioclimatic variables.")
  }
  BC_names <- c("Annual Mean Temperature", "Mean Diurnal Range", "Isothermality", "Temperature Seasonality", "Max Temperature of Warmest Month", "Min Temperature of Coldest Month", "Temperature Annual Range (BIO5-BIO6)", "Mean Temperature of Wettest Quarter", "Mean Temperature of Driest Quarter", "Mean Temperature of Warmest Quarter", "Mean Temperature of Coldest Quarter", paste("Annual", Water_Var), paste(Water_Var, "of Wettest Month"), paste(Water_Var, "of Driest Month"), paste(Water_Var, "Seasonality"), paste(Water_Var, "of Wettest Quarter"), paste(Water_Var, "of Driest Quarter"), paste(Water_Var, "of Warmest Quarter"), paste(Water_Var, "of Coldest Quarter"))
  BC_names <- paste0("BIO", 1:19, " - ", BC_names)
  names(BioClims) <- BC_names

  ToPlot <- BioClims[[Which]]
  Colours <- list(
    Temperature = inferno(1e3),
    Water = mako(1e3)
  )

  Plots_ls <- lapply(1:nlyr(ToPlot), FUN = function(x) {
    COL <- ifelse(grepl(Water_Var, names(ToPlot[[x]]), fixed = TRUE), "Water", "Temperature")
    if (!is.null(SF)) {
      Plot.SpatRast(SpatRast = ToPlot[[x]], SF = SF, Dates = names(ToPlot[[x]]), Legend = "", COL = Colours[[COL]], Size = Size, Shape = Shape)
    } else {
      Plot.SpatRast(SpatRast = ToPlot[[x]], Dates = names(ToPlot[[x]]), Legend = " ", COL = Colours[[COL]], Size = Size, Shape = Shape)
    }
  })

  cowplot::plot_grid(plotlist = Plots_ls, ncol = ncol)
} # export the plot
