# KrigR
An R Package for downloading, preprocessing, and statistical downscaling of the European Centre for Medium-range Weather Forecasts ReAnalysis 5 (ERA5) family provided by the [European Centre for Medium‐Range Weather Forecasts (ECMWF)](https://www.ecmwf.int/).The package interfaces with the [Climate Data Store](https://cds.climate.copernicus.eu/#!/home) hosted by the [Copernicus Climate Change Service (C3S)](https://cds.climate.copernicus.eu/about-c3s) for retrieval of climate data.

<img align="right" width="300" src="inst/extdata/KrigRLogo.png" />

KrigR contains functions for:
- Downloading Era5(-Land) data directly from within `R` via a wrapper function for the [`ecmwfr` package](https://github.com/bluegreen-labs/ecmwfr)
- Downloading interpolation covaraite data:
    - [USGS GMTED 2010](https://www.usgs.gov/core-science-systems/eros/coastal-changes-and-impacts/gmted2010?qt-science_support_page_related_con=0#qt-science_support_page_related_con) elevation data  
    - [soil hydraulic and thermal parameters](http://globalchange.bnu.edu.cn/research/soil4.jsp) data
- Preparing covariate data for use in statistical interpolation
- Kriging spatial input to desired output using user-specified covariates via a wrapper for the [`automap` package](https://github.com/cran/automap)

**NOTE:** All kriging functionality can be parallelised for faster computing using the `Cores` argument.

# How to Cite
KrigR has been [published here](https://iopscience.iop.org/article/10.1088/1748-9326/ac48b3).

# Installation
KrigR is not yet on CRAN, so it needs to be installed as such:

```{r}
devtools::install_github("https://github.com/ErikKusch/KrigR")
library(KrigR)
```
Users require personal API-access tokens which can be obtained [here](https://accounts.ecmwf.int/auth/realms/ecmwf/login-actions/registration?client_id=cds&tab_id=VkbipqjwuIQ).

# Workshop Material and Further Information
I have put together a comprehensive [workshop](https://www.erikkusch.com/courses/krigr/) that walks you through the functionality of the KrigR package. For any additional information on the project please refer to the project [website](https://www.erikkusch.com/project/krigr/). Upates will be available on said website as well as the KrigR [twitter profile](https://twitter.com/ERAKrigR)

# Abstract
Advances in climate science have rendered obsolete the gridded observation data widely used in downstream applications. Novel climate reanalysis products outperform legacy data products in accuracy, temporal resolution, and provision of uncertainty metrics. Consequently, there is an urgent need to develop a workflow through which to integrate these improved data into biological analyses. The ERA5 product family (ERA5 and ERA5-Land) are the latest and most advanced global reanalysis products created by the European Center for Medium-range Weather Forecasting (ECMWF). These data products offer up to 83 essential climate variables (ECVs) at hourly intervals for the time-period of 1981 to today with preliminary back-extensions being available for 1950-1981. Spatial resolutions range from 30x30km (ERA5) to 11x11km (ERA5-Land) and can be statistically downscaled to study-requirements at finer spatial resolutions. Kriging is one such method to interpolate data to finer resolutions and has the advantages that one can leverage additional covariate information and obtain the uncertainty associated with the downscaling. 

The KrigR R-package enables users to (1) download ERA5(-Land) climate reanalysis data for a user-specified region, and time-period, (2) aggregate these climate products to desired temporal resolutions and metrics, (3) acquire topographical co-variates, and (4) statistically downscale spatial data to a user-specified resolution using co-variate data via kriging. KrigR can execute all these tasks in a single function call, thus enabling the user to obtain any of 83 (ERA5) / 50 (ERA5-Land) climate variables at high spatial and temporal resolution with a single R-command. Additionally, KrigR contains functionality for computation of bioclimatic variables and aggregate metrics from the variables offered by ERA5(-Land).

This R-package provides an easy-to-implement workflow for implementation of state-of-the-art climate data while avoiding issues of storage limitations at high temporal and spatial resolutions by providing data according to user-needs rather than in global data sets. 
Consequently, KrigR provides a toolbox to obtain a wide range of tailored climate data at unprecedented combinations of high temporal and spatial resolutions thus enabling the use of world-leading climate data through the R-interface and beyond.

# Keywords
Climate data, Climate reanalysis, Interdisciplinary ecology, Macroecology, R, R Package, Statistical downscaling
