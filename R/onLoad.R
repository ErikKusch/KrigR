# .onLoad <- function(libname, pkgname)
# {
#   library.dynam("mclust", pkgname, libname)
# }
.onAttach <- function(lib, pkg) {
    # startup message
    msg <- paste0("This is KrigR (version ", packageVersion("KrigR"), "). Note that as of version 0.5.0, considerable changes from the development path have been merged with the main branch leading to the deprecation of some older and outdated functionality. If you have used a version of KrigR prior to 0.5.0 before, we strongly recommend you re-familiarise yourself with the complete suite of KrigR. This message will keep showing until KrigR version 1.0.0 is achieved.")
    packageStartupMessage(msg)
    # invisible()
}


usethis::use_citation()
# Please cite it as Kusch, E., & Davy, R. (2022). KrigR-a tool for downloading and statistically downscaling climate reanalysis data. Environmental Research Letters, 17(2). https://doi.org/10.1088/1748-9326/ac48b3"
