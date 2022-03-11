
.First <- function() {
  options(repos = c(
    CRAN = "https://mran.microsoft.com/snapshot/2021-01-01",
    INWTLab = "https://inwtlab.github.io/drat/",
    PANDORA = "https://Pandora-IsoMemo.github.io/drat/"
  ))
  
  # Check operating system
  if (Sys.info()["sysname"] == "Windows") {
    # Add libWin with the full path to libPaths
    .libPaths(new = c(paste(getwd(), "libWin", sep = "/"), .libPaths()))
  } else if (Sys.info()["sysname"] == "Linux") {
    .libPaths(new = c(paste(getwd(), "libLinux", sep = "/"), .libPaths()))
  } else if (Sys.info()["sysname"] == "Darwin") {
    .libPaths(new = c(paste(getwd(), "libMac", sep = "/"), .libPaths()))
  }
}

.First()
