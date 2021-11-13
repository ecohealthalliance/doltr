
# #' @param the Dolt version. If NULL, use the latest release version
# dolt_install <- function(version = "latest") {
#   if (version == "latest") {
#     base_url <- "https://github.com/dolthub/dolt/releases/latest/download/"
#   } else {
#     version <- gsub("v", "", version)
#     base_url <- paste0("https://github.com/dolthub/dolt/releases/download/v",
#                        version, "/")
#   }
#
#   sysname <- tolower(Sys.info()[["sysname"]])
#
#   dfile <- switch(sysname,
#                   darwin = "dolt-darwin-amd64.tar.gz",
#                   linux = "dolt-linux-amd64.tar.gz",
#                   windows = "dolt-windows-amd64.msi")
#   tmploc <- tempdir()
#   tmpdl <- file.path(tmploc, dfile)
#   download.file(paste0(base_url, dfile), destfile = tmpdl)
#
# }

# dolt_install_windows <- function(version = NULL) {
#
# }
