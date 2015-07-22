# Runs the whole kit and kaboodle
#
# This file does a few things to control the script execution at a high level:
#  1. Declares where your packages are installed
#  2. Sets a reporting period variable (r_period) based on user input. This is useful if you need to subset data by a date which changes month to month
#  3. Lets the user source all of the files stored in a directory with the init() function
#  4. Lets the user source a file stored online with the source_https() function
#

# set directory where your packages are stored. this is useful if you can't control your computer's PATH variable, like they have it set up in City Hall
.libPaths("YOUR PACKAGE DIRECTORY")

# get user to set reporting period. some functions in plotters.R need this variable
cat("What is the reporting period?\n\n##(Use mmm yyyy format, please)##\n\n")
r_period <- readLines("stdin", 1, warn = FALSE)

# sources all the files in a subdirectory
init <- function(subdir) {
  Rfiles <- list.files(subdir, pattern = "*.R", full.names = TRUE)
  sapply(Rfiles, source)
  cat("Running!\n")
}

#function to source files stored on Github. use this to source shared scripts stored on Github by passing the raw URL as u in this function. see below
source_https <- function(u, unlink.tmp.certs = FALSE) {
    require(RCurl)

    if(!file.exists("cacert.pem")) download.file(url="http://curl.haxx.se/ca/cacert.pem", destfile = "cacert.pem")
    script <- getURL(u, followlocation = TRUE, cainfo = "cacert.pem")
    if(unlink.tmp.certs) unlink("cacert.pem")

    eval(parse(text = script), envir= .GlobalEnv)
}

# sequence of script executions. list in the order you need the scripts to execute
source_https("https://raw.githubusercontent.com/cno-opa/graphics/master/plotters.R")
source_https("https://raw.githubusercontent.com/cno-opa/graphics/master/mappers.R")
init("R/lib")
init("R")

# finish
cat("Finished!")
