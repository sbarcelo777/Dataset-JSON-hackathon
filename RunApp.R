
###############################################################################;
# PROJECT/PATH : <PROGRAM_PATH>
# PROGRAM NAME : <PROG_NAME>
# CREATION DATE: 11/15/2021 (mm/dd/yyyy)
# PROGRAMMER   : Sebastià Barceló Bauzà (user name)
# PURPOSE      : Instructions for data manipulation.
#                NOTE: R is key sensitive regarding to the name of the variables
###############################################################################;
# Modifications History:
#------------------------------------------------------------------------------;
# Modif. #01   :
# Programmer   :
# Date         :
# Purpose      :
#------------------------------------------------------------------------------;
# Modif. #0n   :
# Programmer   :
# Date         :
# Purpose      :
###############################################################################;


#--- global settings;

library(shiny)
library(shinythemes)
hello
library(dplyr)

library(gridExtra)
library(rlang)

#install.packages("jsonlite")
library(jsonlite)
#install.packages("readr")
library(readr)
#install.packages("dplyr")
library(dplyr)

json_str <- readr::read_file("Data/SDTM/dm.json")

data <- jsonlite::fromJSON(json_str, simplifyVector = F)

cols <- c(sapply(data$columns, function(x) x$name))

dm <- data$rows %>% 
  lapply(function(row) {
    data.frame(matrix(unlist(row), nrow = 1), stringsAsFactors = FALSE)
  }) %>%
  dplyr::bind_rows()

names(dm) <- cols

json_str <- readr::read_file("Data/SDTM/ae.json")

data <- jsonlite::fromJSON(json_str, simplifyVector = F)

cols <- c(sapply(data$columns, function(x) x$name))

ae <- data$rows %>% 
  lapply(function(row) {
    data.frame(matrix(unlist(row), nrow = 1), stringsAsFactors = FALSE)
  }) %>%
  dplyr::bind_rows()

names(ae) <- cols


# Run App
runApp()

#*--- end of program ------------------------------------------------------------------------------;