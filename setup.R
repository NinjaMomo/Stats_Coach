#Function to load relevant libraries. Checks are done to install necessary packages
#
#Args:
#   NULL
#
#Returns:
#   NULL

importLibraries <- function()
{
  if(!(suppressWarnings(suppressMessages(require(XML)))))
  {
    install.packages("XML", quiet = TRUE)
    library(XML)
  }
  
  if(!(suppressWarnings(suppressMessages(require(rmarkdown)))))
  {
    install.packages("rmarkdown", quiet = TRUE)
    library(rmarkdown)
  }
}

#Function to source relevant scripts
#
#Args:
#   NULL
#
#Returns:
#   NULL

sourceAll <- function()
{
  source("setFunctions.R")
  source("masterFunctions.R")
  source("classes.R")
  source("cleanup.R")
}