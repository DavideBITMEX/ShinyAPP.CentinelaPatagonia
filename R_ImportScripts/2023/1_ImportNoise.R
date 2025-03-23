# make sure you have Rtools installed
if(!require('devtools')) install.packages('devtools')
# install from GitHub
devtools::install_github('TaikiSan21/PamBinaries')
library(PamBinaries)
library(lubridate)
library(dplyr)

################################
# Import NoiseBand data, 2023
# Control Area
################################

######################################################
# Load Multiple /pgdf files from the same folder (day)
### 17 Feb 2023
{
  # Set the folder containing your .pgdf files
  folderPath <- '/Users/davide/Pamguard Projects/CentinelaPatagonia/2023 Campaign/6468/Database/Binary/20230217/'
  # Get a list of .pgdf files starting with "Noise_Band" in the folder
  pgdfFiles <- list.files(path = folderPath, pattern = "^Noise_Band.*\\.pgdf$", full.names = TRUE)
  # Initialize an empty list to store data from each file
  allData <- list()
  # Loop through each filtered .pgdf file and load the data
  for (file in pgdfFiles) {
    # Load the binary file
    NoiseBandData <- loadPamguardBinaryFile(file, convertDate = TRUE)
    # Convert the loaded data to a data frame
    Control <- pbToDf(NoiseBandData)
    # Append the data frame to the list
    allData[[file]] <- Control
  }
  # Combine all data frames into a single data frame
  Control <- do.call(rbind, allData)
  # View the combined data frame
  head(Control)
  # remove unnecessary data
  rm(NoiseBandData, allData)
  # check how many bands
  unique(Control$nBands)
}
