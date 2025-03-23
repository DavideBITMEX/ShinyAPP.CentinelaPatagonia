# make sure you have Rtools installed
if(!require('devtools')) install.packages('devtools')
# install from GitHub
devtools::install_github('TaikiSan21/PamBinaries')
library(PamBinaries)
library(lubridate)
library(dplyr)

################################
# Import NoiseBand data, Quemchi
# 5 days effective of campaign:
# 5-9 Nov 2024
################################

######################################################
# Load Multiple /pgdf files from the same folder (day)
# We create 5 datasets, one for each day
### 5 Nov
{
  # Set the folder containing your .pgdf files
  folderPath <- '/Users/davide/Pamguard Projects/CentinelaPatagonia/2024 November Campaign/20241100_quemchi/Database/Binary/20241105/'
  # Get a list of .pgdf files starting with "Noise_Band" in the folder
  pgdfFiles <- list.files(path = folderPath, pattern = "^Noise_Band.*\\.pgdf$", full.names = TRUE)
  # Initialize an empty list to store data from each file
  allData <- list()
  # Loop through each filtered .pgdf file and load the data
  for (file in pgdfFiles) {
    # Load the binary file
    NoiseBandData <- loadPamguardBinaryFile(file, convertDate = TRUE)
    # Convert the loaded data to a data frame
    Nov5 <- pbToDf(NoiseBandData)
    # Append the data frame to the list
    allData[[file]] <- Nov5
  }
  # Combine all data frames into a single data frame
  Nov5 <- do.call(rbind, allData)
  # View the combined data frame
  head(Nov5)
  # remove unnecessary data
  rm(NoiseBandData, allData)
  # check how many bands
  unique(Nov5$nBands)
}
### 6 Nov
{
  # Set the folder containing your .pgdf files
  folderPath <- '/Users/davide/Pamguard Projects/CentinelaPatagonia/2024 November Campaign/20241100_quemchi/Database/Binary/20241106/'
  # Get a list of .pgdf files starting with "Noise_Band" in the folder
  pgdfFiles <- list.files(path = folderPath, pattern = "^Noise_Band.*\\.pgdf$", full.names = TRUE)
  # Initialize an empty list to store data from each file
  allData <- list()
  # Loop through each filtered .pgdf file and load the data
  for (file in pgdfFiles) {
    # Load the binary file
    NoiseBandData <- loadPamguardBinaryFile(file, convertDate = TRUE)
    # Convert the loaded data to a data frame
    Nov6 <- pbToDf(NoiseBandData)
    # Append the data frame to the list
    allData[[file]] <- Nov6
  }
  # Combine all data frames into a single data frame
  Nov6 <- do.call(rbind, allData)
  # View the combined data frame
  head(Nov6)
  # remove unnecessary data
  rm(NoiseBandData, allData)
  # check how many bands
  unique(Nov6$nBands)
}
### 7 Nov
{
  # Set the folder containing your .pgdf files
  folderPath <- '/Users/davide/Pamguard Projects/CentinelaPatagonia/2024 November Campaign/20241100_quemchi/Database/Binary/20241107/'
  # Get a list of .pgdf files starting with "Noise_Band" in the folder
  pgdfFiles <- list.files(path = folderPath, pattern = "^Noise_Band.*\\.pgdf$", full.names = TRUE)
  # Initialize an empty list to store data from each file
  allData <- list()
  # Loop through each filtered .pgdf file and load the data
  for (file in pgdfFiles) {
    # Load the binary file
    NoiseBandData <- loadPamguardBinaryFile(file, convertDate = TRUE)
    # Convert the loaded data to a data frame
    Nov7 <- pbToDf(NoiseBandData)
    # Append the data frame to the list
    allData[[file]] <- Nov7
  }
  # Combine all data frames into a single data frame
  Nov7 <- do.call(rbind, allData)
  # View the combined data frame
  head(Nov7)
  # remove unnecessary data
  rm(NoiseBandData, allData)
  # check how many bands
  unique(Nov7$nBands)
}
### 8 Nov
{
  # Set the folder containing your .pgdf files
  folderPath <- '/Users/davide/Pamguard Projects/CentinelaPatagonia/2024 November Campaign/20241100_quemchi/Database/Binary/20241108/'
  # Get a list of .pgdf files starting with "Noise_Band" in the folder
  pgdfFiles <- list.files(path = folderPath, pattern = "^Noise_Band.*\\.pgdf$", full.names = TRUE)
  # Initialize an empty list to store data from each file
  allData <- list()
  # Loop through each filtered .pgdf file and load the data
  for (file in pgdfFiles) {
    # Load the binary file
    NoiseBandData <- loadPamguardBinaryFile(file, convertDate = TRUE)
    # Convert the loaded data to a data frame
    Nov8 <- pbToDf(NoiseBandData)
    # Append the data frame to the list
    allData[[file]] <- Nov8
  }
  # Combine all data frames into a single data frame
  Nov8 <- do.call(rbind, allData)
  # View the combined data frame
  head(Nov8)
  # remove unnecessary data
  rm(NoiseBandData, allData)
  # check how many bands
  unique(Nov8$nBands)
}
### 9 Nov
{
  # Set the folder containing your .pgdf files
  folderPath <- '/Users/davide/Pamguard Projects/CentinelaPatagonia/2024 November Campaign/20241100_quemchi/Database/Binary/20241109/'
  # Get a list of .pgdf files starting with "Noise_Band" in the folder
  pgdfFiles <- list.files(path = folderPath, pattern = "^Noise_Band.*\\.pgdf$", full.names = TRUE)
  # Initialize an empty list to store data from each file
  allData <- list()
  # Loop through each filtered .pgdf file and load the data
  for (file in pgdfFiles) {
    # Load the binary file
    NoiseBandData <- loadPamguardBinaryFile(file, convertDate = TRUE)
    # Convert the loaded data to a data frame
    Nov9 <- pbToDf(NoiseBandData)
    # Append the data frame to the list
    allData[[file]] <- Nov9
  }
  # Combine all data frames into a single data frame
  Nov9 <- do.call(rbind, allData)
  # View the combined data frame
  head(Nov9)
  # remove unnecessary data
  rm(NoiseBandData, allData)
  # check how many bands
  unique(Nov9$nBands)
}
######################################################

######################################################
# 1. Merge the datasets into one unique dataset
# 2. change date/time from UTC ---> Local Time (CLST Time...UTC - 3h)
# 3. - Average 'noiseMean' per minute (per Octave band), so we have a smaller dataset (but still with the info we need)
#    - Take the max value of 'noisePeak per minute (per Octave band), so we have a smaller dataset (but still with the info we need)
# 4. Remove first and last 30 minutes (to ensure we don't get any sounds from our boat deploying/retrieving the soundtrap)
# 5. Save data into .RData file (R Environment), so next time we can directly just load it
{
  ### 1.
      Quemchi2024_NoiseBand <- rbind(Nov5, Nov6, Nov7, Nov8, Nov9)
  ### 2.
      # rename date into date_UTC
      Quemchi2024_NoiseBand <- Quemchi2024_NoiseBand %>%
        rename(date_UTC = date)
      # create date_Local
      Quemchi2024_NoiseBand$date_Local <- with_tz(Quemchi2024_NoiseBand$date_UTC, tzone = "America/Santiago")
      # 'https://en.wikipedia.org/wiki/List_of_tz_database_time_zones' reference for TZ codes
      head(Quemchi2024_NoiseBand$date_UTC)
      head(Quemchi2024_NoiseBand$date_Local)
      # alternativerly: Quemchi2024_NoiseBand$date_Local <- Quemchi2024_NoiseBand$date_UTC - hours(3)
  ### 3.
      # Calculate the average noiseMean per minute for each octaveBand, and the maximum noisePeak
      Quemchi2024_NoiseBand_final <- Quemchi2024_NoiseBand %>%
        mutate(date_Local = format(date_Local, "%Y-%m-%d %H:%M")) %>% # Extract the minute from the date
        group_by(octaveBand, date_Local) %>%
        summarise(noiseMean = mean(noiseMean, na.rm = TRUE),
                  noisePeak = max(noisePeak, na.rm = TRUE),
                  .groups = "drop") %>% # Average noiseMean
        arrange(date_Local, octaveBand) # Order by minute and octaveBand
      # Isolate the variables we need
      Quemchi2024_NoiseBand_final <- Quemchi2024_NoiseBand_final %>% # If you launch this more than once, you'll have to re-run 3.
        mutate(date_Local = with_tz(as.POSIXct(date_Local), tzone = "America/Santiago") + hours(4),
               date_UTC = with_tz(date_Local, tzone = "UTC")) %>%
        select(date_UTC, date_Local, octaveBand, noiseMean, noisePeak)
      # check tz
      head(Quemchi2024_NoiseBand_final$date_Local)
      head(Quemchi2024_NoiseBand_final$date_UTC)
  ### 4.
      # Removing the first ~ 30 minutes and last ~ 5 minutes, to ensure we get just underwater data (remove in-air data)
      # We want data from (Local time) 13:35 5 Nov until 12:25 9 Nov
      Quemchi2024_NoiseBand_final_filt <- Quemchi2024_NoiseBand_final %>%
        filter(date_Local >= (with_tz(as.POSIXct("2024-11-05 13:35:00"), tzone = "America/Santiago") + hours(4)) &
                 date_Local <= (with_tz(as.POSIXct("2024-11-09 12:25:00"), tzone = "America/Santiago") + hours(4)))
      # CHECK IF THE REMOVED OBSERVATIONS ARE CORRECT:
      # Find the removed observations
      removed_observations <- anti_join(Quemchi2024_NoiseBand_final, Quemchi2024_NoiseBand_final_filt, by = c("octaveBand", "date_Local"))
      # Print the dates of the removed observations
      removed_dates <- unique(removed_observations$date_Local)
      print(removed_dates)
      # if everything works fine, remove unnecessary variables
      rm(removed_observations, removed_dates)

}

### 5.
# some cleaning first
Quemchi2024_NoiseBand_final <- Quemchi2024_NoiseBand_final_filt
rm(Quemchi2024_NoiseBand_final_filt)
# save the environment
save("Nov5", "Nov6", "Nov7", "Nov8", "Nov9",
     "Quemchi2024_NoiseBand",
     "Quemchi2024_NoiseBand_final",
     file = "R/Environment.RData")
######################################################



