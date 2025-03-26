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
######################################################


######################################################
# Data Manipulation
{
# rename date into date_UTC
Control <- Control %>%
  rename(date_UTC = date)
# create date_Local
Control$date_Local <- with_tz(Control$date_UTC, tzone = "America/Santiago")
# 'https://en.wikipedia.org/wiki/List_of_tz_database_time_zones' reference for TZ codes
head(Control$date_UTC)
head(Control$date_Local)
# alternativerly: Control$date_Local <- Control$date_UTC - hours(3)
### 3.
# Calculate the average noiseMean per minute for each octaveBand, and the maximum noisePeak
Control_final <- Control %>%
  mutate(date_Local = format(date_Local, "%Y-%m-%d %H:%M")) %>% # Extract the minute from the date
  group_by(octaveBand, date_Local) %>%
  summarise(noiseMean = mean(noiseMean, na.rm = TRUE),
            noisePeak = max(noisePeak, na.rm = TRUE),
            .groups = "drop") %>% # Average noiseMean
  arrange(date_Local, octaveBand) # Order by minute and octaveBand
# Isolate the variables we need
Control_final <- Control_final %>% # If you launch this more than once, you'll have to re-run 3.
  mutate(date_Local = with_tz(as.POSIXct(date_Local), tzone = "America/Santiago") + hours(4),
         date_UTC = with_tz(date_Local, tzone = "UTC")) %>%
  select(date_UTC, date_Local, octaveBand, noiseMean, noisePeak)
# check tz
head(Control_final$date_Local)
head(Control_final$date_UTC)
### 4.
# Retaing, for example, just 1 hour of recording for control
# We want data from (Local time) 13:35 5 Nov until 12:25 9 Nov
Control_final_filt <- Control_final %>%
  filter(date_Local >= (with_tz(as.POSIXct("2023-02-17 12:00:00"), tzone = "America/Santiago") + hours(4)) &
           date_Local <= (with_tz(as.POSIXct("2023-02-17 13:00:00"), tzone = "America/Santiago") + hours(4)))
# CHECK IF THE REMOVED OBSERVATIONS ARE CORRECT:
# Find the removed observations
removed_observations <- anti_join(Control_final, Control_final_filt, by = c("octaveBand", "date_Local"))
# Print the dates of the removed observations
removed_dates <- unique(removed_observations$date_Local)
print(removed_dates)
# if everything works fine, remove unnecessary variables
rm(removed_observations, removed_dates)


Control_final <- Control_final_filt
rm(Control_final_filt)
}
######################################################

# save the environment
save("Nov5", "Nov6", "Nov7", "Nov8", "Nov9",
     "Quemchi2024_NoiseBand",
     "Quemchi2024_NoiseBand_final",
     "Control_final",
     file = "R/Environment.RData")
