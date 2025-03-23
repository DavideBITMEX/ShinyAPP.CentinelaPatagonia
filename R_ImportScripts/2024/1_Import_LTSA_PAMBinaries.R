library(PamBinaries)
library(lubridate)
library(dplyr)

### Import LTSA 8 November
{
  # Set the folder containing your .pgdf files
  folderPath <- '/Users/davide/Pamguard Projects/CentinelaPatagonia/2024 November Campaign/20241100_quemchi/Database/Binary/20241108/'
  # Get a list of .pgdf files starting with "Noise_Band" in the folder
  pgdfFiles <- list.files(path = folderPath, pattern = "^LTSA.*\\.pgdf$", full.names = TRUE)
  # Initialize an empty list to store data from each file
  allData <- list()
  # Loop through each filtered .pgdf file and load the data
  for (file in pgdfFiles) {
    # Load the binary file
    LTSABandData <- loadPamguardBinaryFile(file, convertDate = TRUE)
    # Convert the loaded data to a data frame
    Nov8 <- pbToDf(LTSABandData$data)
    # Append the data frame to the list
    allData[[file]] <- Nov8
  }
  # Combine all data frames into a single data frame
  Nov8 <- do.call(rbind, allData)
  # View the combined data frame
  head(Nov8)
  # remove unnecessary data
  rm(LTSABandData, allData)
}

{
  # Load required packages
  library(dplyr)
  library(ggplot2)

  ## ~~~~~ Calibration and Spectrum Parameters ~~~~~
  # These values match your MATLAB example
  sR    <- 24000        # Sample rate in Hz
  hsens <- -175.9          # Hydrophone sensitivity (dB re 1 V/µPa)
  gain  <- 0             # Additional gain (dB)

  ## ~~~~~ Prepare the Data ~~~~~
  # We assume that your LTSA data (imported as Nov8) has one row per time-frequency cell.
  # In particular, for each unique time (the 'date' column) there is a block of rows
  # corresponding to the different frequency bins for that LTSA time bin.
  # (You can verify this by checking, for example, the number of rows per unique date.)

  # Add a frequency–bin index within each LTSA time bin.
  Nov8_spec <- Nov8 %>%
    group_by(date) %>%
    mutate(bin = row_number()) %>%   # frequency bin index (1, 2, 3, …)
    ungroup()

  # For each time bin (each unique date), compute the number of frequency bins and assign a frequency value.
  # Here we assume that the frequency axis runs linearly from 0 Hz to sR/2.
  Nov8_spec <- Nov8_spec %>%
    group_by(date) %>%
    mutate(nBins = n(),
           # Frequency in Hz: assign 0 Hz to bin 1 and sR/2 to the last bin.
           frequency = (bin - 1) * (sR/2) / (nBins - 1)) %>%
    ungroup()

  ## ~~~~~ Convert Amplitude to dB ~~~~~
  # Here we assume that the 'data' column contains a linear amplitude.
  # To convert to decibels (dB), we use 20*log10(amplitude) and then add the hydrophone calibration.
  # (Make sure that there are no zero or negative values in 'data' before taking log10.)
  Nov8_spec <- Nov8_spec %>%
    mutate(calib_dB = 20 * log10(data) + hsens + gain) # this is probably wrong, why 20log10(data)?

  ## ~~~~~ Plot the Spectrogram ~~~~~
  # We use ggplot2 to create a tile plot with time on the x–axis and frequency on the y–axis.
  # The fill color corresponds to the calibrated level in dB.
  p <- ggplot(Nov8_spec, aes(x = date, y = frequency, fill = calib_dB)) +
    geom_tile() +
    scale_fill_gradientn(colors = rev(terrain.colors(10)),
                         name = "Level (dB re 1 µPa)") +
    labs(x = "Time", y = "Frequency (Hz)", title = "LTSA Spectrogram") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
          axis.text.y = element_text(size = 12),
          axis.title  = element_text(size = 14),
          plot.title  = element_text(size = 16, face = "bold"))

  # Print the plot
  print(p)

}
