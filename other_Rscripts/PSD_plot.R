# Load required packages
library(seewave)
library(tuneR)
library(dplyr)
library(ggplot2)

# Define the folder containing your WAV files
wav_folder <- "/Users/davide/Pamguard Projects/CentinelaPatagonia/2024 November Campaign/20241100_quemchi"  # update with your folder path
wav_files <- list.files(path = wav_folder, pattern = "\\.wav$", full.names = TRUE)

# Parameters for segmentation and spectrogram
seg_duration <- 60        # Process in 60-second chunks (adjust as needed)
wl <- 8192                # FFT window length (large to reduce time bins)
ovlp <- 50                # Overlap percentage
ds_factor_time <- 4       # Downsample factor for time dimension
ds_factor_freq <- 4       # Downsample factor for frequency dimension

# Objects to hold the combined spectrogram data
combined_amp <- NULL
combined_time <- c()
combined_freq <- NULL
time_offset <- 0  # cumulative time offset

for (file in wav_files) {
  # Read file header to get sampling rate and total duration without loading full data
  wave_info <- readWave(file, header = TRUE)
  samp_rate <- wave_info$samp.rate
  # Ensure total_samples is a single number:
  total_samples <- if(length(wave_info$samples) > 1) wave_info$samples[1] else wave_info$samples
  total_duration <- total_samples / samp_rate

  # Create segmentation start times (in seconds)
  seg_starts <- seq(0, total_duration - seg_duration, by = seg_duration)
  # If seg_starts is empty (file is shorter than seg_duration), use 0 as start
  if(length(seg_starts) == 0) {
    seg_starts <- 0
  } else if (tail(seg_starts, 1) + seg_duration < total_duration) {
    # Optionally add an extra segment if the file has a leftover part
    seg_starts <- c(seg_starts, tail(seg_starts, 1) + seg_duration)
  }

  for (seg_start in seg_starts) {
    seg_end <- min(seg_start + seg_duration, total_duration)

    # Read a segment from the file
    wave_seg <- readWave(file, from = seg_start, to = seg_end, units = "seconds")

    # Compute spectrogram without plotting
    spec_seg <- spectro(wave_seg, wl = wl, ovlp = ovlp, scale = FALSE, plot = FALSE)

    # Downsample the amplitude matrix to reduce memory usage
    ds_rows <- seq(1, nrow(spec_seg$amp), by = ds_factor_freq)
    ds_cols <- seq(1, ncol(spec_seg$amp), by = ds_factor_time)
    spec_seg$amp <- spec_seg$amp[ds_rows, ds_cols]
    spec_seg$freq <- spec_seg$freq[ds_rows]
    spec_seg$time <- spec_seg$time[ds_cols]

    # Adjust the time vector by the cumulative offset so that segments line up
    spec_seg$time <- spec_seg$time + time_offset

    # Combine this segment's spectrogram with previous segments
    if (is.null(combined_amp)) {
      combined_amp <- spec_seg$amp
      combined_time <- spec_seg$time
      combined_freq <- spec_seg$freq
    } else {
      combined_amp <- cbind(combined_amp, spec_seg$amp)
      combined_time <- c(combined_time, spec_seg$time)
    }
  }

  # Update cumulative time offset by adding the file's total duration
  time_offset <- time_offset + total_duration
}

# Finally, plot the combined, downsampled spectrogram using image()
image(x = combined_time, y = combined_freq, z = t(combined_amp),
      xlab = "Time (s)", ylab = "Frequency (Hz)",
      main = "Combined Spectrogram (Segmented & Downsampled)",
      col = rev(heat.colors(256)))

# Optionally, capture the plot for later replay:
final_spectro_plot <- recordPlot()
