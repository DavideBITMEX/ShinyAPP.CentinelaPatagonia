# Load ggplot2 package
library(ggplot2)

# Create the PSD over time plot using noiseMean as the PSD value
psd_plot <- ggplot(Quemchi2024_NoiseBand,
                   aes(x = date_UTC, y = factor(octaveBand), fill = noiseMean)) +
  geom_tile() +
  scale_fill_gradient(low = "blue", high = "yellow") +
  labs(
    title = "PSD Over Time",
    x = "Time (UTC)",
    y = "Octave Band",
    fill = "Noise Mean (dB)"
  ) +
  theme_minimal()

# Display the plot
print(psd_plot)

### USE PAMGUIDE INSTEAD
