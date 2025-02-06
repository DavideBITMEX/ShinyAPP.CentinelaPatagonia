### Noise Mean full data
# Count the number of values greater than 100
count_above_100 <- sum(Quemchi2024_NoiseBand$noiseMean > 100, na.rm = TRUE)
# Count the total number of values (excluding NAs)
total_count <- sum(!is.na(Quemchi2024_NoiseBand$noiseMean))
# Calculate the percentage
percentage_above_100 <- (count_above_100 / total_count) * 100
# Print the result
percentage_above_100

summary(Quemchi2024_NoiseBand$noiseMean[Quemchi2024_NoiseBand$octaveBand == 1])
summary(Quemchi2024_NoiseBand$noiseMean[Quemchi2024_NoiseBand$octaveBand == 2])
summary(Quemchi2024_NoiseBand$noiseMean[Quemchi2024_NoiseBand$octaveBand == 3])
summary(Quemchi2024_NoiseBand$noiseMean[Quemchi2024_NoiseBand$octaveBand == 4])
summary(Quemchi2024_NoiseBand$noiseMean[Quemchi2024_NoiseBand$octaveBand == 5])
summary(Quemchi2024_NoiseBand$noiseMean[Quemchi2024_NoiseBand$octaveBand == 6])
summary(Quemchi2024_NoiseBand$noiseMean[Quemchi2024_NoiseBand$octaveBand == 7])
summary(Quemchi2024_NoiseBand$noiseMean[Quemchi2024_NoiseBand$octaveBand == 8])



plot(Quemchi2024_NoiseBand$noisePeak)
# Count the number of values greater than 100
count_above_100 <- sum(Quemchi2024_NoiseBand$noisePeak > 100, na.rm = TRUE)

# Count the total number of values (excluding NAs)
total_count <- sum(!is.na(Quemchi2024_NoiseBand$noisePeak))

# Calculate the percentage
percentage_above_100 <- (count_above_100 / total_count) * 100

# Print the result
percentage_above_100
