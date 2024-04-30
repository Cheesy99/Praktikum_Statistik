#a)
Air_Transport_Data <- read.csv("../Docs/Air transport of passengers by country (yearly data).csv", header = TRUE, sep = ";", dec = ",", stringsAsFactors = FALSE)
Immigration_Data <- read.csv("../Docs/Immigration.csv", header = TRUE, sep = ";", dec = ",", stringsAsFactors = FALSE)
# Cleaning data
names(Air_Transport_Data)[1] <- "Countries"
names(Immigration_Data)[1] <- "Countries"
selected_columns <- c("Countries", "X2013","X2014","X2015","X2016","X2017","X2018","X2019","X2020", "X2021")
Air_Transport_Data <- subset(Air_Transport_Data, select = selected_columns)
Immigration_Data <- subset(Immigration_Data, select = selected_columns)
names(Air_Transport_Data) <- sub("^X", "", names(Air_Transport_Data))
names(Immigration_Data) <- sub("^X", "", names(Immigration_Data))

countries <- c("Belgium", "Bulgaria", "Czechia", "Denmark","Germany", "Estonia", "Ireland", "Greece", "Spain", "France", "Croatia", "Italy", "Cyprus"
  ,"Latvia", "Lithuania", "Luxembourg", "Hungary", "Malta", "Netherlands", "Austria", "Poland", "Portugal", "Romania", "Slovenia", "Slovakia", "Finland",
               "Sweden", "Iceland", "Norway", "Switzerland")

normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

convert_to_numeric <- function(x) {
  x[x == ":"] <- NA
  as.numeric(gsub("\\.", "", x))
}
#a)
# A
colors <- rainbow(length(countries))

plot(1, 1, type = "n", xlim = c(2013, 2021), ylim = c(0, 1), xlab = "Year", ylab = "Normalized Value", main = "Normalized Air Transport Data")

# Loop over each country for Air Transport Data
# Loop over each country for Air Transport Data
for (i in seq_along(countries)) {
  country <- countries[i]
  color <- colors[i]

  # Subset the data for the current country
  country_Air_Transport_Data <- subset(Air_Transport_Data, Countries == country)[-1]
  # Convert the data to numeric
  country_Air_Transport_Data <- sapply(country_Air_Transport_Data[-1], convert_to_numeric)

  # Normalize the data
  country_Air_Transport_Data_normalized <- normalize(country_Air_Transport_Data)
  country_Air_Transport_Data_normalized <- country_Air_Transport_Data_normalized[!is.na(country_Air_Transport_Data_normalized)]

  # Create a vector of years
  years <- 2013:(2013 + length(country_Air_Transport_Data_normalized) - 1)

  # Draw the line for the current country
  lines(years, country_Air_Transport_Data_normalized, type = "l", col = color)
  }

# B

# Add a legend for Air Transport Data
legend("topright", legend = countries, col = colors, lty = 1, cex = 0.75)

# Set up the plot
plot(1, 1, type = "n", xlim = c(2013, 2021), ylim = c(0, 1), xlab = "Year", ylab = "Normalized Value", main = "Normalized Immigration Data")

# Loop over each country for Immigration Data
for (i in seq_along(countries)) {
  country <- countries[i]
  color <- colors[i]

  # Subset the data for the current country
  country_Immigration_Data <- subset(Immigration_Data, Countries == country)[-1]
  # Convert the data to numeric
  country_Immigration_Data <- sapply(country_Immigration_Data[-1], convert_to_numeric)

  # Normalize the data
  country_Immigration_Data_normalized <- normalize(country_Immigration_Data)

  # Remove NA values
  country_Immigration_Data_normalized <- country_Immigration_Data_normalized[!is.na(country_Immigration_Data_normalized)]

  # Create a vector of years
  years <- 2013:(2013 + length(country_Immigration_Data_normalized) - 1)

  # Draw the line for the current country
  lines(years, country_Immigration_Data_normalized, type = "l", col = color)
}

# Add a legend for Immigration Data
legend("topright", legend = countries, col = colors, lty = 1, cex = 0.75)

#b)

# Initialize vectors to store the averages
averages_Air_Transport_Data <- numeric()
averages_Immigration_Data <- numeric()

# Loop over each year
for (year in 2013:2021) {
  # Subset the data for the current year
  year_Air_Transport_Data <- subset(Air_Transport_Data, Countries %in% countries)[, as.character(year)]
  year_Immigration_Data <- subset(Immigration_Data, Countries %in% countries)[, as.character(year)]

  # Convert the data to numeric
  year_Air_Transport_Data <- sapply(year_Air_Transport_Data, convert_to_numeric)
  year_Immigration_Data <- sapply(year_Immigration_Data, convert_to_numeric)

  # Calculate the average and store it in the vectors
  averages_Air_Transport_Data <- c(averages_Air_Transport_Data, mean(year_Air_Transport_Data, na.rm = TRUE))
  averages_Immigration_Data <- c(averages_Immigration_Data, mean(year_Immigration_Data, na.rm = TRUE))
  # Plot the averages

}

# Calculate the correlation between the averages
correlation <- cor(averages_Air_Transport_Data, averages_Immigration_Data, use = "pairwise.complete.obs")
print(paste("The correlation between the averages of Air Transport Data and Immigration Data is", correlation))

# Calculate the significance of the correlation
correlation_test <- cor.test(averages_Air_Transport_Data, averages_Immigration_Data, method = "pearson")
print(paste("The p-value for the correlation is", correlation_test$p.value))

plot(averages_Air_Transport_Data, averages_Immigration_Data, main = "Correlation between Air Transport and Immigration Data", xlab = "Average Air Transport Data", ylab = "Average Immigration Data")

# Add a regression line to the plot
model <- lm(averages_Immigration_Data ~ averages_Air_Transport_Data)
abline(model, col = "red")

# c)

# Get the data for Germany
Germany_A_Data <- subset(Air_Transport_Data, Countries == "Germany")[-1]
Germany_B_Data <- subset(Immigration_Data, Countries == "Germany")[-1]
Germany_A_Data <- sapply(Germany_A_Data[-1], convert_to_numeric)
Germany_B_Data <- sapply(Germany_B_Data[-1], convert_to_numeric)

countries <- setdiff(countries, "Germany")

# Initialize data frames to store the correlations
correlations_Air_Transport_Data <- data.frame(Country = character(), Correlation = numeric())
correlations_Immigration_Data <- data.frame(Country = character(), Correlation = numeric())

# Loop over the countries
for (country in countries) {
# Subset the data for the current country
country_A_Data <- subset(Air_Transport_Data, Countries == country)[-1]
country_B_Data <- subset(Immigration_Data, Countries == country)[-1]

# Convert the data to numeric
country_A_Data <- sapply(country_A_Data[-1], convert_to_numeric)
country_B_Data <- sapply(country_B_Data[-1], convert_to_numeric)
# remove rows with NA values
country_A_Data <- na.omit(country_A_Data)
country_B_Data <- na.omit(country_B_Data)

# Calculate the correlation with Germany's data
correlation_A <- cor(Germany_A_Data, country_A_Data, use = "pairwise.complete.obs")
correlation_B <- cor(Germany_B_Data, country_B_Data, use = "pairwise.complete.obs")

# Store the country and its correlation in the data frames
correlations_Air_Transport_Data <- rbind(correlations_Air_Transport_Data, data.frame(Country = country, Correlation = correlation_A))
correlations_Immigration_Data <- rbind(correlations_Immigration_Data, data.frame(Country = country, Correlation = correlation_B))
}

# Sort the data frames by correlation
correlations_Air_Transport_Data <- correlations_Air_Transport_Data[order(correlations_Air_Transport_Data$Correlation), ]
correlations_Immigration_Data <- correlations_Immigration_Data[order(correlations_Immigration_Data$Correlation), ]

# Print the countries with the highest and lowest correlation with Germany
print(paste("For dataset Air_Transport, the country with the highest correlation with Germany is", tail(correlations_Air_Transport_Data$Country, 1), "and the country with the lowest correlation is", head(correlations_Air_Transport_Data$Country, 1)))
print(paste("For dataset Immigration, the country with the highest correlation with Germany is", tail(correlations_Immigration_Data$Country, 1), "and the country with the lowest correlation is", head(correlations_Immigration_Data$Country, 1)))

