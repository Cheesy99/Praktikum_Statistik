# a)
OECD_Data <- read.csv("Data/oecdM.csv", header = TRUE, sep= ",", dec= ".", stringsAsFactors =  FALSE)
# b)
numeric_cols <- sapply(OECD_Data, is.numeric)
OECD_Data_Numeric <- OECD_Data[, numeric_cols]
mean_values <- apply(OECD_Data_Numeric, 2, mean, na.rm = TRUE)
variance_values <- apply(OECD_Data_Numeric, 2, var, na.rm = TRUE)
# c)

# Install the readr package if not already installed
"Niederlande" %in% OECD_Data[,1]
"China" %in% OECD_Data[,1]

# d)
countires_withDrunk_youth <- complete.cases(OECD_Data$Alkohol)
OECD_Alkohol <- OECD_Data[countires_withDrunk_youth, ]
Alkohol_max <- which.max(OECD_Alkohol$Alkohol)
Country_max <- OECD_Alkohol$`ï..`[Alkohol_max]
countries_with_alkohol <- OECD_Alkohol$`ï..`
print(countries_with_alkohol)
print(Country_max)
# e)
index <- which.min(OECD_Data_Numeric$`SÃ.uglsterblichkeit`)
country <- OECD_Data$`ï..`[index]
infant_mortality_rate <- OECD_Data_Numeric$`SÃ.uglsterblichkeit`[index]
print(country)
print(infant_mortality_rate)

# f)
average_exercise <- mean(OECD_Data$Bewegung, na.rm = TRUE)
countries_below_average <- OECD_Data[OECD_Data$Bewegung < average_exercise, 1]
print(average_exercise)
print(countries_below_average)

# Aufgabe_02
# a)
europe_count <- sum(OECD_Data$Geo == "E", na.rm = TRUE)
rest_of_world_count <- sum(OECD_Data$Geo != "E", na.rm = TRUE)

# Create a pei chart to represent this information
counts <- c(europe_count, rest_of_world_count)
labels <- c("Europe", "Rest of the world")
colors <- c("green", "blue")
print(counts)
pie(counts, labels = labels, col = colors, main = "Number of Countries in Europe vs Rest of the World")
# b)
# Visualize the 'Lesen' variable separeted by the 'Geo'
stripchart(OECD_Data$Lesen ~ OECD_Data$Geo, vertical = TRUE, method = "jitter",
          main = "Stripchart of 'Lesen' separated by 'Geo",
         xlad = "Geo", ylab = "Lesen")
# TODO Welche Aussage können Sie mit diesem Stripchart treffen?

# Aufgabe 3
# a)
boxplot(OECD_Data$Bildung, main = "Boxplot of 'Bildung'", ylab = "Bildung" )

# TODO Was fällt Ihnen auf?

# b)
quantiles <- quantile(OECD_Data$Bildung, na.rm = TRUE)
print(quantiles)

# c)
# Sort the 'Bildung' values in ascending order
sorted_bildung <- sort(OECD_Data$Bildung, na.last = TRUE)

# Plot the sorted 'Bildung' values as a curve
plot(sorted_bildung, type = "l", main = "Sorted 'Bildung' Values", ylab = "Bildung")
