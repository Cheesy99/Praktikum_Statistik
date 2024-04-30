# Aufgabe 4
# a)
X1 <- rexp(n = 100, rate = 0.1)
HX2 <- rexp(n = 100, rate = 0.1)
X2 <- 20 - HX2

#b)
t_test_result <- t.test(X1, X2)
print(t_test_result)

# c)

wilcox_test_result <- wilcox.test(X1, X2)
print(wilcox_test_result)

# Aufgabe 5
# a)
PISA_Data <- read.csv("../Data/PISA.csv", header = TRUE, sep= ",", dec= ".", stringsAsFactors =  FALSE)

# b)
boxplot(PISA_Data[,c("R00", "R06", "M00", "M06", "S00", "S06")], main = "Boxplot of PISA Scores", ylab = "Score", xlab = "Year and Subject" )

# c)

t_test_result_R <- t.test(PISA_Data$R00, PISA_Data$R06, paired = TRUE)
t_test_result_M <- t.test(PISA_Data$M00, PISA_Data$M06, paired = TRUE)
t_test_result_S <- t.test(PISA_Data$S00, PISA_Data$S06, paired = TRUE)

print(t_test_result_M)
print(t_test_result_R)
print(t_test_result_S)

# TODO Untersuchen Sie mit einem geeigneten Test, ob sich die drei PISA-Scores signifikant verändert
# haben.

# Aufgabe 06
Hustensaft_Data <- read.csv("../Data/Hustensaft.csv", header = TRUE, sep= ",", dec= ".", stringsAsFactors =  FALSE)
t_test_result <- t.test(Hustensaft_Data$Kon, mu = 40)

print(t_test_result)

# The t-test result shows that the p-value is 0.1167,
# which is greater than the common significance level of 0.05.
# This means that we fail to reject the null hypothesis
# that the true mean is equal to 40g/L.
# Therefore, based on this sample,
# there is not enough evidence to suggest
# that the concentration of Halsruhe is significantly different from 40g/L.
# Consequently, these data do not justify a production stop.

# Aufgabe 07
# a)
sues <- read.csv("../Data/Suess.csv", header = TRUE, sep = ";", dec = ".", stringsAsFactors = FALSE)
# b)
coplot(Geschmack ~ Feuchtigkeit | Suesse, data = sues, pch = c(5,18), rows = 1, columns = 3)

# c)
model <- lm(Geschmack ~ Feuchtigkeit + Suesse, data = sues)

summary(model)

# d)
interaction_term <- sues$Feuchtigkeit * sues$Suesse
plot(interaction_term, model$residuals, main = "Residuals vs Feuchtigkeit*Suesse", xlab = "Feuchtigkeit", ylab = "Residuals")

# e)
model_interaction <- lm(Geschmack ~ Feuchtigkeit + Suesse + Feuchtigkeit:Suesse, data = sues)
summary(model_interaction)

# f)

plot(interaction_term, model_interaction$residuals, main = "Residuals vs Feuchtigkeit*Suesse (Interaction Model)",
  xlab = "Feuchtigkeit*Suesse", ylab = "Residuals")

# TODO Verbessert sich die Anpassung des Modells an die Daten? Ist der Koeffizient ß3 der Interaktion
# signifikant von 0 verschieden? Plotten Sie erneut die Residuen gegen Feuchtigkeit· Suesse und
# vergleichen Sie die Ergebnisse mit dem Plot aus Aufgabe (d).
#detach(sues)
# Aufgabe 08
# a)
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

# a)
Immigration_Data_Denmark <- subset(Immigration_Data, Countries == "Denmark")
Air_Transport_Data_Denmark <- subset(Air_Transport_Data, Countries == "Denmark")

convert_to_numeric <- function(x) {
  x[x == ":"] <- NA
  as.numeric(gsub("\\.", "", x))
}

# Get the list of all unique countries
all_countries <- unique(Air_Transport_Data$Countries)

# Generate a different color for each country
colors <- rainbow(length(all_countries))
# Subset the years between 2013 and 2021
years <- subset(years, years >= 2013 & years <= 2021)
# Loop over each country
for (i in seq_along(all_countries)) {
  # Get the current country and its color
  country <- all_countries[i]
  color <- colors[i]

  # Subset the data for the current country
  country_Air_Transport_Data <- subset(Air_Transport_Data, Countries == country)[-1]
  country_Immigration_Data <- subset(Immigration_Data, Countries == country)[-1]

  # Convert the data to numeric
  country_Air_Transport_Data <- sapply(country_Air_Transport_Data[-1], convert_to_numeric)
  country_Immigration_Data <- sapply(country_Immigration_Data[-1], convert_to_numeric)

  # Normalize the data
  country_Air_Transport_Data_normalized <- normalize(country_Air_Transport_Data)
  country_Immigration_Data_normalized <- normalize(country_Immigration_Data)

  # Plot the normalized data
  lines(years, country_Air_Transport_Data_normalized, type = "l", col = color)
  lines(years, country_Immigration_Data_normalized, type = "l", col = color)
}


# Normalize the data
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

Immigration_Data_Denmark_normalized <- normalize(Immigration_Data_Denmark)
Air_Transport_Data_Denmark_normalized <- normalize(Air_Transport_Data_Denmark)

# Plot the normalized data
plot(years, Immigration_Data_Denmark_normalized, type = "l", col = "blue", xlab = "Year", ylab = "Value", main = "Normalized Immigration and Air Transport in Denmark")
lines(years, Air_Transport_Data_Denmark_normalized, type = "l", col = "red")
legend("bottomleft", legend = c("Immigration", "Air Transport"), col = c("blue", "red"), lty = 1)
# b)
plot(Air_Transport_Data_Denmark, Immigration_Data_Denmark, main = "Air Transport vs Immigration", xlab = "Air Transport", ylab = "Immigration")
model <- lm(Immigration_Data_Denmark ~ Air_Transport_Data_Denmark)

# Add a regression line to the plot
abline(model, col = "red")

correlation <- cor(Air_Transport_Data_Denmark, Immigration_Data_Denmark, use = "pairwise.complete.obs")
print(paste("Correlation: ", correlation))
# Fit a quadratic regression model


# c)

# Get the data for Germany
Germany_A_Data <- subset(Air_Transport_Data, Countries == "Germany")[-1]
Germany_B_Data <- subset(Immigration_Data, Countries == "Germany")[-1]
Germany_A_Data <- sapply(Germany_A_Data[-1], convert_to_numeric)
Germany_B_Data <- sapply(Germany_B_Data[-1], convert_to_numeric)

countries <- c("Belgium", "Bulgaria", "Czechia", "Denmark", "Estonia", "Ireland", "Greece", "Spain", "France", "Croatia", "Italy", "Cyprus"
,"Latvia", "Lithuania", "Luxembourg", "Hungary", "Malta", "Netherlands", "Austria", "Poland", "Portugal", "Romania", "Slovenia", "Slovakia", "Finland",
               "Sweden", "Iceland", "Norway", "Switzerland")




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

