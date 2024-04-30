#Aufgabe 09
# a)
load("Data/SPECTF.RData")
# Überprüfen Sie, welche Objekte geladen wurden
objects <- ls()
#print(objects)

# Überprüfen Sie die Dimensionen des SPECTF-Datensatzes
SPECTF_Dimensions <- dim(SPECTF)
#print(SPECTF_Dimensions)

# b)
# Calculate the range (Maximum - Minimum) for each parameter
spannweite <- apply(SPECTF[, -1], 2, function(x) max(x, na.rm = TRUE) - min(x, na.rm = TRUE))

# View the summary of the ranges
summary_spannweite <- summary(spannweite)
print(summary_spannweite)

# Create a histogram of the ranges
hist(spannweite, main = "Histogramm der Spannweiten", xlab = "Spannweite")

# c)
varianz <- apply(SPECTF[, -1], 2, var, na.rm = TRUE)

hoechste_varianz <- sort(varianz, decreasing = TRUE)[1:6]

par(mfrow = c(2, 3))
for(param in names(hoechste_varianz)) {
  boxplot(SPECTF[[param]], main = param, ylab = "Wert")
}
