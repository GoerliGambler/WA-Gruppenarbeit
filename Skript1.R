# Verwendete Bibliotheken
library("moments")

# Datei einlesen
Datensatz <- read.csv("Datensatz.csv")

# (a) Eine Funktion, die verschiedene geeignete deskriptive Statistiken
# für metrische Variablen berechnet und ausgibt

# Merkmal alter
alter_quantile <- quantile(Datensatz$alter)
alter_mean <- mean(Datensatz$alter)

alter_range <- range(Datensatz$alter)
alter_iqr <- IQR(Datensatz$alter)
alter_sd <- sd(Datensatz$alter)

alter_skewness <- skewness(Datensatz$alter)
alter_kurtosis <- kurtosis(Datensatz$alter)

hist(Datensatz$alter, main="Histogramm von Alter", xlab = "Alter in Jahren", ylab =  "absolute Häufigkeit")
boxplot(Datensatz$alter, main="Altersverteilung", horizontal=TRUE)

# (b) Eine Funktion, die verschiedene geeignete deskriptive Statistiken
# für kategoriale Variablen berechnet und ausgibt


# (c) Eine Funktion, die geeignete deskriptive bivariate Statistiken für
# den Zusammenhang zwischen zwei kategorialen Variablen
# berechnet ausgibt


# (d) Eine Funktion, die geeignete deskriptive bivariate Statistiken für
# den Zusammengang zwischen einer metrischen und einer
# dichotomen Variablen berechnet und ausgibt


# (e) Eine Funktion, die eine mindestens ordinal skalierte Variable
# quantilbasiert kategorisiert (z.B. in "niedrig", "mittel", "hoch")


# (f) Eine Funktion, die eine geeignete Visualisierung von drei oder vier
# kategorialen Variablen erstellt


# Freiwillig: weitere zur Deskription und Visualisierung geeignete
# Funktionen