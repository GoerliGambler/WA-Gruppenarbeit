# Verwendete Bibliotheken
library("moments")

# Datei einlesen
Datensatz <- read.csv("Datensatz.csv")

# (a) Eine Funktion, die verschiedene geeignete deskriptive Statistiken
# fuer metrische Variablen berechnet und ausgibt

# Merkmal alter
alter_quantile <- quantile(Datensatz$alter)
alter_mean <- mean(Datensatz$alter)

alter_range <- range(Datensatz$alter)
alter_iqr <- IQR(Datensatz$alter)
alter_sd <- sd(Datensatz$alter)

alter_skewness <- skewness(Datensatz$alter)
alter_kurtosis <- kurtosis(Datensatz$alter)

hist(Datensatz$alter, main="Histogramm von Alter", xlab = "Alter in Jahren", ylab =  "absolute Haeufigkeit")
boxplot(Datensatz$alter, main="Altersverteilung", horizontal=TRUE)

# (b) Eine Funktion, die verschiedene geeignete deskriptive Statistiken
# fuer kategoriale Variablen berechnet und ausgibt


# (c) Eine Funktion, die geeignete deskriptive bivariate Statistiken fuer
# den Zusammenhang zwischen zwei kategorialen Variablen
# berechnet ausgibt


# (d) Eine Funktion, die geeignete deskriptive bivariate Statistiken fuer
# den Zusammengang zwischen einer metrischen und einer
# dichotomen Variablen berechnet und ausgibt


# (e) Eine Funktion, die eine mindestens ordinal skalierte Variable
# quantilbasiert kategorisiert (z.B. in "niedrig", "mittel", "hoch")



kategorisierung = function( x ){
  niedrig = 0
  mittel = 0
  hoch = 0
  
  for( i in 1:100){
    if( x[i] == 1  | x[i] == 2){ niedrig = niedrig + 1}
    if( x[i] == 3  | x[i] == 4 | x[i] == 5){ mittel = mittel + 1}
    if( x[i] == 6  | x[i] == 7){ hoch = hoch + 1}
  }
  return( c( niedrig,  mittel, hoch) )
}

katMathe = Datensatz$interesseMathe

barplot( kategorisierung(katMathe), xlab = "Interesse Mathe niedrig, mittel,hoch", 
         ylab = "Anzahl")

katProg = Datensatz$programmieren

barplot( kategorisierung(katProg), xlab = "Interesse Programmieren niedrig, mittel,hoch", 
         ylab = "Anzahl")

# (f) Eine Funktion, die eine geeignete Visualisierung von drei oder vier
# kategorialen Variablen erstellt


# Freiwillig: weitere zur Deskription und Visualisierung geeignete
# Funktionen
