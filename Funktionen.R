# Verwendete Bibliotheken
library("moments")

# Datei einlesen
Datensatz <- read.csv("Datensatz.csv")[,-1]

# (a) Eine Funktion, die verschiedene geeignete deskriptive Statistiken
# fuer metrische Variablen berechnet und ausgibt

descStatMet = function(metricValue) {
  # Lagemaße
  mv_quantile = quantile(metricValue)
  mv_mean     = mean(metricValue)
  names(mv_quantile) = c("q0.0", "q0.25", "q0.5", "q0.75", "q1.0")
  names(mv_mean)     = "mean"
  
  # Streuungsmaße
  mv_range = mv_quantile[5] - mv_quantile[1]
  mv_iqr   = IQR(metricValue)
  mv_sd    = sd(metricValue)
  names(mv_range) = "range"
  names(mv_iqr)   = "iqr"
  names(mv_sd)    = "sd"
  
  # Höhere Momente
  mv_skewness = skewness(metricValue)
  mv_kurtosis = kurtosis(metricValue)
  names(mv_skewness) = "skewness"
  names(mv_kurtosis) = "kurtosis"
  
  # Ergebnis bestimmen
  mv_result = c(mv_quantile, mv_mean, mv_range, mv_iqr, mv_sd,
                mv_skewness, mv_kurtosis)
  
  # hist(Datensatz$alter, main="Histogramm von Alter", xlab = "Alter in Jahren", ylab =  "absolute Haeufigkeit")
  # boxplot(Datensatz$alter, main="Altersverteilung", horizontal=TRUE)
  
  return(mv_result)
}

test = descStatMet(Datensatz$alter)



# (b) Eine Funktion, die verschiedene geeignete deskriptive Statistiken
# fuer kategoriale Variablen berechnet und ausgibt


# (c) Eine Funktion, die geeignete deskriptive bivariate Statistiken fuer
# den Zusammenhang zwischen zwei kategorialen Variablen
# berechnet ausgibt


# (d) Eine Funktion, die geeignete deskriptive bivariate Statistiken fuer
# den Zusammengang zwischen einer metrischen und einer
# dichotomen Variablen berechnet und ausgibt


# bivStats - Berechnet Kennzahlen, um Zusammenhang zwischen einer dichotomen und
#            einer metrischen Variable zu verdeutlichen
#            
#Input: dicho - Auspraegungen der dichotomen Variable (numerischer Vektor)
#       metri - Auspraegungen der metrischen Variable (numerischer Vektor)
#
#Output: benannte Liste: summary - Matrix mit Summarys der, nach der dichotomen
#                                  Variable kategorisierten, metrischen Variable
#                                  (numerische Matrix)
#                       correlation - Pearson-Korrelations-koeffizient zwischen 
#                                     den Variablen (numerisch)
#                       covariance - Kovarianz zwischen den Variablen (numerisch)

bivStat = function(dicho, metri){
  cNames = levels(as.factor(dicho))
  #"Zuteilen" der Werte der metrischen Variable zu den Auspraegungen der 
  #dichotomen Variable
  dicho1 = dicho[1]
  cName1 = cNames[cNames == dicho1]
  metri1 = metri[dicho == dicho1]
  cName2 = cNames[cNames != dicho1]
  metri2 = metri[dicho != dicho1]
  
  #Summary fuer Zuordnungen erstellen und in Matrix abspeichern
  summary1 = summary(metri1)
  summary2 = summary(metri2)
  summ = cbind(summary1, summary2)
  colnames(summ) = c(cName1, cName2)
  
  #Variablen auf Korrelation und Covarianz untersuchen (jeweils mit Pearson)
  dicho = as.numeric(dicho == dicho1)
  corr = cor(dicho, metri, method = "pearson")
  cova = cov(dicho, metri, method = "pearson")
  
  return(list(summary = summ, correlation = corr, covariance = cova))
}
#Bsp:
bivStat(as.vector.factor(Datensatz$matheLk), Datensatz$alter)



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
