# Verwendete Bibliotheken
library("moments")
library(rcompanion)
library(DescTools)

# Datei einlesen
Datensatz <- read.csv("Datensatz.csv")[,-1]

# (a) Eine Funktion, die verschiedene geeignete deskriptive Statistiken
# fuer metrische Variablen berechnet und ausgibt

descStatMet = function(metricValue) {
  # LagemaÃe
  mv_quantile = quantile(metricValue)
  mv_mean     = mean(metricValue)
  names(mv_quantile) = c("q0.0", "q0.25", "q0.5", "q0.75", "q1.0")
  names(mv_mean)     = "mean"
  
  # StreuungsmaÃe
  mv_range = mv_quantile[5] - mv_quantile[1]
  mv_iqr   = IQR(metricValue)
  mv_sd    = sd(metricValue)
  names(mv_range) = "range"
  names(mv_iqr)   = "iqr"
  names(mv_sd)    = "sd"
  
  # HÃ¶here Momente
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

# Kategoriale Variabeln werden gut durch Barplots wiedergegeben. 
# Minimum und Modalwert sind brauchbare KenngroeSen
# die Wahl der Studienfaecher: 
# Stabdiagramm
barplot(table(Datensatz$studienfach)[1:4], main = "Studienfaecher",
        names.arg = c("Data Science", "Informatik", "Mathe",
                     "Statistik"), las=1, xlab = "Studienfaecher",
        ylab = "absolute Haeufigkeit", col = "green")
# absolute Haeufigkeiten  
DataStud <- sum(Datensatz$studienfach == "Data Science")
InfoStud <- sum(Datensatz$studienfach == "Informatik")
MathStud <- sum(Datensatz$studienfach == "Mathe")
StatStud <- sum(Datensatz$studienfach == "Statistik")
# Modalwert
c("DataStud","InfoStud", "MathStud","StatStud")[which.max(c(DataStud,InfoStud, MathStud,StatStud))]
# Minimum
c("DataStud","InfoStud", "MathStud","StatStud")[which.min(c(DataStud,InfoStud, MathStud,StatStud))]

# (c) Eine Funktion, die geeignete deskriptive bivariate Statistiken fuer
# den Zusammenhang zwischen zwei kategorialen Variablen
# berechnet ausgibt

# katBivStats - Berechnet Kennzahlen fÃ¼r den Zusammenhang zwischen
# zwei kategorialen Variablen

# Input: katx, katy - jeweils Auspraegungen einer kategorialen Variable
#
# Output: benannte Liste: tabelle - Kontingenztabelle der Faktoren
#                         cramerV - Kontingenzkoeffizient nach Cramer
#

katBivStats <- function(katx, katy){
  
  # Kontingenztafel
  kreuztab <- table(katx, katy)
  
  # Cramers Kontingenzindex (Cramer's V)
  cramerV <- cramerV(kreuztab)
  
  return(list(tabelle = kreuztab, cramerV = cramerV))
  
}


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

# Uebersicht
head(Datensatz)

# Die Spalte "studienfach" scheint sich am besten fuers Gruppieren zu eignen. 
table(Datensatz$studienfach)
# Wir haben also 4 Gruppen. 

# Diese speichern wir jeweils in eine Gruppenvariable
gruppeDS <- Datensatz[Datensatz$studienfach == "Data Science" ,]
gruppeST <- Datensatz[Datensatz$studienfach == "Statistik" ,] 
gruppeMA <- Datensatz[Datensatz$studienfach == "Mathe" ,]
gruppeIN <- Datensatz[Datensatz$studienfach == "Informatik" ,]


ncol(Datensatz)
# Wir sehen dass wir 6 Spalten haben.
# Unsere zu interessierene kategoriale Variable werden 
# "alter" (=Alter), 
# "programmieren" (=Programmierkenntnisse von 1-10),
# "interesseMathe (=Interesse an Mathematik von 1-10)
# sein.

# Da einzelnt abgerufen werden 3 Funktionen erstellt.

# Funktion vis_kat(1-3)    : Visualisiert kategoriale Variablen
# Eingabe : dataset : Ein Dataframe/Character Variable oder Liste mit 
#                     kategorialen Daten
# Ausgabe : Visualierte Darstellung der 4 Gruppen in Boxplots

# vis_kat1 = Alter
vis_kat1 <- function(dataset){
  
  # Neues Fenster
  par(mfrow = c(1,4))
  
  # Durchschnittsalter
  boxplot(x = gruppeDS$alter, xlab = "Data Scienctists", ylab = "Alter")
  boxplot(x = gruppeST$alter, xlab = "Statistiker", ylab = "Alter")
  boxplot(x = gruppeMA$alter, xlab = "Mathematiker", ylab = "Alter")
  boxplot(x = gruppeIN$alter, xlab = "Informatik", ylab = "Alter")
}

# vis_kat2 :  Programmierkenntnisse
vis_kat2 <- function(dataset){
  
  # Neues Fenster
  par(mfrow = c(1,4))
  
  boxplot(x = gruppeDS$programmieren, xlab = "Data Scienctists", 
          ylab = "Programmierkenntnisse")
  boxplot(x = gruppeST$programmieren, xlab = "Statistiker", 
          ylab = "Programmierkenntnisse")
  boxplot(x = gruppeMA$programmieren, xlab = "Mathematiker", 
          ylab = "Programmierkenntnisse")
  boxplot(x = gruppeIN$programmieren, xlab = "Informatik", 
          ylab = "Programmierkenntnisse")
}

# vis_kat3  : Intereses an Mathematik
vis_kat3 <- function(dataset){
  
  # Neues Fenster
  par(mfrow = c(1,4))
  
  boxplot(x = gruppeDS$interesseMathe, xlab = "Data Scienctists", 
          ylab = "Interesse an Mathematik")
  boxplot(x = gruppeST$interesseMathe, xlab = "Statistiker", 
          ylab = "Interesse an Mathematik")
  boxplot(x = gruppeMA$interesseMathe, xlab = "Mathematiker", 
          ylab = "Interesse an Mathematik")
  boxplot(x = gruppeIN$interesseMathe, xlab = "Informatik", 
          ylab = "Interesse an Mathematik")
}

# Direktausfuehrung als Test:
# vis_kat1(Datensatz)
# vis_kat2(Datensatz)
# vis_kat3(Datensatz)


# Freiwillig: weitere zur Deskription und Visualisierung geeignete
# Funktionen
