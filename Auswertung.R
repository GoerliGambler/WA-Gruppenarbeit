## Auswertung

source("Funktionen.R")
Datensatz <- read.csv("Datensatz.csv")[,-1]


### mit c)

## Zusammenhang zwischen Studienfach und Interesse am Programmieren

katBivStats(Datensatz$studienfach, as.factor(Datensatz$programmieren))

# Der Cramer V Koeffizient zeigt einen mittel-grossen Zusammenhang zwischen 
# Studienfach und dem Interesse am Programmieren

# Die Studierenden der Data Science liegen etwa im mittleren Bereich, 
# die Informatik-Studierenden gaben ein eher grosses, und die Statistik- 
# und Mathematik-Studierenden ein geringeres Interesse an.



## Zusammenhang zwischen Studienfach und Interesse an Mathematik

katBivStats(Datensatz$studienfach, as.factor(Datensatz$interesseMathe))

# Der Cramer V Koeffizient spricht fuer einen moderaten Zusammenhang
# zwischen dem Studienfach und dem Interesse an Mathematik

# Das Interesse bei den Data Scientists ist liegt jeweils im mittleren Bereich,
# die Informatik-Studierenden tendieren eher zu groesserem, und die Statistik-Studierenden
# zu geringerem Interesse; die Mathematik-Studierenden gaben jeweils ein
# eher geringes Interesse an


## Zusammenhang zwischen Interesse an Mathematik und Programmieren

katBivStats(Datensatz$programmieren, Datensatz$interesseMathe)

# Der Cramer V Koeffizient zeigt einen mittel-kleinen Zusammenhang an, 
# das Interesse an Mathematik und Programmierung hat hier einen leichten 
# positiven Zusammenhang

#-------------------------------------#

### mit d)

## Zusammenhang zwischen Alter und Mathe-LK

bivStat(Datensatz$matheLk, Datensatz$alter)

# Sowohl Korrelation als auch Kovarianz liegen im niedrigen positiven Bereich.
# Es gibt also nur einen leichten Zusammenhang. Diejenigen Studierenden die 
# in der Schule einen Mathe-LK hatten, sind eher etwas juenger als die,
# die keinen Mathe-LK hatten
# Wie die Summary zeigt, hat das Alter der LK-Gruppe eine groessere Range,
# bei der anderen Gruppe streut es aber etwas mehr

#-------------------------------------#
### Mit f)

vis_kat1(Datensatz)
# Wir sehen dass das Durchschnittsalter der Data Scientists und Statistiker
# am hoechsten ist. Die Spannweite der Mathematiker und Informatiker 
# hingegen ist groeSer, aber im Mittel (Median) kleiner.

vis_kat2(Datensatz)
# Wir sehen dass die Statistiker und Mathematiker im kleinen Bereich
# von 1 - 3 sich aufhalten. Waehrend sich die Data Scientists im Mittelfeld
# befinden ( 3-4 ). Die Informatiker hingegen haben die groeSte Streuung
# mit einem Median von 6.

vis_kat3(Datensatz)
# Wir sehen dass das Interesse an Mathematik in allen 4 Gruppen gut ausgepraegt
# ist. Die Mathematiker sind mit den Statistikern leicht fuehrend.
# Erwaehnenswert ist, dass es Mathematiker gibt mit wenig interesse
# an Mathematik da Spannweite zw. 1-3.
Mathematiker = Datensatz[Datensatz$studienfach == "Mathe" ,]
length(Mathematiker)
# [1] 6 
# Wir scheinen also eine kleine Stichprobe (n=6) von Mathematikern zu haben.
Mathematiker$interesseMathe
# [1] 3 1 1 2 3 3
# Wir scheinen tatsaechlich nur wenige Mathematiker zu haben, welche zudem 
# auch noch alle uninteressiert an Mathematik sind! 

#-------------------------------------#

# mit b)
# Stabdiagramm
barplot(table(Datensatz$studienfach)[1:4], main = "Studienfächer",
        names.arg = c("Data Science", "Informatik", "Mathe",
                     "Statistik"), las=1, xlab = "Studienfaecher",
        ylab = "absolute Haeufigkeit", col = "green")
# Das Stabdiagramm gibt einen ersten Überblick über die Verteilung der Fächer. Für die konkreten Zahlen rechnen wir weiter:
# absolute Häufigkeiten  
DataStud <- sum(Datensatz$studienfach == "Data Science")
InfoStud <- sum(Datensatz$studienfach == "Informatik")
MathStud <- sum(Datensatz$studienfach == "Mathe")
StatStud <- sum(Datensatz$studienfach == "Statistik")
# Modalwert
c("DataStud","InfoStud", "MathStud","StatStud")[which.max(c(DataStud,InfoStud, MathStud,StatStud))]
InfoStud
#Der Modalwert ist Infostud, also die Informatikstudenten. Dieser beträgt mit 34 gut ein drittel aller Beobachtungen.
# Minimum
c("DataStud","InfoStud", "MathStud","StatStud")[which.min(c(DataStud,InfoStud, MathStud,StatStud))]
MathStud
#Das Minimum ist Mathstud, also die Mathematikstudenten. Diese betragen bloß 6 Studenten und liegen somit weit hinter den anderen Studienfächer.
DataStud
StatStud
# Die anderen beiden Studiengänge liegen mit Data Science bei 27 und den Statistikstudenten bei 33 Studenten zwischen den beiden Extremwerten.
# Dabei zeigt das Stabdiagramm, dass es sich um fast gleichviele Infomatik- und Statistikstudenten handelt.
