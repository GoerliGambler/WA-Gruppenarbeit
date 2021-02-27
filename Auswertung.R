## Auswertung

source("Funktionen.R")
Datensatz <- read.csv("Datensatz.csv")[,-1]



## Zusammenhang zwischen Studienfach und Interesse am Programmieren

katBivStats(Datensatz$studienfach, as.factor(Datensatz$programmieren))

# Der Cramer V Koeffizient zeigt einen mittel-groSen Zusammenhang zwischen 
# Studienfach und dem Interesse am Programmieren

# Die Studierenden der Data Science liegen etwa im mittleren Bereich, 
# die Informatik-Studierenden gaben ein eher groSes, und die Statistik- 
# und Mathematik-Studierenden ein geringeres Interesse an.



## Zusammenhang zwischen Studienfach und Interesse an Mathematik

katBivStats(Datensatz$studienfach, as.factor(Datensatz$interesseMathe))

# Der Cramer V Koeffizient spricht fuer einen moderaten Zusammenhang
# zwischen dem Studienfach und dem Interesse an Mathematik

# Das Interesse bei den Data Scientists ist liegt jeweils im mittleren Bereich,
# die Informatik-Studierenden tendieren eher zu groeSerem, und die Statistik-Studierenden
# zu geringerem Interesse; die Mathematik-Studierenden gaben jeweils ein
# eher geringes Inzeresse an


## Zusammenhang zwischen Interesse an Mathematik und Programmieren

katBivStats(Datensatz$programmieren, Datensatz$interesseMathe)

# Der Cramer V Koeffizient zeigt einen mittel-kleinen Zusammenhang an, 
# das Interesse an Mathematik und Programmierung hat hier einen leichten 
# positiven Zusammenhang

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
