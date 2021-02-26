## Auswertung


Datensatz <- read.csv("Datensatz.csv")[,-1]



## Zusammenhang zwischen Studienfach und Interesse am Programmieren

katBivStats(Datensatz$studienfach, as.factor(Datensatz$programmieren))

# Der Cramer V Koeffizient zeigt einen mittel-großen Zusammenhang zwischen 
# Studienfach und dem Interesse am Programmieren

# Die Studierenden der Data Science liegen etwa im mittleren Bereich, 
# die Informatik-Studierenden gaben ein eher großes, und die Statistik- 
# und Mathematik-Studierenden ein geringeres Interesse an.



## Zusammenhang zwischen Studienfach und Interesse an Mathematik

katBivStats(Datensatz$studienfach, as.factor(Datensatz$interesseMathe))

# Der Cramer V Koeffizient spricht für einen moderaten Zusammenhang
# zwischen dem Studienfach und dem Interesse an Mathematik

# Das Interesse bei den Data Scientists ist liegt jeweils im mittleren Bereich,
# die Informatik-Studierenden tendieren eher zu größerem, und die Statistik-Studierenden
# zu geringerem Interesse; die Mathematik-Studierenden gaben jeweils ein
# eher geringes Inzeresse an


## Zusammenhang zwischen Interesse an Mathematik und Programmieren

katBivStats(Datensatz$programmieren, Datensatz$interesseMathe)

# Der Cramer V Koeffizient zeigt einen mittel-kleinen Zusammenhang an, 
# das Interesse an Mathematik und Programmierung hat hier einen leichten 
# positiven Zusammenhang

