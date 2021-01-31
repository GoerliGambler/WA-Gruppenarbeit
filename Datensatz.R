#Datensatz

id = 1:100

# Variable Alter

alter = rnorm( n=100 , mean=25 , sd=2 )

#Variable Studienfach
fächer = c("Statistik", "Data Science", "Mathe", "Informatik")
studienfach = sample(fächer, 100, replace = TRUE, prob = c(0.3,0.3,0.15,0.25))

datensatz = data.frame( id, alter, studienfach)
