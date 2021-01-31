Datensatz

id = 1:100

# Variable Alter

alter = rnorm( n=100 , mean=25 , sd=2 )

#Variable Studienfach
fächer = c("Statistik", "Data Science", "Mathe", "Informatik")
studienfach = sample(fächer, 100, replace = TRUE, prob = c(0.3,0.3,0.15,0.25))

#Variable "Interesse an Programmieren"

programmieren = c()
for (i in 1:100){
  if(studienfach[i] == "Statistik" | studienfach[i] == "Mathe"){
    programmieren[i] = sample(1:3 ,1, prob = c(0.25 , 0.5, 0.25))
  }
  if(studienfach[i] == "Data Science"){
    programmieren[i] = sample(3:5, 1, prob = c(0.25 , 0.5, 0.25))
  }
  if(studienfach[i] == "Informatik"){
    programmieren[i] = sample(5:7, 1, prob = c(0.25 , 0.5, 0.25))
  }
}

datensatz = data.frame( id, alter, studienfach, programmieren)

# Interesse Mathe

mathe = function( x ){
  res = c()
  for( i in 1:length(x)){
    if( studienfach[i] == "Statistik"){ res[i] = sample(c(2:5),1)}
    if( studienfach[i] == "Data Science"){ res[i] = sample(c(3:5),1)}
    if( studienfach[i] == "Mathe"){ res[i] = sample(c(1:3),1)}
    if( studienfach[i] == "Informatik"){ res[i] = sample(c(3:7),1)}
  }
  return( res )
}

interesseMathe = mathe( c(1:100) )

datensatz = data.frame( id, alter, studienfach, programmieren, interesseMathe)
