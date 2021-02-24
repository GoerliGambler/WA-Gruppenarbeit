## Hilfsfunktionen
# Funktionen die ggf. von den Hauptfunktionen als Hilfe benutzt werden koennen

# 1. Hilfsfunktion
# Funktino Daten_str : Gibt allgemeine Daten ueber die Datenstruktur 
#                      des Objekts aus
# Eingabe : data - Objekt
# Ausgabe : Liste mit (1)Struktur (2)Klasse und (3) Datentyp 
#           des uebergebenen Arguments/Objekts
Daten_str <- function(data){
  Liste <- list(Struktur = str(data), 
                Klasse = class(data),
                Datentyp = typeof(data))
  return(Liste)
}
