Ausf�hrungsschritt:

1.Daten schon in working space importiert bzw. list:dataman besteht in working space.

2.Befehl eingeben:
"library(shiny)
library(leaflet)
library(sp)
library(dplyr)
library(readxl)
library(shinydashboard)
library(data.table)
library(geosphere)
library(ggplot2)"
(notwendige bibliothek laden)

3.Befehl eingeben:
"rm(akk)"
(f�r jeder Durchf�hrung die Ergebnislist akkualisieren)

4.Befehl eingeben:
runApp("Name der App-Ordner")

5.Durch Slider zu einem Zeitpunkt gehen und Egofahrzeug durch Mausklick in Map w�hlen.

6.der Slider laufen lassen.
Bemerkung:
1.Wenn die Egofahrzeug allein in einer Richtung fahren,dann kann die nicht gew�hlt werden.(Keine andere Objektfahrzeug,deshalb keine Ann�herungsfahrt)

2.Wenn die Egofahrzeug verschwindt,dann wird die app auch zu.


 
  