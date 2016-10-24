# ---------------------------------------------------
# SurSauD
# ---------------------------------------------------




###-------- Chargement ds packages complementaires ----------------------###
library( shiny )
library( googleVis )
library( shinyjs )
library( DT )
library( data.table )
library( dygraphs )
library( d3heatmap )

Liste_RS  <- "Syndrome grippal" #c("--Vous pourrez bientôt choisir un RS--" )

List_RS_short  <- c("Syndrome grippal", "Gastro-enterite", "Bronchiolite", "Bronchite", "Pneumopathie", "Infection respiratoire aigue basse")

dayrange <- fread( "Data/Donnees_Sursaud_Sentinelles/dayrange.txt", encoding="UTF-8" )

# Proposer comme dernier jour par défaut le dernier dimanche (demande des Cires)
dayofweek <- as.numeric( strftime( dayrange$last, "%w") ); dayofweek
lastsunday <- as.Date( dayrange$last ) - dayofweek


endingtime <- fread( "Data/Donnees_Sursaud_Sentinelles/endingtime.txt", encoding="UTF-8" )

