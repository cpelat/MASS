# ----------------------------------------------------------------------------------------------------
# ----------------------------------------------------------------------------------------------------
# Programme SERVER
# ----------------------------------------------------------------------------------------------------
# ----------------------------------------------------------------------------------------------------

library( maptools  )
library( xts )



###--------------------Création de quelques fonctions utiles dans server.R -----------------------###

# Renvoie le nom de la région
getname.region <- function( Region ){
  res <-  correspondance_nomreg_codereg$Nom_region[ match(Region, correspondance_nomreg_codereg$Region) ]
  return( ifelse( is.na(res), Region, res ) )
}


# Capitalize the first letter
cap <- function( s ) {
  paste( toupper( substring(s, 1, 1) ),  substring(s, 2), sep="" )
}

# Uncapitaize the first letter
uncap <- function( s ) {
  paste( tolower( substring(s, 1, 1) ),  substring(s, 2), sep="" )
}


gvisMerge2 <- function( x, y ){
  gvisMerge(x, y, horizontal=T, tableOptions = "class=TableGauge" ) #  
}


addGvisATLTitle <- function(gvisATL,title) {
  if (!all(class(gvisATL) == c("gvis","list"))) {
    stop('ERROR in addGvisATLTitle: Incorrect type, expect gvisAnnotatedTimeLine.')
  }
  if (class(title) == "character") {
    gvisATL$html$chart['divChart'] <- paste(title,gvisATL$html$chart['divChart'],sep="")
  } else if (class(title) == "shiny.tag") {
    gvisATL$html$chart['divChart'] <- paste(as.character(title)[1],gvisATL$html$chart['divChart'],sep="")
  } else {
    stop('ERROR in addGvisATLTitle: Unknown title type.')
  }
  return(gvisATL)
}


# Représentation graphique
add_shades <- function(x, periods, ...) {
  for( period in periods ) {
    x <- dyShading(x, from = period$from, to = period$to, ... )
  }
  x
}


# Looking for epidemic periods - only for weekly data
# dd : data.table
# epid : character, nom de la colonne contenant le statut épidéique 0/1
search.epidemic.period <- function( dd, epid="alarme_Serfling" ) {
  TMP <- dd[ dd[[ (epid) ]]==1, ]$d
  Periode <- rep(0,length(TMP))
  Periode[1] <- 1
  
  # If more than 1 epidemic week, looking for the different epidemic periods
  if( length(TMP)>1 ){
    for (i in 2:length(TMP)) {
      # If epidemic week i is next in time to epidemic week i-1
      if (TMP[i]==TMP[i-1]+7) {
        Periode[i] <- Periode[i-1]
      }
      else Periode[i] <- Periode[i-1]+1
    }
  }
  
  bigLL <- list()
  for( i in 1:length(unique(Periode)) ) {
    bigLL[[i]] <- list(from=min(TMP[Periode==i]),to=max(TMP[Periode==i]+7))
  }
  
  return( bigLL )
}


# Styling function for text output (strong blue character strings)
hh <- function( text ){
  HTML('<span style="color:#181866; font-weight: bold;">', text, '</span>' )
}






###-------- Chargement des données pour toutes les sessions ----------------------###


# Label des noms de region
correspondance_nomreg_codereg <- fread( "Data/correspondance_codereg_nomreg.csv", encoding="UTF-8", colClasses="character")
setkey( correspondance_nomreg_codereg, Region )

###### Fichier des seuils pour les couleurs des cartes d'intensité
seuils_carte <- fread( "Data/seuils_cartes.csv", encoding="UTF-8")


############## Fonds de carte ##############
mapdir <- "Data/Fonds_de_carte/France_DOM_Mayotte_StBarth_StMartin_Regions"

# Fonds de carte Anciennes regions
map.fr_regionsA <- readShapePoly( paste( mapdir, "Region_France_DOM_2012_tt", sep='/') )
centro_regionsA <- readShapePoints( paste( mapdir, "XYRegion_DOM_2012_tt", sep='/') )


# Fonds de carte Nouvelles regions
map.fr_regionsN <- readShapePoly( paste( mapdir, "New_Regions_DOM", sep='/') )
centro_regionsN <- readShapePoints( paste( mapdir, "XYNew_Region_DOM", sep='/') )

# Couleur des proportions
#coul.prop <-c( "#fef0d9", "#fdcc8a", "#fc8d59", "#d7301f" )
#coul.prop <-c( "#eff3ff", "#bdd7e7", "#6baed6", "#2171b5" ) #
coul.prop <-c( "#bdd7e7","#6baed6", "#3182bd", "#08519c" )

# Couleur des alarmes
coul.alarme <- c("#19BA00", "#FF9900", "#FF0000" ) #c("green", "orange", "red") #2ca25f c("#a1d99b", "#feb24c", "#de2d26" )
# rgb(255,191,0, max = 255)

# Couleur des jauges
coul.jauge <- c( "#efedf5", "#bcbddc", "#756bb1" )

# Couleur des heatmap
coul.heatmap <- c( coul.alarme, "#808080" )

# Label des unites
typenom <- c("SAU", "Sentinelles", "SOS")
typeacte <- c( "visits", "consultations", "consultations" )
typedeno <- c( "10,000 emergency room visits", "100,000 inhabitants", "10,000 SOS Médecins consultations" )  
names( typeacte ) <- typenom
names( typedeno ) <- typenom



######  Fichiers de correspondance code INSEE de region - numero region POUR LES REGIONS ANCIENNES   
corresp_nom_num_regionsA <- fread("Data/correspondance_coderegold_numregold_pour_carte.csv", encoding="UTF-8", colClasses=list(character=1) )
corresp_nom_num_regionsN <- fread("Data/correspondance_coderegnew_numregnew_pour_carte.csv", encoding="UTF-8", colClasses=list(character=1))



########## Chargement des données de surveillance  ##########

# Lectures des données hebdo  
Data_week <- readRDS( "Data/Donnees_Sursaud_Sentinelles/Data_week.rds" )
#saveRDS( Data_week[ d>as.Date("2016-01-01") ], "Data/Donnees_Sursaud_Sentinelles/Data_week.rds" )
Data_week[ d > lastsunday, Dates:=paste(Dates, "INCOMPLETE")]

# Mise à jour des listes de choix des champs selects, lues dans la base hebdo
Liste_RS  <- levels( Data_week$RS )
#write.csv2( data.frame( key=Liste_RS, fr=Liste_RS, en=Liste_RS), file="liste_rs.csv",quote = F, row.names=F )

Liste_regionsA <- unique( Data_week$Region[ Data_week$Region %in% corresp_nom_num_regionsA$code_reg ] ) 
names( Liste_regionsA ) <- getname.region( Liste_regionsA ) 
Liste_regionsA <-  Liste_regionsA[ order(names(Liste_regionsA) ) ] 

Liste_regionsN <- unique( Data_week$Region[ Data_week$Region %in% corresp_nom_num_regionsN$code_reg ] )  
names( Liste_regionsN ) <- getname.region( Liste_regionsN )
Liste_regionsN  <-  Liste_regionsN[ order(names(Liste_regionsN) ) ]  


# List of the possible analyzed zones for the corresponding radio button
List_analyzed_zones <- c("FRANCE", "METROPOLE", "regionsN", "regionsA")

# List of the possible mapped zones for the corresponding radio button
List_mapped_zones <- c("regionsN", "regionsA")


# List of the data source, the methods and the age classes for the select input fields.
List_sources <- sort( unique( Data_week$Source ) )
List_methods <- gsub( "alarme_", "", grep( "alarme_", names(Data_week), fixed=T, value=T ) )
List_ages <- sort( unique( Data_week$Classe_age ) )
#write.csv2( data.frame( age=List_ages ), row.names=F, file="age.csv" )

# Weekly alarm levels for a subset of dieases (winter pathologies)
alarm_level <-  readRDS( "Data/Donnees_Sursaud_Sentinelles/alarm_level.rds" )
alarm_level[ d > lastsunday, Dates:=paste(Dates, "INCOMPLETE")]

# Weekly alarm list for this same subset
alarm_list <- readRDS( "Data/Donnees_Sursaud_Sentinelles/alarm_list.rds" )



# Lectures des données quotidiennes 
#Data_day <- readRDS("Data/Donnees_Sursaud_Sentinelles/Data_day.rds") 

# Lectures des données mensuelles
Data_month <- readRDS("Data/Donnees_Sursaud_Sentinelles/Data_month.rds") 


# Description des variables pour les fichiers téléchargeables (résultats des méthodes de détection)
descMet <- fread( "Data/downloadMet_description_variables.csv", encoding="UTF-8" )
descNiv <- fread( "Data/downloadNiv_description_variables.csv", encoding="UTF-8" )
descMat <- fread( "Data/downloadMat_description_variables.csv", encoding="UTF-8" )


# Dictionnary dataset for the bilangual site
#lang_analyzedzones <- read.csv2( "Data/lang_analyzedzones.csv", as.is=TRUE, encoding="UTF-8") 
#lang_mappedzones <- read.csv2( "Data/lang_mappedzones.csv", as.is=TRUE, encoding="UTF-8") 
#lang_timeunits <- read.csv2( "Data/lang_timeunits.csv", as.is=TRUE, encoding="UTF-8")  

# List of the time units available for analysis
#List_time_units <- c( "day", "week", "month" )
List_time_units <- c( "week", "month" )

# ----------------------------------------------------------------------------------------------------

shinyServer( function(input, output,session) {
  
  getPlural <- function( text ){
    n <- nchar( text )
    if( substr(text, n, n) != "s" ){
      text <- paste( text, "s", sep="" )    
    }
    return( text )
  }
  
  cat("\n############################\n MASS \n #######################\n")
  
  output$sessioninfo <- renderUI({
    if(!is.null(input$packages)) sapply(input$packages, function(x) eval(parse(text=paste0("library(",x,")"))))
    includeRmd("SessionInfo.Rmd")
  })
  
  

  ############ Mise à jour de l'UI Regroupement syndromique en fonction des éléments de la bdd
  #updateSelectizeInput( session, "RS", choices = getNamedList2( Liste_RS ), selected="Syndrome grippal" )
  updateSelectizeInput( session, "RegionA", choices = Liste_regionsA ) 
  updateSelectizeInput( session, "RegionN", choices = Liste_regionsN ) 
  
  observe({
    updateRadioButtons(session, "TimeUnit", choices=getNamedList2( List_time_units ), selected=input$TimeUnit, inline=T )
    if( input$menu=="determination" ) {
      cat( "\ninput$RS:", input$RS, "\n")
      updateSelectizeInput( session, "RS", choices = getNamedList2( List_RS_short ), selected=ifelse( is.null(input$RS), "Syndrome grippal", input$RS ) ) #"Syndrome grippal"
    } else {
      cat( "\ninput$RS:", input$RS, "\n")
      updateSelectizeInput( session, "RS", choices = getNamedList2( Liste_RS ), selected=ifelse( is.null(input$RS), "Syndrome grippal", input$RS )  )  #"Syndrome grippal"
    }
      
  })
  

  
  #~~~~~~~~~~~~~~ UI that depend on the language chosen ~~~~~~~~~~~~~~
  lang <- read.csv2( "Data/lang.csv", as.is=TRUE, row.names=1, encoding="UTF-8")  
  
  # Translates to french and capitalize
  toCapfrench <- function( text ){ 
    return( cap( lang[ text, "fr"] ) ) 
  } 
  
  # Creation of the vector of reactive values
  values <- reactiveValues( language="en" )  
  
  onclick( "fr_flag", expr={ 
    values$language <- "en" 
    #updateSelectizeInput( session, "language", selected="en" )
    #info( paste("clicked now", values$language ) )
  })
  
  
  onclick( "en_flag", expr={ 
    values$language <- "fr" 
    #updateSelectizeInput( session, "language", selected="fr" )
    #info( paste("clicked now", values$language ) )
  })
  
  # Function that translates text into current language 
  tr <- function(text){ 
    #return( sapply( text,function(s) lang[s, values$language], USE.NAMES=FALSE ) ) 
    res <- lang[ text, values$language ] #
    res[ is.na(res)] <- text[ is.na(res)]
    return( res ) 
  } 

  # Function that returns a named list from a dataset and a chosen language 
  getNamedList <- function( d ){ 
    l <- as.list( d$key )
    names( l ) <- d[ , values$language ]
    return( l ) 
  } 
  
  # Function that returns a named list from an input list, using the main dictionnary 
  getNamedList2 <- function( l, d=lang ){ 
    ll <- as.list( l )
    names( ll ) <-  d[ l, values$language ]
    names( ll )[ is.na(names( ll )) ] <- l[ is.na(names( ll )) ]
    return( ll ) 
  } 

  # Function that calculates the day range of a week
  # @d: date
  getDayRange <- function( d ){
    if( values$language == "fr" ){
      dayrange <- paste( "du", format( d, format="%d/%m/%Y" ), "au", format( d+6, format="%d/%m/%Y" ) )
    } else {
      dayrange <- paste( "from", format( d, format="%Y-%m-%d" ), "to", format( d+6, format="%Y-%m-%d" ) )
    }
    return( dayrange )
  }

  output$uiSBtitre <- renderUI({ 
    HTML("<img src='img/logo_SPF.png'>", 
          paste( '<span style="font-family:Trebuchet MS; color: #181866;">',
          tr( "titre" ), '</span>', sep="") )
  }) 
  
  
  output$uiSBlang_message <- renderUI({ 
    HTML( 
      paste( '<span style="font-size:11px;">', tr( "lang_message" ), '</span>',
             ifelse( values$language=="fr", 
                     '<span id="fr_flag" class="menu_links"> <img src="img/en.png"> </span>', 
                     '<span id="en_flag" class="menu_links"> <img src="img/fr.png"> </span>'
                    ),
             sep="") 
       )
  }) 

  output$uiSBloading<- renderUI({ 
    HTML( tr( "loading" ), "..." )
  }) 
  
  
  output$uiSBsyndgroup <- renderUI({ 
    HTML( hh( tr( "syndromic grouping" ) ) )
  }) 
  
  output$uiSBlanguage<- renderUI({ 
    hh( tr( "language" ) ) 
  }) 
  
  
  output$uiSBdata_upload<- renderUI({ 
    HTML( tr( "data uploaded on" ), " ", endingtime$date, tr("at"), endingtime$heure, "." )
  }) 
  
  output$uiSBsituation <- renderUI({ 
    HTML( '<i class="fa fa-warning"></i>', tr('situation') )
    })
  
  output$uiSBminimapsAlarm <- renderUI({ 
    HTML( '<i class="fa fa-globe"></i>', tr('alarm maps') )
  })
  
  output$uiSBminimapsProp <- renderUI({ 
    HTML( '<i class="fa fa-globe"></i>', tr('proportion maps') )
  })
  
  output$uiSBalarmList <- renderUI({ 
    HTML( '<i class="fa fa-list-ol"></i>', tr('alarm list') )
  })
  
  
  
  output$uiSBdata <- renderUI({ 
    HTML( '<i class="fa fa-list"></i>', tr('data') )
  })
  
  output$uiSBtimeSeries <- renderUI({ 
    HTML( '<i class="fa fa-bar-chart"></i>', tr('time series') )
  })
  
  output$uiSBtable <- renderUI({ 
    HTML( '<i class="fa fa-table"></i>', tr('table') )
  })
  
  output$uiSBgauges <- renderUI({ 
    HTML( '<i class="fa fa-tachometer"></i>', tr('gauges') )
  })
  
  output$uiSBmaps <- renderUI({ 
    HTML( '<i class="fa fa-globe"></i>', tr('maps') )
  })
  
  output$uiSBcalendar <- renderUI({ 
    HTML( '<i class="fa fa-calendar"></i>', tr('calendar') )
  })
  
  
  
  output$uiSBdetermination <- renderUI({ 
    HTML( '<i class="fa fa-flag"></i>', tr('determination') )
  })
  
  output$uiSBthresholds <- renderUI({ 
    HTML( '<i class="fa fa-bell"></i>', tr('thresholds') )
  })
  
  output$uiSBserfling <- renderUI({ 
    HTML( '<i class="fa fa-bar-chart"></i>', tr('Serfling') )
  })
  
  output$uiSBrobustSerfling <- renderUI({ 
    HTML( '<i class="fa fa-bar-chart"></i>', tr('Serfling_robuste') )
  })
  
  output$uiSBHMM <- renderUI({ 
    HTML( '<i class="fa fa-bar-chart"></i>', tr('HMM') )
  })
  
  
  output$uiSBalarmMatrix <- renderUI({ 
    HTML( '<i class="fa fa-table"></i>', tr('alarm matrix') )
  })
  
  output$uiSBalarmLevelMap <- renderUI({ 
    HTML( '<i class="fa fa-globe"></i>', tr('alarm level map') )
  })
  
  
  
  output$uiSBhelp <- renderUI({ 
    HTML( '<i class="fa fa-question-circle"></i>', tr('help') )
  })
  
  output$uiSBmethods <- renderUI({ 
    HTML( '<i class="fa fa-book"></i>', tr('methods') )
  })
  
  output$uiSBdataResults <- renderUI({ 
    HTML( '<i class="fa fa-info"></i>', tr('results') )
  })
  
  output$uiSBdataDesc <- renderUI({ 
    HTML( '<i class="fa fa-book"></i>', tr('data') )
  })
  
  output$uiSBdetectionResults <- renderUI({ 
    HTML( '<i class="fa fa-bell"></i>', tr('detection method results') )
  })
  
  output$uiSBalarmLevels <- renderUI({ 
    HTML( '<i class="fa fa-exclamation-triangle"></i>', tr('alarm levels') )
  })
  
  output$uiSBalarmMatrixInfo <- renderUI({ 
    HTML( '<i class="fa fa-table"></i>', tr('alarm matrix') )
  })
  
  output$uiSBwhatisnew <- renderUI({ 
    HTML( '<i class="fa fa-lightbulb-o "></i>', tr("what is new") )
  })
  
  
  output$uiSBlinks <- renderUI({ 
    HTML( '<i class="fa fa-external-link-square"></i>', tr('links') )
  })
  
  ##### Download buttons #####
  ### Dectection method results
  output$uiSBdownloadmet_title <- renderUI({
    HTML( hh( tr( 'download title 1' ) ) )
  })
  
  output$uiSBdownloadmet_button <- renderUI({
    downloadButton( 'downloadmet', tr( 'download' )  )
  })
  
  output$uiSBdownloadmet_legend <- renderUI({
    HTML( paste( "<a class='menu_links' id='lien_met'>", tr( "file description" ), "</a>", sep="" ) )
  })
  
  
  ### Alarm levels
  output$uiSBdownloadlev1_title <- renderUI({
    HTML( hh( tr( 'alarm levels' ) ) )
  })

  output$uiSBdownloadlev1_button <- renderUI({
    downloadButton( 'downloadlev1', tr( 'download' )  )
  })
  
  output$uiSBdownloadlev1_legend <- renderUI({
    HTML( paste( "<a class='menu_links' id='lien_lev1'>", tr( "file description" ), "</a>", sep="" ) )
  })
  
  
  ### Alarm matrix
  output$uiSBdownloadmat_title <- renderUI({
    HTML( hh( tr( 'alarm matrix' ) ) )
  })
  
  output$uiSBdownloadmat_button <- renderUI({
    downloadButton( 'downloadmat', tr( 'download' )  )
  })
  
  output$uiSBdownloadmat_legend <- renderUI({
    HTML( paste( "<a class='menu_links' id='lien_mat'>", tr( "file description" ), "</a>", sep="" ) )
  })

  
  # Data of the alarm map
  output$uiSBdownloadmap_title <- renderUI({
    HTML( hh( tr( 'download title 2' ) ) )
  })
  
  output$uiSBdownloadmap_button <- renderUI({
    downloadButton( 'downloadmap', tr( 'download' )  )
  })
  
  output$uiSBdownloadmap_legend <- renderUI({
    HTML( paste( "<a class='menu_links' id='lien_map'>", tr( "file description" ), "</a>", sep="" ) )
  })
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  
  
  ##################################################
  # FILTRES  
  ##################################################  

  # Ensemble du jeu de données pour le pas de temps selectionné
  Dataset <- reactive({
    #tt <- ifelse( !is.null(input$TimeUnit), input$TimeUnit, "week" )
    get( paste("Data_", input$TimeUnit, sep="") )
  }) 


  # On crée deux valeurs réactive qui vont prendre l'index de la région selectionnée et
  # la liste des sources selectionnées.
  observe({
    values$Region <- ifelse( input$Geo %in% c("FRANCE", "METROPOLE"), input$Geo, ifelse(input$Geo=="regionsA", input$RegionA, input$RegionN) )
    
    if( is.null(input$Source) || input$Source== "Toutes" ){
      values$Source <- unique( Dataset()[ Classe_age==input$Age & RS==input$RS & Region==values$Region, Source ] ) 
    } else {
      values$Source <- input$Source
    }
  })  
  
  
  # Sous-ensemble du jeu de données : sélection sur  "RS"         "Region"     "Source"     "Classe_age" 
   Datasubset <- reactive({
    dd <- Dataset()[ .(input$RS, values$Region, values$Source, input$Age)  ]
    d1 <- switch( input$TimeUnit,
                   "day"=input$DateStart,
                   "week"=input$DateStart-6, 
                    "month"=as.Date( paste(format( input$DateStart, "%Y-%m" ), "01", sep="-")  )
                  )
     dd <- dd[ d>=d1 & d<=input$DateEnd ]
     return(dd)
   }) 

  
  output$Source = renderUI({
      vars <- c( "Toutes", unique( Dataset()[ Classe_age==input$Age & RS==input$RS & Region==values$Region, Source ] ) )
      selectInput( "Source",
                   HTML('<b><span style="color: #181866"><font>Source</font></span></b>'),
                   choices = getNamedList2( vars ),  
                   selected = input$Source ) 
      
  })
  
  output$Age = renderUI({
    vars <- levels( Dataset()[ .(input$RS, values$Region) ]$Classe_age )
    if( !is.null(input$Source) && input$Source=="Sentinelles" ){
      vars <- "Tous ages"
    }
    
    # For menu determnination of epidemic periods
    if( input$menu=="determination" ) {
      if( input$RS == "Bronchiolite" ){
        vars <- c( "Tous ages", "Moins de 2 ans")
      } else {
        vars <- "Tous ages"
      }
    }
    
    selectInput( "Age", 
                 hh( tr("age class") ),
                 choices = getNamedList2( vars ),
                 selected = input$Age  
                 )
  })


  output$Geo = renderUI ({ 
    radioButtons("Geo",
        hh( tr("analyzed zone") ),
        #choices=getNamedList( lang_analyzedzones ) 
        choices=getNamedList2( List_analyzed_zones ),
        selected=input$Geo
    )  
  })


  output$GeoMap = renderUI ({  
    radioButtons("GeoMap", 
                 hh( tr("cartography") ),
                 #getNamedList( lang_mappedzones )
                 getNamedList2( List_mapped_zones ),
                 selected=input$GeoMap
    )
  })  

  
  output$label_TimeUnit <- renderText( { 
    hh( tr("time unit") )
  })

  output$label_newRegion <- renderText( { 
    hh( tr("new region") )
  })
  
  output$label_oldRegion <- renderText( { 
    hh( tr("old region") )
  })
  
  

  output$label_DateStart <- renderText( { 
    hh( paste( tr( paste( "starting", input$TimeUnit, sep=" " ) ) ) )
  } )
  
  
  output$label_DateEnd <- renderText( { 
    lab <- ifelse( (input$menu=='donnees' & input$menu_donnees=='cartes')
        | ( input$menu=='determination' & (input$menu_determination=='carte_alarme' | input$menu_determination=='matrice_alarme') )
        | ( input$menu=='situation' & (input$menu_situation=='minimap_proportion' | input$menu_situation=='minimap_alarme') )
        , cap( tr( input$TimeUnit ) ), 
        paste( tr( paste( "ending", input$TimeUnit, sep=" " ) ) ) 
    )
    
    hh( lab )
  } )

  
  output$uiSBDateStart <- renderUI({
    dateInput( 'DateStart',
               label= htmlOutput("label_DateStart"),
               value = dayrange$first, 
               min = dayrange$first, max = dayrange$last,
               format = tr( "dateInput format"), startview = "month", weekstart = 1,
               language = tr( "language" ) )
 })
  
  
  output$uiSBDateEnd <- renderUI({
    dateInput( 'DateEnd', label= htmlOutput("label_DateEnd"),
               value = lastsunday,  
               min = dayrange$first, max = dayrange$last,
               format = tr( "dateInput format"), startview = "month", weekstart = 1,
               language = tr( "language" )
    )
  })
  
  ##################################################
  #SITUATION EPIDEMIOLOGIQUE
  ##################################################
  
  #################################################
  # MINI ALARM MAPS
  #################################################
  
  # NB: separating bronchiolitis All ages and Younger than 2 years
  for( i in 1:length(List_RS_short) ) {
    
    # Local permet de faire la boucle en faisant avancer i (sinon ne marche pas)
    local({

      # Syndromic grouping
      rs <- List_RS_short[i]
      
      # Label of the map : name of the syndromic grouping in the chosen language
      namelab <- paste( "label_alarm_map", i, sep="_" )
      output[[ namelab ]] <- renderText({ tr( rs ) })
      
      # Classes d'age disponibles pour ce jeu de données au pas de temps selectionné
      list_ages <- unique( alarm_level[ RS==rs ]$Classe_age )
      
      for( j in 1:length( list_ages ) ){
        local({
        age <- list_ages[ j ]
        
        
        # id de l'element HTML contenant la petite carte à cliquer
        id <- ifelse( length(list_ages)>1, paste( "petite_carte_alarme", i, j, sep="_"), paste( "petite_carte_alarme", i, sep="_")) 
        onclick( id, expr=
        { updateSelectizeInput(session, "RS", selected = rs ); 
          updateSelectizeInput(session, "Age", selected = age ); 
          updateTabsetPanel(session, inputId="menu", selected = "determination");
          updateTabsetPanel(session, inputId="menu_determination", selected = "carte_alarme")
        }
        )
        
        # Nom du graphique pour le regroupement syndromique 
        nom <- ifelse( length(list_ages)>1, paste( "mapAlarme", i, j, sep="_"), paste( "mapAlarme", i, sep="_") ) 
        
        output[[ nom ]] <- renderPlot( {
          
          # Selection du type de region
          if( is.null(input$GeoMap) || input$GeoMap=="regionsN" ) {
            map.fr <- map.fr_regionsN
            liste_reg <- Liste_regionsN
            corresp_nomreg_numreg <- corresp_nom_num_regionsN
          } else { 
            map.fr <- map.fr_regionsA
            liste_reg <- Liste_regionsA
            corresp_nomreg_numreg <- corresp_nom_num_regionsA
          }
          
          # Selection de la date de la carte : dernier temps en cours
          DateEnd <- ifelse( is.null(input$DateEnd), max( alarm_level$d ), input$DateEnd )
          lastd <- max( alarm_level$d[ alarm_level$d <= DateEnd ] )
          #print( nom ); print( age )
          tmp <- alarm_level[ CJ( rs, liste_reg, as.character(age), lastd  ), nomatch=0L ][ corresp_nomreg_numreg, on=c(Region="code_reg") ]
          tmp[, col := coul.alarme[ tmp$niv_alarme ] ]
            
          ##### Carte  
          par( mar=c(0,0,0,0)  )
          plot( map.fr, col=tmp$col )
          #box("figure", bty="o")
          
        }, width=150, height=170*0.75) 
        
       })
        
      }
    })
  }
  
  output$label_allage_alarm <- renderText( { 
    tr("Tous ages") 
  })
  
  output$label_younger2_alarm <- renderText( { 
    tr("Moins de 2 ans") 
  })
  
  
  
  
  
  #################################################
  # MINI PROPORTION MAPS
  #################################################

  output$HeaderTableAccueil <- renderText( {
    switch( length(unique( Dataset()$Source )), 
            "An error has occured in header names",
            "<TABLE width=100% class='HeaderCartes' ><TR>
            <TH width='50%'>SAU</TH> 
            <TH>SOS</TH></TR> 
            </TABLE>",
            "<TABLE width=100% class='HeaderCartes' ><TR>
            <TH width='33.33%'>SAU</TH> 
            <TH width='33.33%'>SOS</TH> 
            <TH>Sentinelles</TH></TR> 
            </TABLE>"
    ) 
  }) 
  
  # Time (day, week or month) of the mapped data
  output$time_prop_maps <- renderText( {
    tt <- which.max( Dataset()$d[Dataset()$d<=input$DateEnd ] )
    temps <- paste( cap( tr(input$TimeUnit) ), 
                    as.character(  Dataset()[ tt, toCapfrench( input$TimeUnit ), with=F] ) 
    ) 
    if( input$TimeUnit=="week" ){
      #temps <- paste( temps, Dataset()$Dates[ tt ], sep="<BR>")
      temps <- paste( temps, getDayRange( Dataset()$d[ tt ] ), sep="<BR>")
    }
    temps
  })
  
  output$time_alarm_maps <- renderText( {
    tt <- which.max( Dataset()$d[Dataset()$d<=input$DateEnd ] )
    temps <- paste( cap(tr(input$TimeUnit)), 
                    as.character(  Dataset()[ tt, toCapfrench(input$TimeUnit), with=F] ) 
    ) 
    if( input$TimeUnit=="week" ){
      #temps <- paste( temps, Dataset()$Dates[ tt ], sep="<BR>")
      temps <- paste( temps, getDayRange( Dataset()$d[ tt ] ), sep="<BR>")
    }
    temps
  })
  
  # Petites cartes pour l'onglet situation epidémiologique
  # Choix de la source et du RS
  for( i in 1:length(List_RS_short) ) {
    
    # Local permet de faire la boucle en faisant avancer i (sinon ne marche pas)
    local({
      
      # Syndromic grouping
      rs <- List_RS_short[i]
      
      # Label of the map : name of the syndromic grouping in the chosen language
      namelab <- paste( "label_prop_map", i, sep="_" )
      output[[ namelab ]] <- renderText({ tr( rs ) })
      
      
      onclick( paste("petite_carte", i, sep="_"), expr=
      { updateSelectizeInput(session, "RS", selected = rs ); 
        updateTabsetPanel(session, inputId="menu", selected = "donnees");
        updateTabsetPanel(session, inputId="menu_donnees", selected = "cartes")
      }
      )
      
      # Nom du graphique pour le regroupement syndromique (contient toutes les sources)
      nom <- paste( "mapAccueil", i, sep="_")
      par( mfrow=c(1,1), mar=c(0,0,0,0) )
      
      
      widthMap <- reactive( { 150*length(unique( Dataset()[ RS==rs ]$Source ))} )
      
      output[[nom]] <- renderPlot( {
        
        cat( "\nPetite carte", nom, "\n")
        
        # Selection du type de region
        if( is.null(input$GeoMap) || input$GeoMap=="regionsN" ) {
          map.fr <- map.fr_regionsN
          liste_reg <- Liste_regionsN
          corresp_nomreg_numreg <- corresp_nom_num_regionsN
        } else { 
          map.fr <- map.fr_regionsA
          liste_reg <- Liste_regionsA
          corresp_nomreg_numreg <- corresp_nom_num_regionsA
        }
        
        # Selection de la date de la carte : dernier temps en cours
        lastd <- max( Dataset()$d[ Dataset()$d <= input$DateEnd ] )
        print( lastd )
        
        # Sources disponibles pour ce jeu de données au pas de temps selectionné
        list_sources <- unique( Dataset()[ RS==rs ]$Source )
        #print( list_sources )
        
        
        par( mfrow=c(1, length(list_sources)), mar=c(0,0,0,0) )
        for( so in list_sources ){
          # Selection du regroupement syndromique, des regions, de la source, de la classe d'âge et de la date 
          # Merge du subset avec les numeros de region
          # Ordonne selon le numero des regionsen mergeant sur corresp_nomreg_numreg
          tmp <- Dataset()[ CJ( rs, liste_reg, so, "Tous ages", lastd  ), nomatch=0L ][ corresp_nomreg_numreg, on=c(Region="code_reg") ]
          
          # rs="Syndrome grippal"
          #lastd <- max( alarm_level$d[ alarm_level$d <= input$DateEnd ] )
          #tmp <- alarm_level[ CJ( rs, liste_reg, "Tous ages", lastd  ), nomatch=0L ][ corresp_nomreg_numreg, on=c(Region="code_reg") ]
          #tmp[, col := coul.alarme[ tmp$niv_alarme ] ]
          
          
          ###### Creation de 4 niveaux de Proportion2
          seuils <- seuils_carte$seuil_Proportion2[ seuils_carte$Source==so 
                                                    & seuils_carte$Classe_age=="Tous ages" 
                                                    & seuils_carte$RS==rs ]
          maxi   <-  max( ceiling(tmp$Proportion2), max(seuils)+1, na.rm=T )
          lev    <- unique( c(0, seuils[seuils < maxi], maxi )   )
          cat( "\n", rs, so, "niveau:", lev, "\n" )
          
          tmp[, classe.prop := cut( tmp$Proportion2, breaks=lev, right=F, include.lowest=T ) ]
          tmp[, col := coul.prop[ tmp$classe.prop ] ]
          tmp[ tmp$Nb_total_passages==0, col:=NA ] 
          
          
          ##### Carte  
          plot( map.fr, col=tmp$col  )
          #box(bty="o")
        }
      }, width=widthMap, height=170*0.75) 
      
    })
  }
  
  
  
  
  
  #################################################
  # ALARM LIST 
  #################################################
  #output$TitreListing<- renderText( paste( tr("alarm list"), ", ", getname.region( unique(Datasubset()$Region)), sep="" ) )
  
  output$ListingAlarmes <- DT::renderDataTable({
    dd <- alarm_list[ Region==values$Region & d>= input$DateStart-6 & d<=input$DateEnd  ]
    if( !is.null(dd) ){
      setorder( dd, -d )
      
      # translate the syndromic groupings
      dd[ , RS:=tr( as.character(RS) ) ]
      
      # translate the age classes
      dd[ , Classe_age:=tr( as.character(Classe_age) ) ]
      
      # translate the methods
      dd[ , Methode:=gsub( "Serfling robuste", tr("rob serf"), Methode, fixed=T) ]
      dd[ , Methode:=gsub( "Serfling", tr("serf"), Methode, fixed=T) ]
      
      # translate the week dates
      if( values$language=="en" ){
        dd[, Dates:=paste( "from", d, "to", d+7 ) ]
      }
      
      dd[ , c("d", "Region"):=NULL ]
      nomscols_aff <- tr( names(dd) )
    }

    
    DT::datatable( if (is.null( dd )) invisible()
                   else dd, 
                   rownames = FALSE,
                   colnames = nomscols_aff,
                   extensions = c("Buttons"),
                   options=list(
                     lengthMenu = list(c(10, 25, 50, -1), c('10', '25','50', 'All')), 
                     pageLength = 10,
                     orderClasses = TRUE,
                     dom = 'Blfrtip',
                     buttons = c('copy', 'csv', 'excel', 'pdf', 'print')   
                   )

    , caption=paste( tr("alarm list"), ", ", getname.region( unique(Datasubset()$Region)), sep="" )
   )
  }) 
  
  
  output$FootnoteAlarmList <- renderText({
    paste0( tr("serf"), ": ", tr("Serfling"),
           "<BR>HMM: ", tr("HMM") 
    )
  })
  

  
  #####################################################################
  # SERIE TEMPORELLE REPRESENTANT LA PROPORTION ET/OU TAUX D'INCIDENCE  
  #####################################################################  
    output$dygraph_series <- renderDygraph({
      dd <- dcast.data.table(  Datasubset()[, .(Proportion2, Source, d) ],d ~ Source, value.var="Proportion2" )
      d3    <- xts( dd[ , names(dd)!="d", with=F ], order.by=dd[, d])

      dy <- dygraph( d3, main=paste( tr( input$RS ), tr( input$Age ), 
                                     tr( getname.region( unique(Datasubset()$Region)) ), 
                                     sep=" - ")
                     ) 
      if( ncol(d3) == 1){
        dy <- dy %>%
        dyAxis( 'y', label=names(d3) )  
      } else {
          dy <- dy %>%
          dyAxis( 'y', label=ifelse( ncol(d3)>2, paste( names(d3)[-ncol(d3)], collapse=", " ), names(d3)[1] ) )  %>%
          dyAxis( 'y2', label=names(d3)[ncol(d3)] )  %>%
          dySeries( names(dd)[ ncol(dd)], axis = 'y2' )
      }
      dy %>% dyLegend(width = 500)
    })
    
    
  ##################################################
  # DATA TABLE
  ##################################################  
  
  #output$TitreDT <- renderText( paste( tr(input$RS), tr(input$Age), tr( getname.region( unique(Datasubset()$Region)) ), sep=" - " ) )
  
  output$DataTable <- DT::renderDataTable({
    nomscols <- names( Datasubset() )
    nomscols <- nomscols[ ( nomscols %in% c( "Semaine", "Dates", "Mois", "Jour", "Source", 
                                             "Nb_passages", "Nb_total_passages", "Proportion2", 
                                             "Proportion_low", "Proportion_up") ) ]
    nomscols_aff <- tr( nomscols ) # Pour l'affichage
    #nomscols_aff[ nomscols_aff=="Nb_passages"] <- 'Nombre de passages'
    #nomscols_aff[ nomscols_aff=="Nb_total_passages"] <- 'Nombre total de passages'

    if( !is.null(Datasubset()) ){
     tmp <- Datasubset()[ , c("d", nomscols), with = FALSE ]
      
      if( values$language=="en" ){
        tmp[, Dates:=paste( "from", d, "to", d+7 ) ]
      }
      
      setorder( tmp, -d )
      tmp[ , d:=NULL ]
      tmp[ , Proportion2:=round(Proportion2) ]
    }
    
    # On réordonne  Datasubset() par temps décroissant : observations les plus récentes en premier
    DT::datatable( if (is.null( Datasubset() )) invisible()
                   else tmp, 
                   rownames = FALSE,
                   colnames = nomscols_aff,
                   extensions = c("Buttons"),
                   options=list(
                     pageLength = 10,
                     lengthMenu = list(c(10, 25, 50, -1), c('10', '25','50', 'All')),
                     orderClasses = TRUE,
                     dom = 'Blfrtip',
                     buttons = c('copy', 'csv', 'excel', 'pdf', 'print')   
                   )
    , caption=paste( tr(input$RS), tr(input$Age), tr( getname.region( unique(Datasubset()$Region)) ), sep=" - " ) 
    )
  }) 
  
    

  
  ###################################################################################################
  # GAUGES REPRESENTING THE PROPORTION AND/OR THE INCIDENCE RATE FOR A GIVEN DAY/WEEK/MONTH 
  # COMPARED TO HISTORICAL DATA IN THE CHOSEN PERIOD
  ###################################################################################################  
  
  output$Jauge <- renderText({
    ss <- unique( Datasubset()$Source )
    NbSources <- length(ss)
    Gauges <- list()
    
    LastDate <- max( Datasubset()$d )
    LastTime <- unlist( unique( Datasubset()[ d==LastDate, toCapfrench(input$TimeUnit), with=F ] ) )
    FirstTime <- unlist( unique( Datasubset()[ which.min(d), toCapfrench(input$TimeUnit), with=F ] ) )
    
    # Titre global pour l'affichage
    date_titre <- LastTime 
    if( input$TimeUnit == "week" ) {
      date_titre <- paste(
        tr( "week" ), " ",
        date_titre, " (",
        getDayRange( LastDate ),
        #unlist( Datasubset()[ nrow(Datasubset()), Dates ] ), 
        ")", sep=""
      )
    }

    
    for (i in 1:NbSources) {
      TMP2 <- Datasubset()[ Source == ss[i] ]

      # Calcul des min et max sur toute la période
      Min_prop <- round( min( TMP2$Proportion2, na.rm=T ) )
      Max_prop <- round( max( TMP2$Proportion2, na.rm=T ) )
      cat( "\n", ss[i], "min prop:", Min_prop, "max prop:", Max_prop, "\n")

      # Calcul des seuils intermediaires : 50% et 80%
      seuils <- quantile( TMP2$Proportion2, c(0.5, 0.8 ), na.rm=T )
        
      TMP2 <- TMP2[ d==LastDate, .( Source, RS, Proportion2 ) ]
      TMP2[, Proportion2:=round(Proportion2) ]
      setnames( TMP2, "Proportion2", ss[i] )
      
      Jauge <- gvisGauge(TMP2, options=list( min=Min_prop, max=Max_prop,
                                             greenColor  = coul.jauge[1], greenFrom =Min_prop, greenTo =seuils[1],
                                             yellowColor = coul.jauge[2], yellowFrom=seuils[1], yellowTo=seuils[2],
                                             redColor    = coul.jauge[3], redFrom   =seuils[2], redTo   =Max_prop,
                                             width=200, height=300, style="padding: 10px;")
      )
                         
      
      titre <- paste( "<center>", tr("number of"), " ",
                      tr( typeacte[[ ss[i] ]] ), " ", tr("for"), "<br>", 
                      tolower( tr(input$RS) ), " ", tr("for"), "<br>",
                      tr( typedeno[[ ss[i] ]] ), 
                      "</center>", sep="" ) 
      if( nrow(TMP2)==0 ){
            titre <- paste( titre, "<br><span style=\"color: #FF0000\">", 
                            paste( tr("no data1"), ss[i], tr("no data2"), sep="" ),
                            "pour", LastTime, "</span>" )
      }
      Gauges[[ ss[i] ]] <- addGvisATLTitle(Jauge , titre)
    }
    
    # Combinaison des jauges
    GT <- Reduce( gvisMerge2, Gauges )
    
    # Affichage + Titre de la légende
    c( "<br><center>", paste( tr( input$RS ), tr( input$Age ),
                              tr( getname.region( unique(Datasubset()$Region) ) ),
                              date_titre , sep=" - " ), 
        "<table class=\"TableGauge\" ><tr><td>", GT$html$chart, "</td></tr></table>",
        "</center>", 
       tr( "percentiles calculated on the" ), 
       getPlural( tr( input$TimeUnit ) ),
       FirstTime, tr( "to" ), LastTime  ) 

  })
  

  output$legend_jauge <- renderText({
    HTML( paste( '<img src="img/legend_jauge_', values$language, '.png" alt="legend_jauge" >', sep="" ) )
  })
  
 
  

  ###################################################################################################
  # CARTES REPRESENTANT LA PROPORTION ET/OU TAUX D'INCIDENCE POUR LA DERNIERE DATE (SEMAINE OU JOUR)   
  ###################################################################################################  
 
  heightMap <- reactive({
      n <-length( unique(Datasubset()[ d==max( Datasubset()$d ) ]$Source) ) 
      switch( n , "1"=700, "2"=600, "3"=400  )
  })
  
  
  output$mapReg2 <- renderPlot({

    # Selection des données selon le type de zone sélectionné
    map.fr <- get( paste( "map.fr", input$GeoMap, sep="_" ) ) 
    centro.reg <- get( paste( "centro", input$GeoMap, sep="_" ) )  
    liste_reg <- get( paste( "Liste", input$GeoMap, sep="_" ) ) 
    corresp_nomreg_numreg <- get( paste( "corresp_nom_num", input$GeoMap, sep="_" ) )
    
    # Selection de la date de la carte, par defaut la plus grande inferieure ou egale a input$DateEnd 
    lastd <- max( Datasubset()$d )
    cat( "\nmapReg2 lastd:", lastd, "\n")
    #lastd <- max( Data_week$d )
    
    # Quelle source ?
    #data.lastd <- Data_week[ CJ("Syndrome grippal", liste_reg, c("SAU", "SOS", "Sentinelles"), "Tous ages", lastd ), nomatch=0L ]
    #data.lastd <- Data_week[ RS=="Syndrome grippal" & Region %in% liste_reg & Source %in% c("SAU", "SOS", "Sentinelles") & Classe_age=="Tous ages" & d==lastd ]
    data.lastd <-  Dataset()[ CJ(input$RS, liste_reg, values$Source, input$Age, lastd), nomatch=0L ]

    sources <- unique( data.lastd$Source ) 
    cat( "\nsources",  sources, "\n")
    
    # Selection des regions, de la date et boucle sur la source
    cexMap <- ifelse( length(sources)==3, 1.5, 1 )   
    cexMap2 <- ifelse( length(sources)==3, 1.5, 1 )   
    
    inset <- -0.5
    omi <- ifelse( length(sources)==1, 1.7, 2)
    par( mfrow=c(1, length(sources)), omi=c(omi,0.2,0.6,0), xpd=NA, mar=c(0,0.5,1,0.5), cex.main=cexMap*1.3  ) 
    for(  so in sources ) {
      
      # Selection du regroupement syndromique, des regions, de la source, de la classe d'âge et de la date 
      # Merge du subset avec les numeros de region
      # Ordonne selon le numero des regionsen mergeant sur corresp_nomreg_numreg
      #tmp <- Datasubset()[ .(input$RS, liste_reg, so, input$Age, lastd  ) ][ corresp_nomreg_numreg,  on=c(Region="code_reg") ]
      tmp <- data.lastd[ Source==so ][ corresp_nomreg_numreg,  on=c(Region="code_reg") ]
      #cat( "\ntmp\n")
      #print( tmp )
      
      #tmp <- tmp1[ tmp1$Source==so, ]
      # Merge du subset avec les numeros de region
      #tmp <- merge( tmp, corresp_nomreg_numreg, by.x="Region", by.y="code_reg", all=T )
      # Ordonne selon le numero des regions( = meme ordre que fond de  carte)
      #tmp <- tmp[ order(tmp$num_reg), ]
      
      
      ###### Creation de 5 niveaux de Proportion2
      seuils <- seuils_carte$seuil_Proportion2[ seuils_carte$Source==so & seuils_carte$Classe_age==input$Age & seuils_carte$RS==input$RS ]
      maxi   <- max( ceiling(tmp$Proportion2), max(seuils)+1, na.rm=T )
      lev    <- unique( c(0, seuils, maxi )   )
      

      if( length(seuils)==1 ){
        lablev <- lev[ length(lev)-1 ] 
      } else {
        lablev <- c( paste( "[", lev[-(length(lev)-c(1,0))], ",", lev[ -c(1,length(lev)) ], ")", sep="" ),
                     paste( ">=", lev[ length(lev)-1 ] )  )
      }
      
      
      tmp[ , classe.prop := cut( tmp$Proportion2, breaks=lev, right=F, include.lowest=T ) ] #, labels=F
      tmp[ , col := coul.prop[ tmp$classe.prop ] ]
      tmp[ Nb_total_passages==0 & !is.na(Nb_total_passages_tousages), col:="grey" ]  
      

      # Traitement des DOMs
      #petites_iles <- c( "GUADELOUPE", "MARTINIQUE", "REUNION", "MAYOTTE", "SAINT-BARTHELEMY", "SAINT-MARTIN" )
      petites_iles <- c( 1, 2, 4, 6, 7, 8 )
      
      cond1 <- !is.na( tmp$Proportion2 ) 
      cond2 <- tmp$Region %in% petites_iles 
      
      lab <- round( tmp$Proportion2[ cond1 & cond2 ], 0 )
      
      ##### Carte  
      plot( map.fr, col=tmp$col  )
      if( any( cond1 & !cond2) ) {
        graphics::text( centro.reg[ cond1 & !cond2, ], labels=round( tmp$Proportion2[ cond1 & !cond2 ], 0), cex=cexMap2 )
      }
      if( any( cond1 & cond2) ) {
        graphics::text( centro.reg[ cond1 & cond2, ], labels=lab, pos=2, offset=1.5, cex=cexMap2 )
      }
      

      fill <- c( coul.prop[1:length(lablev)], "white" )
      leg <- c( lablev, paste( tr("no data1"), so, tr("no data2"), sep="") )
      if( input$Age!="Tous ages" & any( tmp$Nb_total_passages==0 & !is.na(tmp$Nb_total_passages_tousages) ) ){
        fill <- c( fill, "grey" )
        leg <- c( leg, paste( "Proportion incalculable : 0", typeacte[[so]], "pour la classe d'âge") )
      }
      
      # Legend title
      legend_title <- paste( tr("number of"), " ",
                      tr( typeacte[[ so ]] ), " ", tr("for"), " ", 
                      tolower( tr(input$RS) ), "\n", tr("for"), " ",
                      tr( typedeno[[ so ]] ), 
                       sep="" ) 
      
      legend( 
        par("usr")[1],
        par("usr")[3]-0.05*(par("usr")[4]-par("usr")[3]),
        fill=fill, 
        legend=leg,
        cex=cexMap, inset=c(0,inset), bty="n",
        #title=paste( "Nombre de ", typeacte[[ so ]], " pour ", tolower( input$RS ), "\npour ", typedeno[[ so ]], sep="" ) 
        title=legend_title
        )
      
      title( so )   #, cex.main=2
    }
    
    # Titre global
    date_titre <- switch( input$TimeUnit,
                          "day"= format( lastd, tr("date format") ) ,
                          "week"= paste( tr( "week" ), " ", data.lastd$Semaine[1] ,
                                         " (", getDayRange( lastd ), ")", 
                                         sep="" ),    
                          "month"= paste( months(lastd), format(lastd, "%Y") ) 
    )
    
    title( main=paste( tr( input$RS ), tr( input$Age ), date_titre, sep=" - " ), line=1, outer=T ) #, cex.main=2
  } )
  
  
  #, width = 800, height=heightMap

  
  
  ###################################################################################
  # CALENDRIER REPRESENTANT LA PROPORTION ET/OU TAUX D'INCIDENCE PAR SEMAINE OU JOUR 
  ###################################################################################  
  output$DataCal <- renderText({
    if (input$Source=="Toutes")
    {
      Cal1 <- gvisCalendar(subset(Datasubset(), Source=="SAU"), 
                           datevar="d",
                           numvar="Proportion", 
                           options=list(
                             title=paste( tr(input$RS)," - SAU"," - ", tr(input$Age) ),
                             width=900, height=900,
                             calendar="{yearLabel: { fontName: 'Times-Roman',
                             fontSize: 32, color: '#1A8763', bold: true},
                             cellSize: 15,
                             cellColor: { stroke: 'red', strokeOpacity: 0.2 },
                             focusedCellColor: {stroke:'red'}}"
                           )
                           )
      
      Cal2 <- gvisCalendar(subset(Datasubset(), Source=="SOS"), 
                           datevar="d",
                           numvar="Proportion", 
                           options=list(
                             title=paste( tr(input$RS)," - SOS"," - ", tr(input$Age) ),
                             width=900, height=900,
                             calendar="{yearLabel: { fontName: 'Times-Roman',
                             fontSize: 32, color: '#1A8763', bold: true},
                             cellSize: 15,
                             cellColor: { stroke: 'red', strokeOpacity: 0.2 },
                             focusedCellColor: {stroke:'red'}}"
                           )
                           )
      GT <- gvisMerge(Cal1,Cal2, horizontal=TRUE) 
      c(GT$html$chart,paste("\n<script type=\"text/javascript\">drawChart",GT$chartid,"();</script>\n",sep=""))
}  
    else {  
      Cal <- gvisCalendar(Datasubset(), 
                          datevar="d",
                          numvar="Proportion", 
                          options=list(
                            title=paste( tr(input$RS)," - ", input$Source, " - ", tr(input$Age) ),
                            width=900, height=900,
                            calendar="{yearLabel: { fontName: 'Times-Roman',
                            fontSize: 32, color: '#1A8763', bold: true},
                            cellSize: 15,
                            cellColor: { stroke: 'red', strokeOpacity: 0.2 },
                            focusedCellColor: {stroke:'red'}}")
                          )
      c(Cal$html$chart,paste("\n<script type=\"text/javascript\">drawChart",Cal$chartid,"();</script>\n",sep=""))
      }
})     
  
  

  ##################### Alarm dygraphs #################################################
  
  for( j in 1:length(List_methods) ){
    
    local({
      met <- List_methods[j]
      alarmname <- paste( "alarme", met, sep="_" )
      
      for( i in 1:length(List_sources) ){
        
        local({
          sou <- List_sources[ i ]
          varname <- ifelse( sou=="Sentinelles", "TxIncidence100000", 'Proportion10000' )
          
          output[[ paste( "dygraph", met, sou, sep="_" ) ]] <- renderDygraph({
            
            if( !(sou %in% values$Source) || input$TimeUnit!="week" ){
              invisible()
            } else {
              
              cat( "\nEnter Dygraph", met, sou, input$RS, "\n")
              #dd <- Data_week[ .("Syndrome grippal", "FRANCE", "SOS", "Tous ages") ]
              dd <- Datasubset()[ Source==sou ]
              
              dd <- rbindlist( list( dd, 
                                     data.table( d=max(dd$d)+6, Proportion=dd$Proportion[which.max(dd$d)]) ), 
                               use.names = T, fill=T )
              
              # Transformation au format xts en sélectionnant les variables utiles de chaque méthode
              vars <- switch( met,
                              Serfling = c("Proportion", "baseline", "serf95"),
                              Serfling_robuste = c("Proportion", "baseline_robuste", "serf95_robuste"),
                              HMM = "Proportion"
              )
              d1 <- xts( dd[ , (vars), with=F ], order.by=dd[, d])
              
              # Pour SOS et SAU, multiplication par 10000
              if( sou != "Sentinelles" ) {
                d1 <- d1 * 10000
              }
              
              
              # On renomme les variables pour l'affichage du graphique
              names(d1)[1] <- varname
              if( met!="HMM" ){
                names(d1)[2:3] <- c("Baseline", "IP95") 
              } 
              
              # Graphique
              dy <- dygraph( d1, main = paste( tr(input$RS), tr(input$Age), 
                                               tr(getname.region(values$Region)), tr(sou),
                                               tr(met), sep=" - "), group = "groupe" )  %>% 
                dySeries( varname, stepPlot = TRUE, color="blue") %>% 
                dyLegend( labelsDiv=paste( "legendDivID", met, sou, sep="_" ), show="onmouseover" ) 
    
              #dy <- dy %>% dyLegend( width = 600 )   
                
              
              # Si Serfling ou Serfling Robuste : plot du baseline et du seuil
              if( met %in% c("Serfling", "Serfling_robuste") ){
                dy <- dy %>%  
                  dySeries("Baseline", color="purple") %>%
                  dySeries("IP95", color="red") 
              }
              
              # S'il existe alarmes (calculées et non-nulles), on recherche le debut et la fin des periodes epidemiques
              alarmes <- dd[ , alarmname, with=FALSE ]
              if( any( alarmes==1, na.rm=T ) ){
                bigLL <- search.epidemic.period( dd, epid=alarmname )
                dy <- add_shades( dy, bigLL, color = "#FFFFB3" ) #"#FFFFCC"
              } 
              
              # S'il existe des valeurs manquantes dans les alarmes mais pas dans les données,
              # on recherche le debut et la fin des ces periodes et on les représente en gris
              cond <- is.na( alarmes ) & !is.na( dd$Proportion )
              if( any( cond ) ){
                dd[ , isNA_alarme := as.numeric( cond ) ]
                mis <- search.epidemic.period ( dd, epid="isNA_alarme" )
                dy <- add_shades( dy, mis, color = "#808080" )
              } 
              
              dy
            }
          }) 
        })
      }
      
      output[[ paste("legend_dygraph_", met, sep="") ]]  <- renderText({
        HTML( paste( '<img src="img/legend_dygraph_', values$language, '.png" alt="legend_dygraph" >', sep="" ) )
      })
      
    })
  }
  

  
  
  ###################################################################################################
  # TELECHARGEMENT des seuils et niveaux d'alarme 
  ###################################################################################################  
  
  # Une seule régions
  output$downloadmet <- downloadHandler(
    filename = function() { 
      paste( 'seuils_', input$RS, '_', 
             min( Datasubset()$Semaine ), '_', max( Datasubset()$Semaine ),
             '_zone',values$Region,
             '_', input$Age,
             '.csv', sep='') 
    },
    content = function(file) {
      dat <- copy( Datasubset() )
      dat[ , c("Proportion2", "serf_Sentinelles"):=NULL ]
      
      # dd <- Data_week[ .("Syndrome grippal", "METROPOLE", "SOS", "Tous ages") ]
      setkey( dat, Region)
      dat <- correspondance_nomreg_codereg[ dat ]
      
      write.csv2( dat, file, row.names=F )
    }
  )
  

  onclick("lien_met", expr=
  { 
    updateTabsetPanel(session, inputId="menu", selected = "questions");
    updateTabsetPanel(session, inputId="menu_questions", selected = "description");
    updateTabsetPanel(session, inputId="menu_description", selected = "met");
  }
  )
  
  
  
  # Niveau des alarmes pour la région sélectionnée
  output$downloadlev1 <- downloadHandler(
    filename = function() { 
      paste( 'niv_alarm_', input$RS, '_', 
             min( Datasubset()$Semaine ), '_', max( Datasubset()$Semaine ),
             '_zone',values$Region,
             '_', input$Age,
             '.csv', sep='') 
    },
    content = function(file) {
      dat <- alarm_level[ CJ(input$RS, values$Region, input$Age), nomatch=0L ][ d >= input$DateStart & d <= input$DateEnd ]
      setkey( dat, Region)
      dat <- correspondance_nomreg_codereg[ dat ]
      write.csv2( dat, file, row.names=F )
    }
  )

  onclick("lien_lev1", expr=
  { 
    updateTabsetPanel(session, inputId="menu", selected = "questions");
    updateTabsetPanel(session, inputId="menu_questions", selected = "description");
    updateTabsetPanel(session, inputId="menu_description", selected = "niv");
  }  )
  
  onclick("lien_map", expr=
  { 
    updateTabsetPanel(session, inputId="menu", selected = "questions");
    updateTabsetPanel(session, inputId="menu_questions", selected = "description");
    updateTabsetPanel(session, inputId="menu_description", selected = "niv");
  }  )
  

  

  ############################ Heatmpap ###########################################
  
  output$TitreHeatmap <- renderText( {
    
    paste( tr(input$RS), tr(input$Age),
           paste( 
             tr("week"), " ",
             max( Data_week[ d <= input$DateEnd ]$Semaine ), " (",
             getDayRange( max( Data_week[ d <= input$DateEnd ]$d ) ), ")", sep=""
             ),
          sep=" - " 
        ) 
    })
  
  output$FootnoteHeatmap <- renderText({
    paste( "*", tr("the alarm level is the"), " ",
           "<a class='menu_links' id='lien_faq'>", 
           tr("synthesis"), "</a>", " ",
           tr("of the alarms emitted"), ".", sep="" 
    )
  })
  
  onclick("lien_faq", expr=
  { 
    updateTabsetPanel(session, inputId="menu", selected = "questions");
    updateTabsetPanel(session, inputId="menu_questions", selected = "methodes");
    session$sendCustomMessage( type = "goToAncre", message=list( ancre="calcul_alarmes" ) )
  }
  )
  
  
  Data_heatmap <- reactive({
    
    # Selection de la liste des zones, selon le type de zone
    liste_reg <- get( paste( "Liste", input$GeoMap, sep="_" ) ) 
    #liste_reg <- get( paste( "Liste", "regionsN", sep="_" ) ) 
    
    #data.lastd <- Data_week[ CJ( "Bronchiolite", c(liste_reg, "METROPOLE", "FRANCE"), c("SAU", "SOS", "Sentinelles"),"Tous ages", max(d)-7 ), nomatch=0L ]
    lastd <- max( Data_week$d[ Data_week$d <= input$DateEnd ] )
    data.lastd <- Data_week[ CJ( input$RS, 
                                    c(liste_reg, "METROPOLE", "FRANCE"), 
                                    c("SAU", "SOS", "Sentinelles"), 
                                    input$Age, lastd ), nomatch=0L ]
    
    # Si aucune alarme calculée, on renvoie NULL
    if( all( is.na(data.lastd[,.(alarme_Serfling, alarme_Serfling_robuste, alarme_HMM)]) ) ) {
      NULL
    } else {
      # Sinon
      
      # Statut 4 (couleurs associée : gris) pour les cas où proportion existe mais méthode échouée
      for( alarme in c("alarme_Serfling", "alarme_Serfling_robuste", "alarme_HMM")){
        data.lastd[ !is.na(Proportion) & is.na( data.lastd[[(alarme)]] ), (alarme):=4  ]
        
        # Transcodage : alarme 0-->1 (vert), alarme 1-->3 (rouge)
        data.lastd[ !is.na( data.lastd[[(alarme)]] ) & data.lastd[[(alarme)]]==1, (alarme):=3  ]
        data.lastd[ !is.na( data.lastd[[(alarme)]] ) & data.lastd[[(alarme)]]==0, (alarme):=1  ]
      }
      
      # on ne garde que les colonnes qui nous intéressent
      #data.lastd <- data.lastd[ ,.(RS, Region, Source, alarme_Serfling, alarme_Serfling_robuste, alarme_HMM) ]

      mat <- dcast.data.table( data.lastd,  
                               Region +  RS + Classe_age + Semaine + d + Dates ~ Source,
                               value.var = c( "alarme_Serfling", "alarme_Serfling_robuste", "alarme_HMM")
      )
      
      # Récupération de la colonne niveau d'alarme
      #niv <- alarm_level[ CJ( "Bronchiolite", c(liste_reg, "METROPOLE", "FRANCE"), "Tous ages", max(d)-7 ), nomatch=0L ]
      niv <- alarm_level[ CJ(input$RS, c(liste_reg, "METROPOLE", "FRANCE"), input$Age, lastd), nomatch=0L ]
      
      # Rajout de la colonne niveau d'alarme et de la colonne vide à la matrice mat
      mat <- mat[ niv[, .(Region, niv_alarme)], on=c(Region="Region") ]
      mat[ , niv_alarme:=as.numeric(niv_alarme)]
      
      # Récupération des noms des régions
      #mat[, Region:=getname.region(Region) ]
      mat <- correspondance_nomreg_codereg[ mat ]
      
      # on ordonne les régions par ordre alaphabétique
      mat2 <- mat[ Region %in% c("METROPOLE", "FRANCE")]
      mat <- mat[ !(Region %in% c("METROPOLE", "FRANCE")) ]
      setorder( mat, Nom_region)
      
      # Matrice finale
      mat <- rbindlist( list(mat, mat2), use.names=T, fill=T )
      rm( mat2 )
      
      # Creation colonne vide
      mat[ , space:=NA ]
      noms <- names( mat )[ names(mat) != "niv_alarme" ]
      
      # Mise de la colonne space à l'avant-dernière position
      setcolorder( mat, c(noms, "niv_alarme") )
      
      # On ordonne les colonnes en regroupant les sources
      n <- 7 # Nombre de variables avant les alarmes des méthodes (Region, Nom_region, RS, Classe_age, Semaine, d, Dates)
      nbsources <- (ncol(mat)- n -2 ) / 3 #length( unique(data.lastd$Source))
      if( nbsources==1 ){
        ordre <- c(1,2,3) + n
      } else if ( nbsources==2 ){
        ordre <- c(1,3,5,2,4,6) + n
      } else if ( nbsources==3 ){
        ordre <- c(1,4,7,2,5,8,3,6,9) + n
      }
      ordre <- c( 1:n, ordre, ncol(mat)-c(1,0) )
      setcolorder( mat, ordre )
      
    }
    
  } )
  
  
    
    output$heatmap <- renderD3heatmap({
      mat <- copy( Data_heatmap() )
      
      # On récupère le nom des régions pour nommer les lignes du heatmap
      nomsrow <- mat$Nom_region
      
      # Suppression de la colonne Région et de la colonne Nom_region
      mat[, c("Region", "Nom_region", "RS", "Classe_age", "Semaine", "d", "Dates" ):=NULL ]
      
      
      cat("\nHEATMAP\n")
      print( str(mat) )
      
      if( is.null(mat)){
        invisible()
      } else {

        
        # Création des noms de colonnes
        liste <-  strsplit( colnames(mat)[ 1:(ncol(mat)-2) ], "_", fixed=T)
        nomscol <- sapply( liste, FUN=function(l){
          paste0( tr( l[[length(l)]] ), " (", tr(  paste( l[2:(length(l)-1)], collapse=" " ) ), ")" )
        }
        )
        nomscol <- c( nomscol, "", paste0( tr("alarm level"), "*") )
  
        # On passe mat en matrice
        mat <- as.matrix(mat)
        
        # Notes d'explication apparaissant au passage de la souris
        cellnotes <- c("Pas d'alarme", "Pre-alarme", "Alarme", "Echec méthode")
        cellnote <- matrix( cellnotes[ mat ], nrow=nrow(mat) )
        cellnote[, 1:( ncol(cellnote)-2)][ is.na(cellnote[,1:( ncol(cellnote)-2)]) ] <- "Pas de données"
        cellnote[, ncol(cellnote)][ is.na(cellnote[,ncol(cellnote)]) ] <- "Non calculable"
        
        d3heatmap( mat, 
                  Rowv=FALSE, Colv=FALSE, 
                  labRow=nomsrow,labCol=nomscol,
                  colors=coul.heatmap[  sort( unique(mat[!is.na(mat)]) ) ], #
                  xaxis_height=230, #axis_height=160,
                  yaxis_width=260,
                  cexRow=0.7,
                  cexCol=0.5,
                  cellnote = cellnote
        )
      }
  })
  

    
    
    output$downloadmat <- downloadHandler(
      filename = function() { 
        paste( 'matrice_', input$RS, '_', 
               max( Datasubset()$Semaine ),
               '_', input$GeoMap,
               '_', input$Age,
               '.csv', sep='') 
      },
      content = function(file) {
        write.csv2( Data_heatmap()[ ,-(ncol(Data_heatmap())-1 ), with=F], file, row.names=F )
      }
    )
    
    onclick("lien_mat", expr=
    { 
      updateTabsetPanel(session, inputId="menu", selected = "questions");
      updateTabsetPanel(session, inputId="menu_questions", selected = "description");
      updateTabsetPanel(session, inputId="menu_description", selected = "mat");
    }  )
    
    
  
  ###################################################################################################
  # ALARM LEVEL MAP 
  ###################################################################################################  

  Data_mapAlarme <- reactive({
    
    # Selection des données selon le type de zone sélectionné
    centro.reg <- get( paste( "centro", input$GeoMap, sep="_" ) )  
    liste_reg <- get( paste( "Liste", input$GeoMap, sep="_" ) ) 
    corresp_nomreg_numreg <- get( paste( "corresp_nom_num", input$GeoMap, sep="_" ) )
    
    # Selection de la date de la carte, par defaut la plus grande inferieure ou egale a input$DateEnd 
    lastd <- max( alarm_level$d[ alarm_level$d <= input$DateEnd ] )
    data.lastd <- alarm_level[ CJ( input$RS, liste_reg, input$Age, lastd ), nomatch=0L ]
    
    # Merge avec les numeros de regions pour le fond de carte
    data.lastd <- data.lastd[ corresp_nomreg_numreg, on=c(Region="code_reg") ]
    noms <- names( data.lastd  )
    firsts <- c( "Region", "Nom_region" )
    setcolorder( data.lastd, c( firsts, noms[ !(noms %in% firsts) ] ) )
    
    data.lastd
  })
    
    
    
  output$mapRegAlarme <- renderPlot({
    
    # Fond de carte
    map.fr <- get( paste( "map.fr", input$GeoMap, sep="_" ) ) 
    
     # Données
    tmp <- Data_mapAlarme()
    
    # Selection des regions, de la date 
    cexMap <- 1#ifelse( length(sources)==3, 1.35, 1 )   
    cexMap2 <- 1#ifelse( length(sources)==3, 1.35*0.9, 1 )   
    
    inset <- -0.5
    omi <- 1.7 #ifelse( length(sources)==1, 1.7, 2)
    par( mfrow=c(1, 1), omi=c(omi,0.2,0.6,0), xpd=NA, mar=c(0,0.5,1,0.5), cex.main=cexMap  ) #mfrow=c(1, length(sources))
    col <- coul.alarme[ tmp$niv_alarme ] 
      
    # Traitement des DOMs
    petites_iles <- c( 1, 2, 4, 6, 7, 8 )
    cond1 <- !is.na( tmp$Proportion2 ) 
    cond2 <- tmp$Region %in% petites_iles 
    lab <- 0 #round( tmp$Proportion2[ cond1 & cond2 ], 0 )
      
    ##### Carte  
    plot( map.fr, col=col  )
    if( any( cond1 & !cond2) ) {
      graphics::text( centro.reg[ cond1 & !cond2, ], labels=round( tmp$Proportion2[ cond1 & !cond2 ], 0), cex=cexMap2 )
    }
    if( any( cond1 & cond2) ) {
       graphics::text( centro.reg[ cond1 & cond2, ], labels=lab, pos=2, offset=1.5, cex=cexMap2 )
    }
      
    
    fill <- c( coul.alarme, "white" )
    leg <- c( tr("no epidemic"), tr("pre-epidemic"), tr("epidemic"), tr("no computable alarm level") )
    legend( 
      par("usr")[1],
      par("usr")[3]-0.05*(par("usr")[4]-par("usr")[3]),
      fill=fill, 
      legend=leg,
      cex=cexMap, inset=c(0,inset), bty="n",
      title=tr("alarm level")
    )
    #box( bty="o")
    #box("outer")
    
    # Overall title
    date_titre <- paste( tr( "week" ), " ", tmp$Semaine[ 1 ], " (", getDayRange( tmp$d[1] ), ")", sep=""
)
    
    
    title( main=paste( tr(input$RS), tr(input$Age), date_titre, sep=" - " ), line=1, outer=T )
  }, width = 800, height=650 )
 
  
  
  # Download the data of the alarm map: alarm levels by zone, for the selected week
  output$downloadmap <- downloadHandler(
    
    filename = function() { 
      cat( "\nCOUCOU", class( input$DateEnd ), input$DateEnd, "\n"  )
      paste( 'carteAlarmes_', input$RS, '_', 
             Data_mapAlarme()$Semaine[1], '_',
             input$GeoMap,
             '_', input$Age,
             '.csv', sep='') 
    },
    content = function(file) {
      write.csv2( subset( Data_mapAlarme(), subset=!is.na(RS),  select=-num_reg ), file, row.names=F )
    }
  )
  
  
  
#   #################################################
#   ## Mise en forme conditionalle des onglets
#   #################################################
   # On affiche le calendrier seulement pour TimeUnit==day
  observe( {
    if( !is.null(input$TimeUnit) ){
      if( input$TimeUnit!="day" & input$menu=="donnees" & input$menu_donnees=="calendrier" ){
        session$sendCustomMessage(type = "desactiveTab",  message="menu_donnees" );
        session$sendCustomMessage(type = "activeTab", message=list( menuToAct="menu_donnees", tabToActID='series_temporelles') );
        
        updateTabsetPanel( session, inputId="menu_donnees", selected='series_temporelles');
      }
      toggle(condition=(input$TimeUnit=="day"), selector = "#menu_donnees li a[data-value=calendrier]")            
    }
   })
  
  
  # On affiche les onglets situation>minimap_alarme et situation>listing_alarme  seulement pour TimeUnit==week
  observe( {
    if( !is.null(input$TimeUnit) ){
      if( input$TimeUnit!="week" & input$menu=="situation" & input$menu_situation %in% c("minimap_alarme", "listing_alarme") ){
        session$sendCustomMessage(type = "desactiveTab", message="menu_situation" );
        session$sendCustomMessage(type = "activeTab", message=list( menuToAct="menu_situation", tabToActID='minimap_proportion') );
        updateTabsetPanel( session, inputId="menu_situation", selected='minimap_proportion');
      }
      toggle(condition=(input$TimeUnit=="week"), selector = "#menu_situation li a[data-value=minimap_alarme]")
      toggle(condition=(input$TimeUnit=="week"), selector = "#menu_situation li a[data-value=listing_alarme]")  
    }
  })
  
  
  
   # On affiche l'onglet determination uniquement pour input.TimeUnit==week 
  observe( {
    if( !is.null(input$TimeUnit) ){
      if( input$TimeUnit!="week" & input$menu=="determination" ){
        session$sendCustomMessage(type = "desactiveTab", message="menu" );
        
        # On active le menu données / series_temporelles (pour cela on descative d'abord l'onglet actif de ce menu)
        session$sendCustomMessage(type = "desactiveTab", message="menu_donnees" );
        session$sendCustomMessage(type = "activeTab", message=list( menuToAct="menu", tabToActID='donnees') );
        session$sendCustomMessage(type = "activeTab", message=list( menuToAct="menu_donnees", tabToActID='series_temporelles') );
        
        updateTabsetPanel( session, inputId="menu", selected='donnees');
        updateTabsetPanel( session, inputId="menu_donnees", selected='series_temporelles');
      }
      toggle(condition=(input$TimeUnit=="week"), selector = "#menu li a[data-value=determination]")            
    }
  })
  


  

  ###################################################################################################
  # DOCUMENTATION  sur les Données 
  ###################################################################################################  
  output$doc_data <- renderUI({
    p( h2("SurSaUD"),
      "Transmission des données de la veille chaque nuit, intégration dans la base SurSaUD tous les matins.",br(),br(),
      
      h2("Sentinelles"),
      "Mise à jour des données le lundi à 14h et le mardi à 10h."
    )
    })


    
###################################################################################################
# DOCUMENTATION   sur les méthodes
###################################################################################################  
output$Documentation <- renderUI({
    p(
      h2("Méthodes statistiques pour la détermination de périodes épidémiques"),
      "Plusieurs méthodes statistiques ont été publiées pour déterminer les périodes épidémiques d'une pathologie saisonnière.",br(),br(),
      
      h3("1.1 Régression linéaire périodique"),
      "Chronologiquement, la première méthode publiée utilise une régression linéaire périodique. Cette méthode est communément 
       appelée méthode de Serfling en référence à l'article publié par Serfling en 1963", a("(Serfling, 1963).",
       href="./Biblio/1963_Serfling_PHR.pdf", target="_blank"),
      "En pratique, cela signifie que la proportion d'actes (pour SOS), de passages (pour SAU) ou le taux d'incidence (pour Sentinelles) 
      du regroupement syndromique (RS) considéré, est expliqué par une régression linaire incluant un intercept, une tendance 
      linaire et un ou plusieurs termes en cosinus/sinus permettant de capter la ou les saisonnalités dans la série temporelle.",br(),br(),
      
      
      h4("Quel est le modèle utilisé dans MASS ?"), 
      h5("Période d'apprentissage"),
      HTML("On calcule la valeur du seuil d'une semaine donnée en utilisant les 261 semaines passées (=5 ans). 
Pour les 261 premières observations, pour lesquelles on ne dispose pas de cet historique, on a ajusté un seul et même modèle 
et généré ensemble les 261 valeurs attendues (niveau de base) et le 261 valeurs du seuil épidémique."),

      h5("Ecrêtage"),
      
      HTML("Les valeurs supérieures au 85<sup>ème</sup> centile, <i>i.e.</i> 15% des valeurs les plus élevées 
de la séries temporelles sont supprimées (paramètre d'écrêtage = 85%). "),
 

    h5("Equation du modèle"),
           
    HTML("Soit <i>y</i> la proportion d'actes (pour SOS), de passages (pour SAU) ou le taux d'incidence (pour Sentinelles), après écrêtage.
    <ul>
    <li> A l'exception des gastro-entérites, l'équation du modèle est la suivante : <BR>
      <center>
      E(y|t)=&alpha;<sub>0</sub> + &alpha;<sub>1</sub>t + &gamma;<sub>1</sub>cos(2&pi;t/52.17) + &delta;<sub>1</sub>sin(2&pi;t/52.17) 
                                                                 + &gamma;<sub>2</sub>cos(4&pi;t/52.17) + &delta;<sub>2</sub>sin(4&pi;t/52.17)
      </center><BR>
      <li> Pour le regroupement syndromique des gastro-entérites, l'équation du modèle est la suivante : <BR>
      <center>
           E(y|t)=&alpha;<sub>0</sub> + &alpha;<sub>1</sub>t + &gamma;<sub>1</sub>cos(2&pi;t/52.17) + &delta;<sub>1</sub>sin(2&pi;t/52.17) 
      </center>
    </ul>"),

    h5("Seuil épidémique"),
           
    HTML("Pour chaque regroupement syndromique, le seuil est la borne supérieure de l'intervalle de 
        prédiction à 95% du niveau de base prédit par le modèle.<BR>"),
      
      "La régression linéaire périodique et les modèles de Markov cachés ont été implémentés sur le site \"Periodic\" développé
      par le réseau Sentinelles (cf onglet 'Liens')", 
      a("(Pelat et al, 2007).",href="./Biblio/2007_Pelat_BMCMedInfo.pdf", target="_blank"),br(),br(),
      
      h3("1.2 Régression linéaire périodique robuste"),
      "La régression linéaire périodique robuste", a ("(Muscatello et al, 2010).", href="./Biblio/2010_Muscatello_robust_serfling_EID.pdf", target="_blank"), 
      "permet de s'affranchir de l'écretage opéré dans la régression précédente. L'objectif ici est de donner à chaque observation, un poids qui 
      sera d'autant plus faible que la valeur est est forte",br(),br(),
      
      h3("1.3 Les modèles de Markov cachés"),
      " Un modèle de Markov caché (Hidden Markov model - HMM) est un modèle qui permet de distinguer dans une série temporelle des états. 
      Ils ont été appliqués pour la première fois sur des séries épidémiologiques en 1999", a("(Le Strat et Carrat, 1999)."  ,href="./Biblio/1999_Le_Strat_SIM.pdf", target="_blank"),br(),br(),
      
      #h3("1.4 La Moving Epidemic Method (MEM)"),
      #"La méthode Moving Epidemic Method (MEM)"  , a("(Vega et al, 2015).",href="./Biblio/2015_Vega_MEM_IRV.pdf", target="_blank"),br(),br()
      

      #h2("2. Méthodes statistiques pour la détection d'événements inhabituels")
      
      h3("1.4 Comment sont calculés les niveaux d'alarme ?", id="calcul_alarmes"),
      HTML("Les niveaux d'alarmes sont calculés à partir des 3 méthodes de détection
           sur toutes les sources de données disponibles, selon la formule suivante :

<BR><BR>  Soit <i>p</i> la proportion d'alarmes parmi les résultats des méthodes disponibles (3 méthode par source au maximum : 
on ne compte ni dans le numérateur ni dans le  dénominateur les méthodes qui n'ont pas pu calculer une alarme),
sur les sources disponibles (3 au maximum : on a donc au maximum 3&times;3=9 résultats de méthodes).
 On classe les semaines comme suit :
          <ul> 
           <li> Couleur verte (phase non-épidémique)      si <i>p</i> < 0.4
            <li> Couleur orange (phase pré ou post-épidémique)   si <i>p</i> >=0.4 et <i>p</i> <1
           <li> Couleur rouge  (phase épidémique)   si <i>p</i> = 1.
</ul>")
    )})
  
    
    
####################################################
##### FAQ
####################################################
    output$FAQ <- renderUI({
      p(br(),
        h3("Comment utiliser les graphiques dynamiques ?"),
        "En passant la souris sur les graphiques, vous pourrez voir, pour chaque unité de temps, les valeurs des séries représentées. 
        Pour sélectionner une partie du graphique, sélectionner cette partie avec la souris (clique gauche maintenu), 
        vous pourrez ainsi zoomer et mieux visualiser la période de temps qui vous intéresse. Vous pouvez zoomer plusieurs fois de suite. 
        Pour revenir à la série entière, double-cliquer sur le graphique.",br(),
        
        h3("Comment exporter les données du tableau ?"),
        "Trois possibilités sont offertes : copy qui permet de copier les données dans le presse-papiers, 
        csv qui permet d'exporter les données en format csv et Excel qui fournit un fichier csv mais qui peut être lu dans Excel. 
        L'export concerne uniquement les données affichées (par défaut 10 lignes). 
        Si vous souhaitez exporter l'ensemble des lignes du tableau, vous devez choisir « All entries » puis exporter.",br(),
        
        h3("Qu'indiquent les jauges ?"),
        "Pour un regroupement syndromique considéré, l'aiguille orange d'une jauge indique, 
        le nombre de passages pour 10,000 passages aux urgences (pour SAU), 
        le nombre de  consultations pour 10,000 consultations (pour SOS Médecins) ou le nombre de consultations pour 10000 habitants 
        (pour le Réseau Sentinelles). Ce nombre est indiqué en bas de la jauge. 
        Sur le contour de la jauge sont indiquées la plus petite valeur et la plus grande valeur jamais observées dans l'historique 
        (soit depuis le 01/01/2010). Trois couleurs sont indiquées : beige, violet clair, violet foncé. 
        Ces couleurs représentent le degré d'intensité des nombres de passages/consultations. 
        Le beige représente les valeurs inférieures à la médiane, le violet clair représente les valeurs comprises entre le 50ème 
        et le 80ème percentile. Le violet foncé représente les valeurs supérieures au 80ème percentile.",br(),
        
        h3("Comment sont calculées les catégories des cartes géographiques ?"),
        HTML("Le nombre observé de passages/consultations pour 10000 passages/consultations est classé en 4 catégories, 
        représentées par des nuances de bleu. Voici comment sont calculées ces catégories.
        <BR>Pour chaque regroupement syndromique et chaque source de données (SAU, SOS ou Sentinelles) on considère la série temporelle 
        constituée des proportions hebdomadaires nationales, observées entre la semaine 2010-01 et la semaine 2014-52.
        <BR>On note <i>min</i> la valeur minimum observée sur cet historique, et <i>Q80</i> la valeur du 80ème percentile. 
        On calcule une quantité <i>q</i> égale à (Q80-min)/3. 
        <ul>
        <li>La catégorie 1 (bleu le plus clair) représente les valeurs comprises dans l'intervalle [0, min+q). </li>
        <li>La catégorie 2 représente les valeurs comprises dans l'intervalle [min+q, min+2q). </li>
        <li>La catégorie 3 représente les valeurs comprises dans l'intervalle [min+2q, Q80). </li>
        <li>La catégorie 4 (bleu foncé) représente les valeurs supérieures au 80ème percentile (Q80). </li>
        </ul>
        La notation « [«  signifie valeur comprise, et la notation « ) » signifie valeur non comprise. 
        Lorsque qu'une région est blanche sans indication d'un nombre, cela signifie qu'aucune donnée n'est disponible"),
      
        
        h3("L'outil MASS travaille à établissements non constants pour SAU : quel impact ?"),
        HTML("On pense, sans pouvoir le démontrer formellement, qu'à un niveau régional, il y a peu d'impact 
        sur la détermination des périodes épidémiques à travailler à établissements non constants versus 
        établissements constants. L'avantage de travailler à établissements non constants est de considérer 
        l'ensemble des données, c'est-à-dire les données transmises par l'ensemble des établissements participants. 
        Ne travailler qu'avec des établissements constants ferait perdre beaucoup de données. 
        Pour que la détermination de périodes épidémiques soit impactée, il faudrait que des établissements 
        qui arrivent ou qui sortent du système de surveillance ou bien qui ne transmettent plus leurs données 
        soient : (1) de gros établissements, et (2) aient une proportion (du regroupement syndromique considéré) 
        très différente des autres établissements de la région.
        En revanche, travailler à établissements non constants peut avoir un impact plus marqué 
        à un niveau géographique plus fin, tel que le département."),
        
        h3("Sur quels critères pourrait-on annoncer le pic d'une épidémie à S-1 ou S-2 ?"),
        HTML("L'application MASS permet de déterminer les débuts et fins d'épidémies.
          Elle n'incorpore pas pour le moment de méthode permettant de faire de la prévision 
        à court terme : elle ne permet donc pas de prédire le pic."),
        
        h3("Pourquoi certains onglets disparaissent-ils ou apparaissent-ils ?"),
        HTML("Certains onglets ne sont disponibles que pour une unité de temps donnée, à choisir dans la barre de gauche, tout en bas : 
             <ul>
                <li> l'onglet \"Calendrier\" du menu Données n'est disponible que si l'unité de temps choisie est le jour.
                <li> les onglets \"Cartes des alarmes\" et \"Listing des alarmes\"du menu Situation épidémiologique ne sont disponibles
                     que si l'unité de temps choisie est la semaine.
                <li> l'onglet \"Détermination des périodes épidémiques\" du menu général n'est disponible 
                     que si l'unité de temps choisie est la semaine.
             </ul>
             ")
      )  
      })
    
  
  ####################################
  # DESCRIPTION DES VARIABLES POUR LES FICHIERS TELECHARGEABLES
  #############################
  
  output$desc_met <- DT::renderDataTable({
    DT::datatable( descMet, 
                   rownames = FALSE,
                   extensions = c("Buttons"),
                   options=list(
                     #lengthMenu = list(c(10, 25, 50, -1), c('10', '25','50', 'All')),
                     pageLength = 50,
                     orderClasses = TRUE,
                     dom = 'Blfrtip',
                     buttons = c('copy', 'csv', 'excel', 'pdf', 'print') 
                   )
    )
  }) 
  
  output$desc_niv <- DT::renderDataTable({
    DT::datatable( descNiv, 
                   rownames = FALSE,
                   extensions = c("Buttons"),
                   options=list(
                     #lengthMenu = list(c(10, 25, 50, -1), c('10', '25','50', 'All')),
                     pageLength = 50,
                     orderClasses = TRUE,
                     dom = 'Blfrtip',
                     buttons = c('copy', 'csv', 'excel', 'pdf', 'print') 
                   )
    )
  }) 
  
  output$desc_mat <- DT::renderDataTable({
    DT::datatable( descMat, 
                   rownames = FALSE,
                   extensions = c("Buttons"),
                   options=list(
                     #lengthMenu = list(c(10, 25, 50, -1), c('10', '25','50', 'All')),
                     pageLength = 50,
                     orderClasses = TRUE,
                     dom = 'Blfrtip',
                     buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
                   )
    )
  }) 
  
  onclick("lien_niv2", expr=
  { 
    updateTabsetPanel(session, inputId="menu", selected = "questions");
    updateTabsetPanel(session, inputId="menu_questions", selected = "description");
    updateTabsetPanel(session, inputId="menu_description", selected = "niv");
  }
  )  
  
  
  ####################################################
  ##### Quoi de neuf ?
  ####################################################
  output$quoi_de_neuf<- renderUI ({
    p(    h3("10/05/2016"),
          HTML("<h4> Les méthodes HMM et Serfling robuste évoluent </h4>
               <ul>
                <li> <b>HMM</b> : auparavant, la présence d'une ou plusieurs données manquantes entre
                      deux observations
                     empéchait l'exécution de la méthode sur toute la série. Ce n'est plus le cas.
                     Pour ce faire, nous appliquons la méthode sur la série \"imputée\", c'est à dire dans laquelle 
                      chaque \"trou\" est remplacé par la moyenne
                     de l'observation précédente et de l'observation suivante.<BR><BR>

                <li> <b>Serfling robuste</b> : auparavant la fonction lmrob du package R 
                      robustbase était utilisée et parfois ne fonctionnait pas, générant 
                      des données manquantes pour les alarmes statistiques.
                      La fonction rlm du package MASS (rien à voir avec notre MASS !) est 
                      désormais utilisée car elle génère moins de données manquantes.
               </ul>
               
               <h4> Téléchargement des seuils de détection et des alarmes </h4>
              <ul>
                <li> dans l'onglet Détermination des périodes épidémiques,
                <li> grâce à des boutons de téléchargement dans la barre de gauche.
              </ul>


               <h4> Semaine sélectionnée par défaut : la dernière semaine <u>complète</u> </h4>
               <ul>
               <li> La semaine suivante reste accessible via le calendrier dans la barre de gauche. 
                    Elle porte la mention INCOMPLETE accolées aux dates de début (lundi) et fin (dimanche).
               </ul>
               ")
    )
  } )
  


###################################################################################################
# LIENS VERS SITES WEB   
###################################################################################################  
output$ReseauSentinelles <- renderUI({
  HTML(
    '<iframe src="https://websenti.u707.jussieu.fr/sentiweb/?page=bulletin" width="900" height="690">
    <p>Your browser does not support iframes.</p>
    </iframe>'
  )
})

output$Periodic <- renderUI({
  HTML(
    '<iframe src="http://marne.u707.jussieu.fr/periodic/" width="900" height="690">
    <p>Your browser does not support iframes.</p>
    </iframe>'
  )
})


output$Irsan <- renderUI({
  HTML(
    '<iframe src="http://recherche.irsan.fr/" width="900" height="690">
    <p>Your browser does not support iframes.</p>
    </iframe>'
  )
})



})

  
