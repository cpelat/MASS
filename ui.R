# ----------------------------------------------------------------------------------------------------
# ----------------------------------------------------------------------------------------------------
# Programme UI
# ----------------------------------------------------------------------------------------------------
# ----------------------------------------------------------------------------------------------------


desactiveTabScript <- 'Shiny.addCustomMessageHandler("desactiveTab", 
/* Nouvelle fonction desactiveTab : desactive seulement */

function( menuToDesact ) { 
  /* Desactiver l onglet actif et le contenu tabpanel actif  en meme temps */
  var activeTabPanel = "#" + menuToDesact;

  /* tab a desactiver */
   $(activeTabPanel).children("li").filter(".active").removeClass("active");

  /*var calend = "#" + message.menuToDesact + " li a[data-value=\\"" + message.tabToDesactID + "\\"]";
  $(calend).parent().removeClass("active");*/

  /* Contenu du tab a desactiver */
  /*var calendtab = "div[data-value=\\"" + message.tabToDesactID + "\\"]";*/
  var activetabcontent = $(activeTabPanel).parent().children(".tab-content").children(".tab-pane").filter(".active");
  //alert(   activetabcontent.length );
  $(activetabcontent).removeClass("active");


  /* NB: On n utilise pas .find() car desactive les panels qu il faut garder actif en background */
  /*css( "border", "9px solid red" );*/
  /* On recherche pour cela tous les elements actifs dans le menu et on leur enleve la classe active */
  /*
  var sel = $(activeTabPanel).parent().find(".active");
  sel.removeClass("active");
  */
}

);'



activeTabScript <- 'Shiny.addCustomMessageHandler("activeTab", 
/* Nouvelle fonction activeTab : active seulement */

  function( message ) { 
    /* tab a activer */
      var selector = "#" + message.menuToAct + " li a[data-value=\\"" + message.tabToActID + "\\"]";
      $(selector).parent().addClass("active");
      
    /* tab-content (Contenu du tab) a activer */
    var selectortab = "div[data-value=\\"" + message.tabToActID + "\\"]";
    $(selectortab).addClass("active"); 
  }

);'




goToAncreScript <- 'Shiny.addCustomMessageHandler("goToAncre", 
function( message ) { 

/* Pour tous les liens commençant par #.
	$("a[href^=\\"#\\"]").click(function (e) {
		// On annule le comportement initial au cas ou la base soit différente de la page courante.
		e.preventDefault(); 
*/				
 	// On ajoute le hash dans l url.
 	//window.location.hash = message.ancre ;
 		
 	// Une fois en place on va forcer l affichage 40 pixels plus haut.
 	//$(window).scrollTop( $(window).scrollTop() - 40 );

  
  setTimeout( function(){
  var selector = "#" + message.ancre;
  var pos=$(selector).position().top   ;
   // $(window).scrollTop( $(selector).scrollTop() );
  $(window).scrollTop( pos );
  }, 400);

});'




# desactiveMenuScript <- 'Shiny.addCustomMessageHandler("desactiveMenu", 
# function( menuToDesact ) { 
# 
# /* On desactive le tab-pane qui contient le menu */
# var activeTabPanel = "#" + menuToDesact;
# 
# var sel2 = $(activeTabPanel).parents().filter(".tab-pane");
# alert(   sel2.length );
# sel2.removeClass("active");
# }
# );'

# activeTabScript <- 'Shiny.addCustomMessageHandler("activeTab", 
# 
# function( message ) { 
#  
#   /* Desactiver l onglet actif et le contenu tabpanel actif  en meme temps */
#   /* On recherche pour cela tous les elements actifs dans le menu et on leur enleve la classe active */
#   var activeTabPanel = "#" + message.menuToDesact;
#   var sel = $(activeTabPanel).parent().find(".active");
#   /*alert(   sel.length );*//*css( "border", "9px solid red" )*/
#   sel.removeClass("active");
# 
#   /*-----------------------------------------------------------------------------------*/
#   /* tab a activer */
#   /*var selector = "#menu_donnees li a[data-value=\\"" + message.tabToActID + "\\"]";*/
#   var selector = "#" + message.menuToAct + " li a[data-value=\\"" + message.tabToActID + "\\"]";
#   $(selector).parent().addClass("active");
# 
#   /* Contenu du tab a activer */
#   var selectortab = "div[data-value=\\"" + message.tabToActID + "\\"]";
#   $(selectortab).addClass("active"); 
# }
# );'



# largeur des dygraphs des seuils
w <- "95%"



# Condition pour l'affichage du side panel "Zone géographique" à la place du menu "Cartographier par" :
cond <- "( input.menu=='donnees' & input.menu_donnees=='cartes')
| ( input.menu=='determination' & (input.menu_determination=='carte_alarme' | input.menu_determination=='matrice_alarme') )
| ( input.menu=='situation' & (input.menu_situation=='minimap_proportion' | input.menu_situation=='minimap_alarme') )"





shinyUI( fluidPage( 
  tags$head( tags$script(activeTabScript), tags$script(desactiveTabScript), tags$script(goToAncreScript) ),
  useShinyjs(),
  
  headerPanel( uiOutput( "uiSBtitre" ), windowTitle="MASS - v2015"  ),
  
  div( id="flagDiv", uiOutput("uiSBlang_message") ),
  
  sidebarLayout(
    
    sidebarPanel( 
      
      #selectizeInput("language", uiOutput('uiSBlanguage'), choices = c("fr", "en") ),
      
      # Barre de gauche pour la page Questions et Liens
      conditionalPanel( condition="(input.menu=='questions' | input.menu=='liens')" ,
                        wellPanel( 
                          HTML('<span style="color:#181866; font-weight: bold;">  <i class="fa fa-envelope-o"></i> Contact </span>
                               <BR><BR> <b>Pour un problème ou une question non traitée dans la FAQ :
                               </b> <BR>
                               <a href="mailto:camille.pelat@santepubliquefrance.fr%3B%20yann.lestrat@santepubliquefrance.fr &subject=Question%20sur%20MASS"> 
                               Camille Pelat et Yann Le Strat </a>
                               <BR><BR>
                               <span style="font-weight: bold;">Pour une suggestion d\'évolution :</span> <BR>
                               <a href="mailto:MASS-users@santepubliquefrance.sante.fr &subject=Suggestion%20d%27évolution%20de%20MASS"> 
                               groupe des utilisateurs de MASS</a>'
                          )
                        )
      ),
      
#       # Barre de gauche pour la page Liens
#       conditionalPanel( condition="input.menu=='liens'" ,
#                         wellPanel( HTML("TEST") )
#       ),
      
      conditionalPanel( condition="!( input.menu=='questions' | input.menu=='liens' )" ,
      conditionalPanel( condition="!( input.menu=='situation' )" ,
                        
                        wellPanel(
                          fluidRow(   
                            column(12,
                                   selectizeInput("RS", uiOutput( "uiSBsyndgroup" ), choices = Liste_RS )
                                   ),
                            
                            br(),
                            conditionalPanel( "!(input.menu=='determination')", column( 6, uiOutput('Source') ) ),
                            
                            column( 6, uiOutput('Age') )
                            
                            )
                          )
                                         
      ),
      
      
      wellPanel(
               conditionalPanel( condition= paste( "!(", cond, ")" ),
                                   uiOutput('Geo')
                  ),
                 
                 conditionalPanel( condition=cond,
                                   uiOutput('GeoMap')
                 ),	      
          
                 conditionalPanel( condition = paste( "input.Geo == 'regionsN' & !(", cond, ")" ),
                                  selectInput("RegionN", htmlOutput( "label_newRegion" ), choices = "" ) 
                 ),
                 conditionalPanel( condition = paste( "input.Geo == 'regionsA' & !(", cond, ")" ),
                                  selectInput("RegionA", htmlOutput( "label_oldRegion" ), choices = "" ) 
                 )

        
      ),
      
      
      wellPanel(
        fluidRow(
          
          conditionalPanel( condition=paste( "!(", cond, ")" ),
                            div( class="divleft1haut", uiOutput("uiSBDateStart") ) 
                            ),
          
          div( class="divleft2haut", uiOutput("uiSBDateEnd") )
          
        )
        
      ),


    conditionalPanel( condition="!( input.menu=='situation' & (input.menu_situation=='minimap_alarme' | input.menu_situation=='listing_alarme') )",
      wellPanel(
        
        conditionalPanel( condition="!(input.menu=='determination' )",
                          radioButtons("TimeUnit",
                                       htmlOutput("label_TimeUnit"),
                                       list( Jour="day", Semaine="week", Mois="month" ),
                                       inline=TRUE, selected = "week"	
       )



        ),
        
        conditionalPanel( condition="input.menu=='determination' & input.menu_determination=='seuils'",
                          # fluidRow(
                          #   column(  uiOutput("uiSBdownloadmet_title"),
                          #           uiOutput("uiSBdownloadmet_button"),
                          #           uiOutput("uiSBdownloadmet_legend"),
                          #           width=6
                          #   ),
                          #   column(
                          #     uiOutput("uiSBdownloadlev1_title"),
                          #     uiOutput("uiSBdownloadlev1_button"),
                          #     uiOutput("uiSBdownloadlev1_legend"),
                          #     width=6
                          #   )
                          # )
                          fluidRow(
                          div( class="divleft3haut",
                            uiOutput("uiSBdownloadmet_title"),
                                     uiOutput("uiSBdownloadmet_button"),
                                     uiOutput("uiSBdownloadmet_legend")

                          ),
                          
                          div( class="divleft4haut", 
                               uiOutput("uiSBdownloadlev1_title"),
                                      uiOutput("uiSBdownloadlev1_button"),
                                      uiOutput("uiSBdownloadlev1_legend")

                          )
                          )
                         
        ),
        
        conditionalPanel( condition="input.menu=='determination' & input.menu_determination=='matrice_alarme'",
                          
                          uiOutput("uiSBdownloadmat_title"),
                          uiOutput("uiSBdownloadmat_button"),
                          uiOutput("uiSBdownloadmat_legend")
        ),
        
        conditionalPanel( condition="input.menu=='determination' & input.menu_determination=='carte_alarme'",
                          
                          uiOutput("uiSBdownloadmap_title"),
                          uiOutput("uiSBdownloadmap_button"),
                          uiOutput("uiSBdownloadmap_legend")
                         # HTML( "<BR><a class='menu_links' id='lien_mapAlarme'>Description des variables</a>" )
                          
        )
      ) 
    )
,
     
    uiOutput('uiSBdata_upload')
  ),

      width=3 ),



    
mainPanel(

  tabsetPanel( id="menu",
               tabPanel( htmlOutput("uiSBsituation"), value="situation", 
                         
                         tabsetPanel( id="menu_situation",
                                      
                                      tabPanel( htmlOutput("uiSBminimapsAlarm"), value='minimap_alarme',
                                                HTML("<TABLE border=0><TR><TH>"),  htmlOutput("time_alarm_maps"), 
                                                HTML( "</TH><TH>"),
                                                HTML( "<TABLE width=100% class='HeaderCartes' >
                                                              <TR><TH width='50%'>"), htmlOutput( "label_allage_alarm" ), HTML("</TH> 
                                                                  <TH>"), htmlOutput( "label_younger2_alarm" ), HTML("</TH></TR> 
                                                             </TABLE>" ),
                                                HTML( "</TH></TR>"),
                                                
                                                
                                                
                                                HTML("<TR><TD class='RS'>"), htmlOutput( "label_alarm_map_1" ), HTML("</TD><TD>"),  
                                                shiny::div( id = "petite_carte_alarme_1",                  
                                                            plotOutput("mapAlarme_1", 
                                                                       width="100%", height="100%" ), 
                                                            class="menu_links" ),
                                                HTML("</TD></TR>"),
                                                
                                                HTML("<TR><TD class='RS'>"), htmlOutput( "label_alarm_map_2" ), HTML("</TD><TD>"),   
                                                shiny::div( id = "petite_carte_alarme_2", 
                                                            plotOutput("mapAlarme_2", width="100%", height="100%"), #300px
                                                            class="menu_links" ),
                                                HTML("</TD></TR>"),
                                                
                                                HTML("<TR><TD class='RS'>"), htmlOutput( "label_alarm_map_3" ), HTML("</TD><TD>
                                                     <TABLE>
                                                      <TR>
                                                          <TD>"),
                                                
                                                shiny::div( id = "petite_carte_alarme_3_1", 
                                                             plotOutput("mapAlarme_3_1", width="100%", height="100%" ), 
                                                             class="menu_links" ),
                                                
                                                HTML(" </TD><TD>"),  
                                              
                                                shiny::div( id = "petite_carte_alarme_3_2", 
                                                            plotOutput("mapAlarme_3_2", width="100%", height="100%" ), 
                                                            class="menu_links" ),
                                                
                                                HTML(" </TD></TR></TABLE>"), 
                                                
                                                HTML("</TD></TR>"),
                                                
                                                HTML("<TR><TD class='RS'>"), htmlOutput( "label_alarm_map_4" ), HTML("</TD><TD>"),   
                                                shiny::div( id = "petite_carte_alarme_4", 
                                                            plotOutput("mapAlarme_4", width="100%", height="100%" ), 
                                                            class="menu_links" ),
                                                HTML("</TD></TR>"),
                                                
                                                HTML("<TR><TD class='RS'>"), htmlOutput( "label_alarm_map_5" ), HTML("</TD><TD>"),    
                                                shiny::div( id = "petite_carte_alarme_5", 
                                                            plotOutput("mapAlarme_5", width="100%", height="100%" ), 
                                                            class="menu_links" ),
                                                HTML("</TD></TR>"),
                                                
                                                HTML("<TR><TD class='RS'>"), htmlOutput( "label_alarm_map_6" ), HTML("</TD><TD>"),   
                                                shiny::div( id = "petite_carte_alarme_6", 
                                                            plotOutput("mapAlarme_6", width="100%", height="100%" ), 
                                                            class="menu_links" ),
                                                HTML("</TD></TR>"),
                                                
                                                HTML("</TABLE>")
                                      ), 
                                      
                                      
                                      tabPanel( htmlOutput("uiSBminimapsProp"), value='minimap_proportion',
                                                HTML("<TABLE ><TR><TH>"), htmlOutput("time_prop_maps"), 
                                                HTML( "</TH><TH>"),  
                                                htmlOutput(outputId="HeaderTableAccueil"),
                                                HTML("</TH></TR>"),
                                                
                                                HTML("<TR><TD class='RS'>"), htmlOutput( "label_prop_map_1" ), HTML("</TD><TD>"),
                                                shiny::div( id = "petite_carte_1",                  
                                                            plotOutput("mapAccueil_1", 
                                                                       width="100%", height="100%" ), 
                                                            class="menu_links" ),
                                                HTML("</TD></TR>"),
                                                
                                                HTML("<TR><TD class='RS'>"), htmlOutput( "label_prop_map_2" ), HTML("</TD><TD>"), 
                                                shiny::div( id = "petite_carte_2", 
                                                            plotOutput("mapAccueil_2", width="100%", height="100%"), #300px
                                                            class="menu_links" ),
                                                HTML("</TD></TR>"),
                                                
                                                HTML("<TR><TD class='RS'>"), htmlOutput( "label_prop_map_3" ), HTML("</TD><TD>"),  
                                                shiny::div( id = "petite_carte_3", 
                                                            plotOutput("mapAccueil_3", width="100%", height="100%" ), 
                                                            class="menu_links" ),
                                                HTML("</TD></TR>"),
                                                
                                                HTML("<TR><TD class='RS'>"), htmlOutput( "label_prop_map_4" ), HTML("</TD><TD>"), 
                                                shiny::div( id = "petite_carte_4", 
                                                            plotOutput("mapAccueil_4", width="100%", height="100%" ), 
                                                            class="menu_links" ),
                                                HTML("</TD></TR>"),
                                                
                                                HTML("<TR><TD class='RS'>"), htmlOutput( "label_prop_map_5" ), HTML("</TD><TD>"), 
                                                shiny::div( id = "petite_carte_5", 
                                                            plotOutput("mapAccueil_5", width="100%", height="100%" ), 
                                                            class="menu_links" ),
                                                HTML("</TD></TR>"),
                                                
                                                HTML("<TR><TD class='RS'>"), htmlOutput( "label_prop_map_6" ), HTML("</TD><TD>"),
                                                shiny::div( id = "petite_carte_6", 
                                                            plotOutput("mapAccueil_6", width="100%", height="100%" ), 
                                                            class="menu_links" ),
                                                HTML("</TD></TR>"),
                                                
                                                HTML("</TABLE>")
                                      ), 
                                      
                                      tabPanel( htmlOutput("uiSBalarmList"), value="listing_alarme", DT::dataTableOutput('ListingAlarmes'), htmlOutput("FootnoteAlarmList") ) #, htmlOutput("TitreListing")
                         ) 
               ),
               

                   tabPanel( htmlOutput("uiSBdata"), value="donnees",
                             tabsetPanel( id="menu_donnees",                                                                  
                                          tabPanel( htmlOutput("uiSBtimeSeries"), value="series_temporelles", dygraphOutput("dygraph_series" ) ),
                                          tabPanel( htmlOutput("uiSBtable"), value='tableau', DT::dataTableOutput('DataTable') ), #, htmlOutput("TitreDT")
                                          tabPanel( htmlOutput("uiSBgauges"), value='jauge', htmlOutput("Jauge"), htmlOutput("legend_jauge") ),
                                          tabPanel( htmlOutput("uiSBmaps"), value='cartes',  plotOutput("mapReg2", width = "100%", height="630px" ) ),
                                          tabPanel( htmlOutput("uiSBcalendar"), value='calendrier', htmlOutput("DataCal") )
                                        ) 
                             ),
                   



                    tabPanel( htmlOutput("uiSBdetermination"), value="determination", #icon("signal")
                             tabsetPanel( id="menu_determination",
                                          tabPanel( htmlOutput("uiSBthresholds"), value='seuils',
                                                   tabsetPanel( id="menu_seuils",
                                                                tabPanel( htmlOutput("uiSBserfling"), value="seuil_serfling", 
                                                                          dygraphOutput("dygraph_Serfling_SAU", height="210px", width = w ), HTML("<BR>"),
                                                                          dygraphOutput("dygraph_Serfling_SOS", height="210px", width = w ), HTML("<BR>"),
                                                                          dygraphOutput("dygraph_Serfling_Sentinelles", height="210px", width = w ),
                                                                          htmlOutput("legend_dygraph_Serfling"),
                                                                          textOutput("legendDivID_Serfling_SAU"),
                                                                          textOutput("legendDivID_Serfling_SOS"),
                                                                          textOutput("legendDivID_Serfling_Sentinelles")
                                                                ),
                                                                tabPanel( htmlOutput("uiSBrobustSerfling"), value="seuil_serfling_robuste",  
                                                                          dygraphOutput( "dygraph_Serfling_robuste_SAU", height="210px", width = w ), HTML("<BR>"),
                                                                          dygraphOutput("dygraph_Serfling_robuste_SOS", height="210px", width = w ), HTML("<BR>"),
                                                                          dygraphOutput("dygraph_Serfling_robuste_Sentinelles", height="210px", width = w ),
                                                                          htmlOutput("legend_dygraph_Serfling_robuste"),
                                                                          textOutput("legendDivID_Serfling_robuste_SAU"),
                                                                          textOutput("legendDivID_Serfling_robuste_SOS"),
                                                                          textOutput("legendDivID_Serfling_robuste_Sentinelles")
                                                                ),
                                                                tabPanel( htmlOutput("uiSBHMM"), value="seuil_HMM",
                                                                          dygraphOutput( "dygraph_HMM_SAU", height="210px", width = w ), HTML("<BR>"),
                                                                          dygraphOutput("dygraph_HMM_SOS", height="210px", width = w ), HTML("<BR>"),
                                                                          dygraphOutput("dygraph_HMM_Sentinelles", height="210px", width = w ),
                                                                          htmlOutput("legend_dygraph_HMM"),
                                                                          textOutput("legendDivID_HMM_SAU"),
                                                                          textOutput("legendDivID_HMM_SOS"),
                                                                          textOutput("legendDivID_HMM_Sentinelles")
                                                                )
                                                                
                                                   )
                                          ),
                                          tabPanel( htmlOutput("uiSBalarmMatrix"), value='matrice_alarme', htmlOutput("TitreHeatmap"), d3heatmapOutput("heatmap", height="700px"), #height = "660px"
                                                    htmlOutput("FootnoteHeatmap")),
                                          tabPanel( htmlOutput("uiSBalarmLevelMap"), value='carte_alarme', plotOutput("mapRegAlarme", width = "100%", height="630px" ))
                             )
                    ),
               
               
                   tabPanel( htmlOutput("uiSBhelp"), value="questions",
                            tabsetPanel( id="menu_questions",
                              tabPanel( htmlOutput("uiSBmethods"), value='methodes', htmlOutput("Documentation")),
                              tabPanel( htmlOutput("uiSBdataDesc"), value='donnees', htmlOutput("doc_data")),
                              tabPanel('FAQ',  value='FAQ', icon = icon("question-circle"), htmlOutput("FAQ")),
                              tabPanel( htmlOutput("uiSBdataResults"), value="description",
                                        tabsetPanel( id="menu_description",
                                          tabPanel( htmlOutput("uiSBdetectionResults"), value="met", DT::dataTableOutput('desc_met') ),
                                          tabPanel( htmlOutput("uiSBalarmLevels"), value="niv", DT::dataTableOutput('desc_niv') ),
                                          tabPanel( htmlOutput("uiSBalarmMatrixInfo"), value="mat", DT::dataTableOutput('desc_mat'),
                                                    HTML("*prop_alarmes est calculé comme expliqué <a class='menu_links' id='lien_niv2'>ici</a>")
                                                    )
                                        ) ),
                             tabPanel( htmlOutput("uiSBwhatisnew"), value='quoi_de_neuf', htmlOutput("quoi_de_neuf"))  #icon("bullhorn")         
                              
                            )
                   ),
                   tabPanel( htmlOutput("uiSBlinks"), value="liens", 
                            tabsetPanel(
                              tabPanel(HTML('Site R&eacute;seau Sentinelles'),icon = icon("external-link-square"),uiOutput("ReseauSentinelles")),
                              tabPanel(HTML('Site Periodic'),icon = icon("external-link-square"),uiOutput("Periodic")),
                              tabPanel(HTML('Site Irsan')   ,icon = icon("external-link-square"),uiOutput("Irsan"))
                            )             
                   )
      ),


conditionalPanel( "$('#mapAlarme_1').is(':empty')",
                  id='progressIndicator',
                  "Loading data...",
                  HTML('<BR><img src="img/ajax-loader.gif" alt="loading" >')
),
conditionalPanel( "$('html').hasClass('shiny-busy')",
                  id='progressIndicator',
                  uiOutput( "uiSBloading" ),
                  HTML('<img src="img/ajax-loader.gif" alt="loading" >')
)


, width = 9)),
theme = "css/MASS.css"
)
)

