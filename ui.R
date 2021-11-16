
##############################
###
### UI 
###
##############################


##############################
### detect mobile device: mobileDetect
##############################

mobileDetect <- function(inputId, value = 0) {
  tagList(
    singleton(tags$head(tags$script(src = "JS/mobile.js"))),
    tags$input(id = inputId,
               class = "mobile-element",
               type = "hidden")
  )
}


##############################
### options for spinner
##############################

options(spinner.color=js, spinner.color.background="#ffffff", spinner.size=2)


##############################
### header
##############################

header <- dashboardHeader(title = hidden(div(id="GOie", tags$b("Greifswalder Oie"))),
                          dropdownMenu(type = "message", icon = icon("info"),
                                       headerText = tags$b("Informationen zum Datenstand"),
                                       shinydashboardPlus::messageItem("Beringung", textOutput("ring_info"), icon= icon("ring"), inputId="openModal1"),
                                       shinydashboardPlus::messageItem("Beobachtungen", textOutput("beob_info"), icon= icon("binoculars"), inputId="openModal2"),
                                       messageItem("Wiederfunde", textOutput("rec_info"), icon = icon("search-location"))
                          ),
                          dropdownMenu(type = 'message', headerText = tags$b("Teilen"),
                                      icon = icon("share-alt"), badgeStatus = NULL,
                                      messageItem(
                                        from = 'Twitter',
                                        message = "",
                                        icon = icon("twitter"),
                                        href = "https://twitter.com/intent/tweet?url=https://g-rppl.shinyapps.io/rec_map/&text=Greifswalder%20Oie%20BirdObs%20datacenter%20dashboard"
                                      ),
                                      messageItem(
                                        from = 'Facebook',
                                        message = "",
                                        icon = icon("facebook"),
                                        href = "https://www.facebook.com/sharer/sharer.php?u=https://g-rppl.shinyapps.io/rec_map/"
                                      ),
                                      messageItem(
                                        from = 'Linkedin',
                                        message = "",
                                        icon = icon("linkedin"),
                                        href = "https://www.linkedin.com/shareArticle?mini=true&url=https://g-rppl.shinyapps.io/rec_map/"
                                      ),
                                      messageItem(
                                        from = 'EMail',
                                        message = "",
                                        icon = tags$i(class = "fas fa-envelope", style="font-size: 12px"),
                                        href = "mailto:info@example.com?&subject=&cc=&bcc=&body=https://g-rppl.shinyapps.io/rec_map/%0AGreifswalder%20Oie%20BirdObs%20datacenter%20dashboard"
                                      )
                          )
)


##############################
### sidebar
##############################

sidebar <- dashboardSidebar(
  tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "CSS/styles.css")),   # add css styles
  
  sidebarMenu(id = "sidebar", 
    menuItem("Startseite", tabName = "Startseite", icon = icon("home")),
    menuItem("Artenliste", tabName = "Artenliste", icon = icon("clipboard-list"),
             badgeLabel = HTML(paste(icon("binoculars"), "/", icon("ring"))), badgeColor = "green"),
    menuItem("Brutvögel", tabName = "breeding", icon = icon("egg"),
             badgeLabel = icon("binoculars"), badgeColor = "green"),
      hidden(div(id="sidbar_bvk", conditionalPanel("input.sidebar === 'breeding'",
                           actionButton("compare_bvk", "Arten vergleichen", icon("sync-alt")),
                           hidden(actionButton("compare_bvk_2", "Anzahl Arten anzeigen", icon("chart-line")))
                           )
      )),
    menuItem("Beringungszahlen", tabName = "ring_count", icon = icon("chart-bar"),
             badgeLabel = icon("ring"), badgeColor = "green"),
      hidden(div(id="sidbar_ring_count", conditionalPanel("input.sidebar === 'ring_count'",
                           selectInput("select_season", "Saison", c("gesamtes Jahr", "Frühjahr", "Herbst")),
                           actionButton("compare_ring_count", "Arten vergleichen", icon("sync-alt")),
                           hidden(actionButton("compare_ring_count_2", "Gesamtzahl anzeigen", icon("chart-bar")))
                           )
      )),
    menuItem("Jahresübersicht", tabName = "year_data", icon = icon("chart-line"),
            badgeLabel = HTML(paste(icon("ring"), "/", icon("binoculars"))), badgeColor = "green"),
      hidden(div(id="sidbar_year_data", conditionalPanel("input.sidebar === 'year_data'",
                           selectInput("year", "Jahr", 2000:year_end, selected = year_end),
                           actionButton("compare_year", "Arten vergleichen", icon("sync-alt")),
                           hidden(actionButton("compare_year_2", "Gesamtzahl anzeigen", icon("chart-bar")))
                           )
      )),
    menuItem("Phänologie", tabName = "pheno", icon = icon("calendar"),
             badgeLabel = HTML(paste(icon("ring"), "/", icon("binoculars"))), badgeColor = "green"),
      hidden(div(id="sidbar_pheno", conditionalPanel("input.sidebar === 'pheno'",
                           selectInput("sp", "Art", artenl$Art[artenl$Dz_Status!="Ausnahmeerscheinung" & artenl$Art!="Alpenbirkenzeisig"], 
                                       selected = "Rotkehlchen"),
                           actionButton("compare_pheno", "Arten vergleichen", icon("sync-alt")),
                           hidden(div(id = "show_sp_2",
                              actionButton("compare_pheno_2", "nur eine Art", icon("caret-up")),
                              selectInput("sp_2", "Art 2", artenl$Art[artenl$Dz_Status!="Ausnahmeerscheinung" & artenl$Art!="Alpenbirkenzeisig"],
                                          selected = "Rotkehlchen")
                              )
                           )
      ))),
    menuItem("Seltenheiten", tabName = "vag", icon = icon("dove"),
             badgeLabel =  HTML(paste(icon("binoculars"), "/", icon("ring"))), badgeColor = "green"),
      hidden(div(id="sidbar_vag", conditionalPanel("input.sidebar === 'vag'",
                           selectInput("vag_sp", "Art", artenl$Art[artenl$Dz_Status=="Ausnahmeerscheinung"], 
                                       selected = "Bergbraunelle"))
      )),
    menuItem("Wiederfundkarte", tabName = "Wiederfundkarte", icon = icon("globe-europe"),
             badgeLabel = HTML(paste(icon("ring"), "/", icon("binoculars"))), badgeColor = "green"),
      hidden(div(id="sidbar_Wiederfundkarte", 
                 conditionalPanel("input.sidebar === 'Wiederfundkarte'",
                   selectInput("Art", "Art", unique(data$Art[order(data$Nr)]), selected = "Rotkehlchen"),
                   actionButton("compare_Wiederfundkarte", "Arten vergleichen", icon("sync-alt")),
                   hidden(div(id = "show_Art_2",
                              actionButton("compare_Wiederfundkarte_2", "nur eine Art", icon("caret-up")),
                              selectInput("Art_2", "Art 2", unique(data$Art[order(data$Nr)]), selected =  "Rotkehlchen")
                   )),
                   sliderTextInput("season_ring", "Monat der Beringung", month_name, selected=month_name[c(1,12)]),
                   sliderTextInput("season_rec", "Monat des Wiederfundes", month_name, selected=month_name[c(1,12)]),
                   selectInput("map_color", "Anzeige", choices = c("Beringungsort", "Entfernung", "Tagesdifferenz", "Zuggeschwindigkeit", "Fundzustand")),
                   conditionalPanel("input.map_color === 'Beringungsort'",
                     fluidRow(div(id="customsidebar", 
                      
                        prettySwitch(inputId = "direct", label = "direkte Wiederfunde",
                                     fill = TRUE, status = "primary")
                        ),
                        HTML(paste0(
                           "<div class='infohover'>",
                             "<i class='fa fa-question-circle'></i>",
                             "<div class='direct'>",
                                "<h3>direkte Wiederfunde</h3>",
                                "<p>Wiederfunde werden als direkt gewertet innerhalb:</p>",
                                "<ul>",
                                  "<li>des gleichen Heimzugs inklusive Brutzeit (Jan-Jul)</li>",
                                  "<li>des gleichen Wegzugs inklusive Brutzeit (Jun-Dez)</li>",
                                  "<li>des gleichen Wegzugs inklusive Winter (Aug-Mär)</li>",
                                "</ul>",
                             "</div>",
                           "</div>"
                        ))
                      )
                   )
                )
      )),
    menuItem("About", tabName = "about", icon = icon("question")), br(),
    hidden(div(id="social_links",
      HTML(paste0(
        "<script>",
        "var today = new Date();",
        "var yyyy = today.getFullYear();",
        "</script>",
        "<p style = 'text-align: center;'><small>&copy; - <a href='mailto:g_r@posteo.de' target='_blank'>Georg Rüppel</a> - <script>document.write(yyyy);</script></small></p>"
        ))
    ))
  )
)


##############################
### body
##############################

body <- dashboardBody(useShinyjs(),
                      tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "CSS/styles.css")),   # add css styles
                      mobileDetect('isMobile'),   # detect mobile device
                      pwa("https://g-rppl.shinyapps.io/goie_shiny/", title = "GOie", output = "www/JS", icon="./www/icon.png"),   # enable PWA
  tabItems(
    
    
    ########## startpage ##########
    
    tabItem(tabName = "Startseite",
            h1("Die Vögel der Greifswalder Oie"), 
            h3(HTML(paste0('Georg Rüppel', '<a href="mailto:georg.rueppel2@jordsand.de"> <sup> <i class="fa fa-envelope"> </i> </sup> </a>',
                          ', Saskia Schirmer', '<a href="mailto:saskia.schirmer@jordsand.de"> <sup> <i class="fa fa-envelope"> </i> </sup> </a>',
                          ', Thomas Klinner', '<a href="mailto:thomas.klinner@jordsand.de"> <sup> <i class="fa fa-envelope"> </i> </sup> </a>',
                          ' & Jan von Rönn', '<a href="mailto:jan.vonroenn@jordsand.de"> <sup> <i class="fa fa-envelope"> </i> </sup> </a>'
            ))), br(),
            fluidRow(
              box(title="Greifswalder Oie BirdObs", solidHeader = TRUE, status = "primary", br(),
                  fluidRow(
                    column(6, HTML('<center><img src="GOie.png"></center>')),
                    column(6, br(),br(),br(),br(), valueBoxOutput("sp_ges", width = 12))
                  ), br(),
                  uiOutput("intro")
              ),
              box(title="Über diese App", status = "success", collapsible = TRUE, icon=icon("user-friends"),
                  HTML('<center><img style="heigth:200px; width:221px;" src="logo.png"></center>'),
                  HTML('Diese App ist eine interaktive Darstellung der Beringungs- und Beobachtungsdaten von der Greifswalder Oie. 
                  Sie enthält keine wesentliche inhaltliche Interpretationen und erhebt keinen Anspruch auf Vollständigkeit. 
                  Eine zitierfähige Version dieser Avifauna wird auf Zenodo (<a href="https://about.zenodo.org/">?</a>) zu finden sein:'),
                  HTML('<center><a href="https://zenodo.org/communities/vj_goie_birdobs/"> <img src="zenodo.png"></a></center>'),
                  h4("Dank"),
                  "Wir danken allen ehemaligen und aktiven HelferInnen, FöjlerInnen,
                   BeringerInnen und allen anderen für ihren Beitrag zum Gelingen der Arbeit auf der Insel.
                   Für die Bereitstellung der Kontroll- und Wiederfunddaten von nicht auf der Insel beringten
                   oder nicht auf der Insel wiedergefundenen Vögeln bedanken wir uns bei der Beringungszentrale Hiddensee."
                  )
            ),
            leafletOutput("overview_map")
    ),
    
    
    ########## species list ##########
    
    tabItem(tabName = "Artenliste",
            h2("Artenliste"),
            fluidRow(
              box(title = "Artenliste der Vögel der Greifswalder Oie", collapsible = TRUE, status = "primary",
                  'Vollständige Liste aller auf der Greifswalder Oie nachgewiesenen Vogelarten. Ausnahmeerscheinungen sind mit einem "A" vor dem Artnamen versehen.', 
                  br(), br(),
                  div(class="scroll-table", tableOutput("artenliste"))
              ),
              box(title= "Gefangenschaftsflüchtlinge", collapsible = TRUE, status = "warning",
                  div(class="scroll-table", tableOutput("gf")))
            )),
    
    
    ########## breeding birds ##########
    
    tabItem(tabName = "breeding",
            h2("Brutvögel"), br(),
            fluidRow(
              valueBoxOutput("sp_ges_bvk", width = 3)
            ),
            fluidRow(
              box(title = "Brutbestand über die Jahre", status = "primary",
                  plotOutput("bvk"),
                  selectInput("bvk_art", "Art", unique(bvk$Art[order(bvk$Nr)]), selected = "", width = 300),
                  "Dargestellt ist die Anzahl der kartierten Reviere / Brutpaare pro Jahr."
              ),
              div(id = "bvk_box_arten",
                  box(title = "Anzahl Brutvogelarten über die Jahre", status = "warning",
                      plotOutput("bvk_arten"),
                      "Dargestellt ist die Anzahl der Brutvogelarten pro Jahr."
                  )
              ),
              hidden(div(id = "bvk_box",
                box(title = "Brutbestand über die Jahre", status = "primary",
                    plotOutput("bvk_2"),
                    selectInput("bvk_art_2", "Art", unique(bvk$Art[order(bvk$Nr)]), selected = "", width = 300),
                    "Dargestellt ist die Anzahl der kartierten Reviere / Brutpaare pro Jahr."
                )
              ))
            ),
    ),


    ########## first captures over years ##########

    tabItem(tabName = "ring_count",
            h2("Beringungszahlen"), br(),
            fluidRow(
              valueBoxOutput("total_catch_sp", width = 3),
              valueBoxOutput("total_catch", width = 3)
            ),
            fluidRow(
              box(title = "Beringungen pro Jahr nach Art", status = "primary",
                  plotOutput("ring_count"),
                  selectInput("ring_count_art", "Art", unique(data_ring$Art[order(data_ring$Nr)]), 
                              selected = "Rotkehlchen", width = 300),
                  "Dargestellt ist die Anzahl der Erstfänge pro Jahr für das Frühjahr (blaue Balken) und den Herbst (grüne Balken)."
              ),
              div(id = "ring_count_ges",
                  box(title = "Beringungen pro Jahr gesamt", status = "warning",
                      plotOutput("ring_count_all"),
                      "Dargestellt ist die Gesamtzahl der Erstfänge pro Jahr für das Frühjahr (blaue Balken) 
                      und den Herbst (grüne Balken)."
                  )
              ),
              hidden(div(id = "ring_count_box",
                         box(title = "Beringungen pro Jahr nach Art", status = "primary",
                             plotOutput("ring_count_2"),
                             selectInput("ring_count_art_2", "Art", unique(data_ring$Art[order(data_ring$Nr)]), 
                                         selected = "Rotkehlchen", width = 300),
                             "Dargestellt ist die Anzahl der Erstfänge pro Jahr für das Frühjahr (blaue Balken) 
                             und den Herbst (grüne Balken)."
                            )
                         )
                     )
            ),
    ),


    ########## yearly data ##########
    
    tabItem(tabName = "year_data",
            h2("Jahresübersicht"), br(),
            fluidRow(
              valueBoxOutput("year_arten", width = 3),
              valueBoxOutput("year_catch", width = 3)
            ),
            fluidRow(
              box(title = "Beringungen pro Pentade", status = "primary",
                  plotOutput("year_sp"),
                  selectInput("year_art", "Art", unique(artenl$Art[artenl$year_plot=="TRUE"]), selected = "Rotkehlchen", width = 300),
                  textOutput("year_text")
              ),
              div(id = "year_box_ges",
                  box(title = "Beringungen aller Arten pro Pentade", status = "warning",
                      plotOutput("year_ges"),
                      textOutput("year_text_ges")
                  )
              ),
              hidden(div(id = "year_box",
                box(title = "Beringungen pro Pentade", status = "primary",
                    plotOutput("year_sp_2"),
                    selectInput("year_art_2", "Art", unique(artenl$Art[artenl$year_plot=="TRUE"]), selected = "Rotkehlchen", width = 300),
                    textOutput("year_text_2")
                )
              ))
            ),
            fluidRow(
              box(title = "Frühjahr", status = "primary",
                  "Saisonübersicht mit minimalen, maximalen und druchschnittlichen Fangzahlen 
                  sowie der Abweichung vom Mittel seit 2000.", br(), br(),
                  div(class="scroll-table", DTOutput("year_s"))
              ),
              box(title = "Herbst", status = "primary",
                  "Saisonübersicht mit minimalen, maximalen und druchschnittlichen Fangzahlen 
                  sowie der Abweichung vom Mittel seit 2000.", br(), br(),
                  div(class="scroll-table", DTOutput("year_a"))
              )
            )
    ),


    ########## phenology ##########

    tabItem(tabName = "pheno",
            h2("Phänologie"),
            fluidRow(column(6, htmlOutput("pheno_species")),
                     hidden(div(id="pheno_species_2", column(6, htmlOutput("pheno_species_2"))))
            ),
            fluidRow(
              box(id="p_box1", title = "Auftreten im Jahresverlauf nach Beringungsdaten", status = "primary", collapsible = TRUE,
                  plotOutput("ringing"),
                  textOutput("ringing_text")
              ),
              hidden(div(id = "pheno_box_1",
                         box(title = "Auftreten im Jahresverlauf nach Beringungsdaten", status = "primary", collapsible = TRUE,
                             plotOutput("ringing_2"),
                             textOutput("ringing_text_2")
                            )
                         )
                     )
            ),
            fluidRow(
              box(title = "Auftreten im Jahresverlauf nach Beobachtungsdaten", status = "primary", collapsible = TRUE,
                  plotOutput("beobachtung"),
                  textOutput("beobachtung_text")
              ),
              hidden(div(id = "pheno_box_2",
                         box(title = "Auftreten im Jahresverlauf nach Beobachtungsdaten", status = "primary", collapsible = TRUE,
                             plotOutput("beobachtung_2"),
                             textOutput("beobachtung_text_2")
                            )
                         )
                     )
            ),
            fluidRow(
              box(title = "Phänologie über die Jahre nach Beringungsdaten", status = "primary", collapsible = TRUE,
                  plotOutput("pheno_year"),
                  tableOutput("pheno_year_table"),
                  textOutput("pheno_year_text")
              ),
              hidden(div(id = "pheno_box_3",
                         box(title = "Phänologie über die Jahre nach Beringungsdaten", status = "primary", collapsible = TRUE,
                             plotOutput("pheno_year_2"),
                             tableOutput("pheno_year_table_2"),
                             textOutput("pheno_year_text_2")
                            )
                         )
                     )
            )
    ),


    ########## vagrants ##########

    tabItem(tabName = "vag",
            h2("Seltenheiten"), 
            htmlOutput("vagrant_species"),
            fluidRow(
              box(title = "Tage mit Nachweisen pro Jahr", status = "primary",
                  plotOutput("vag_year"),
                  "Dargestellt ist die Anzahl der Tage mit Nachweisen pro Jahr."
              ),
              box(title = "Phänologie", status = "primary",
                  plotOutput("vag_pheno"),
                  "Dargestellt ist die Anzahl der Tage mit Nachweisen pro Dekade."
              )
            )
    ),


    ########## recovery map ##########

    tabItem(tabName = "Wiederfundkarte",
            div(id = "main_map",
                withSpinner(leafletOutput("map"), type = "7")
            ),
            uiOutput("map_panel"),
            hidden(div(id = "compare_map",
                       fluidRow(column(6, withSpinner(leafletOutput("map_1"), type = "7")),
                                column(6, withSpinner(leafletOutput("map_2"), type = "7"))
                       )
            ))
    ),
    
    
    ########## about ##########
    
    tabItem(tabName = "about",
            uiOutput("about"))
    
  )
)


##############################
### combine to dashboardPage
##############################

ui <- dashboardPage(
  header,
  sidebar,
  body
)
