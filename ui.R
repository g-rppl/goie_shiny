
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
                          dropdownMenu(type = "message", icon = icon("info"), badgeStatus = NULL,
                                       headerText = tags$b("Informationen zum Datenstand"),
                                       shinydashboardPlus::messageItem("Beringung", textOutput("ring_info"), icon= icon("ring"), inputId="openModal1"),
                                       shinydashboardPlus::messageItem("Beobachtungen", textOutput("beob_info"), icon= icon("binoculars"), inputId="openModal2"),
                                       messageItem("Wiederfunde", textOutput("rec_info"), icon = icon("search-location"))
                          ),
                          dropdownMenu(type = "message", headerText = tags$b("Teilen"),
                                      icon = icon("share-alt"), badgeStatus = NULL,
                                      messageItem(
                                        from = "Twitter",
                                        message = "",
                                        icon = icon("twitter"),
                                        href = "https://twitter.com/intent/tweet?url=https://vereinjordsand.shinyapps.io/goie/&text=Greifswalder%20Oie%20BirdObs%20datacenter%20dashboard"
                                      ),
                                      messageItem(
                                        from = "Facebook",
                                        message = "",
                                        icon = icon("facebook"),
                                        href = "https://www.facebook.com/sharer/sharer.php?u=https://vereinjordsand.shinyapps.io/goie/"
                                      ),
                                      messageItem(
                                        from = "Linkedin",
                                        message = "",
                                        icon = icon("linkedin"),
                                        href = "https://www.linkedin.com/shareArticle?mini=true&url=https://vereinjordsand.shinyapps.io/goie/"
                                      ),
                                      messageItem(
                                        from = "Telegram",
                                        message = "",
                                        icon = tagAppendAttributes(icon("telegram")),
                                        href = "https://telegram.me/share/url?url=https://vereinjordsand.shinyapps.io/goie/&text=%0AGreifswalder%20Oie%20BirdObs%20datacenter%20dashboard"
                                      ),
                                      messageItem(
                                        from = "WhatsApp",
                                        message = "",
                                        icon = tagAppendAttributes(icon("whatsapp")),
                                        href = "https://wa.me/?text=https://vereinjordsand.shinyapps.io/goie/"
                                      ),
                                      messageItem(
                                        from = "EMail",
                                        message = "",
                                        icon = tags$i(class = "fas fa-envelope", style="font-size: 12px"),
                                        href = "mailto:info@example.com?&subject=&cc=&bcc=&body=https://vereinjordsand.shinyapps.io/goie/%0AGreifswalder%20Oie%20BirdObs%20datacenter%20dashboard"
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
             badgeLabel = HTML(paste(icon("binoculars"), "/", icon("ring"))), badgeColor = "olive"),
    menuItem("Brutvögel", tabName = "breeding", icon = icon("egg"),
             badgeLabel = icon("binoculars"), badgeColor = "olive"),
      hidden(div(id="sidbar_bvk", conditionalPanel("input.sidebar === 'breeding'",
                           actionButton("compare_bvk", "Arten vergleichen", icon("sync-alt")),
                           hidden(actionButton("compare_bvk_2", "Anzahl Arten anzeigen", icon("chart-line")))
                           )
      )),
    menuItem("Beringungszahlen", tabName = "ring_count", icon = icon("chart-bar"),
             badgeLabel = icon("ring"), badgeColor = "olive"),
      hidden(div(id="sidbar_ring_count", conditionalPanel("input.sidebar === 'ring_count'",
                           selectInput("select_season", "Saison", c("gesamtes Jahr", "Frühjahr", "Herbst")),
                           actionButton("compare_ring_count", "Arten vergleichen", icon("sync-alt")),
                           hidden(actionButton("compare_ring_count_2", "Gesamtzahl anzeigen", icon("chart-bar")))
                           )
      )),
    menuItem("Jahresübersicht", tabName = "year_data", icon = icon("chart-line"),
            badgeLabel = HTML(paste(icon("ring"), "/", icon("binoculars"))), badgeColor = "olive"),
      hidden(div(id="sidbar_year_data", conditionalPanel("input.sidebar === 'year_data'",
                           selectInput("year", "Jahr", 2000:year_end, selected = year_end),
                           actionButton("compare_year", "Arten vergleichen", icon("sync-alt")),
                           hidden(actionButton("compare_year_2", "Gesamtzahl anzeigen", icon("chart-bar")))
                           )
      )),
    menuItem("Phänologie", tabName = "pheno", icon = icon("calendar"),
             badgeLabel = HTML(paste(icon("ring"), "/", icon("binoculars"))), badgeColor = "olive"),
      hidden(div(id="sidbar_pheno", conditionalPanel("input.sidebar === 'pheno'",
                           selectInput("sp", "Art", artenl$Art[artenl$Dz_Status!="Ausnahmeerscheinung" & artenl$Art!="Alpenbirkenzeisig"], 
                                       selected = "Rotkehlchen"),
                           actionButton("compare_pheno", "Arten vergleichen", icon("sync-alt")),
                           hidden(div(id = "show_sp_2",
                              actionButton("compare_pheno_2", "nur eine Art", icon("caret-up")),
                              selectInput("sp_2", "Art 2", artenl$Art[artenl$Dz_Status!="Ausnahmeerscheinung" & artenl$Art!="Alpenbirkenzeisig"],
                                          selected = "Rotkehlchen")
                              )
                           ),
                           prettySwitch("show_descriptions", "zeige Bildunterschriften", fill = TRUE, value = TRUE, status = "primary")
      ))),
    menuItem("Seltenheiten", tabName = "vag", icon = icon("dove"),
             badgeLabel =  HTML(paste(icon("binoculars"), "/", icon("ring"))), badgeColor = "olive"),
      hidden(div(id="sidbar_vag", conditionalPanel("input.sidebar === 'vag'",
                           selectInput("vag_sp", "Art", artenl$Art[artenl$Dz_Status=="Ausnahmeerscheinung"], 
                                       selected = "Bergbraunelle"))
      )),
    menuItem("Wiederfundkarte", tabName = "Wiederfundkarte", icon = icon("globe-europe"),
             badgeLabel = HTML(paste(icon("ring"), "/", icon("binoculars"))), badgeColor = "olive"),
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
    hidden(div(id = "social_links",
      HTML(paste0(
        "<script>",
        "var today = new Date();",
        "var yyyy = today.getFullYear();",
        "</script>",
        "<p style = 'text-align: center;'> <small>",
          "<a href = 'https://creativecommons.org/licenses/by-nc-nd/4.0/'> <img src = 'https://i.creativecommons.org/l/by-nc-nd/4.0/88x31.png' width = 50px > </a>",
          "<a href = 'mailto:g_r@posteo.de' target = '_blank'> &nbsp Rüppel </a> et al. <script> document.write(yyyy); </script>",
        "</small> </p>"
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
                      pwa("https://vereinjordsand.shinyapps.io/goie/", title = "GOie", output = "www/JS", icon="./www/icon.png"),   # enable PWA

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
              box(title="Die Insel Greifswalder Oie", solidHeader = TRUE, status = "primary", br(),
                  fluidRow(
                    column(6, HTML('<center><img src="GOie.png"></center>')),
                    column(6, br(),br(),br(), valueBoxOutput("sp_ges", width = 12))
                  ), br(),
                  uiOutput("intro")
              ),
              box(title = "Über diese App", status = "primary", collapsible = TRUE, icon = icon("user-friends"),
                  HTML('<br> <center> <img style = "heigth:200px; width:221px;" src = "vj.jpg"> </center> <br> <br>'),
                  HTML('Diese App ist eine interaktive Darstellung der Beringungs- und Beobachtungsdaten von der Greifswalder Oie. 
                  Sie enthält keine wesentliche inhaltliche Interpretationen und erhebt keinen Anspruch auf Vollständigkeit. 
                  Eine zitierfähige Version dieser Avifauna wird auf Zenodo (<a href="https://about.zenodo.org/">?</a>) zu finden sein:'),
                  HTML('<br> <br> <center> <a href="https://zenodo.org/communities/vj_goie_birdobs/"> <img src = "zenodo.png"> </a> </center>'),
                  h4("Dank"),
                  "Wir danken allen ehemaligen und aktiven Helfer:innen, Föjler:innen,
                   Beringer:innen und allen anderen für ihren Beitrag zum Gelingen der Arbeit auf der Insel.
                   Für die Bereitstellung der Kontroll- und Wiederfunddaten von Vögeln mit Bezug zu anderen 
                   Beringungszentralen bedanken wir uns bei der Beringungszentrale Hiddensee 
                   und dem Dachverband Deutscher Avifaunisten danken wir für die Bereitstellung der ornitho-Daten."
                  )
            ),
            leafletOutput("overview_map")
    ),
    
    
    ########## species list ##########
    
    tabItem(tabName = "Artenliste",
            h2(HTML("Artenliste &ndash; <i> Species list </i>")), br(),
            fluidRow(
              valueBoxOutput("sp_ges_2", width = 3)
            ),
            fluidRow(
              box(title = "Artenliste der Vögel der Greifswalder Oie", collapsible = TRUE, status = "primary",
                  htmlOutput("Artenliste_text"), 
                  br(),
                  div(class="scroll-table", tableOutput("artenliste"))
              ),
              box(title= "Gefangenschaftsflüchtlinge", collapsible = TRUE, status = "warning",
                  div(class="scroll-table", tableOutput("gf")))
            )),
    
    
    ########## breeding birds ##########
    
    tabItem(tabName = "breeding",
            h2(HTML("Brutvögel &ndash; <i> Breeding birds </i>")), br(),
            fluidRow(
              valueBoxOutput("sp_ges_bvk", width = 3)
            ),
            fluidRow(
              box(title = "Brutbestand über die Jahre", status = "primary",
                  plotOutput("bvk"),
                  fluidRow(column(6, selectInput("bvk_art", "Art", unique(bvk$Art[order(bvk$Nr)]), selected = "", width = 300)),
                           column(6, br(), htmlOutput("bvk_art_name"))
                  ),
                  HTML("Dargestellt ist die Anzahl der kartierten Reviere / Brutpaare pro Jahr. &ndash;
                       <i> Number of territories / breeding pairs per year. </i>")
              ),
              div(id = "bvk_box_arten",
                  box(title = "Anzahl Brutvogelarten über die Jahre", status = "warning",
                      plotOutput("bvk_arten"),
                      HTML("Dargestellt ist die Anzahl der Brutvogelarten pro Jahr. &ndash; <i> Number of breeding bird species per year. </i>")
                  )
              ),
              hidden(div(id = "bvk_box",
                box(title = "Brutbestand über die Jahre", status = "primary",
                    plotOutput("bvk_2"),
                    fluidRow(column(6, selectInput("bvk_art_2", "Art", unique(bvk$Art[order(bvk$Nr)]), selected = "", width = 300)),
                             column(6, br(), htmlOutput("bvk_art_name_2"))
                    ),
                    HTML("Dargestellt ist die Anzahl der kartierten Reviere / Brutpaare pro Jahr. &ndash;
                       <i> Number of territories / breeding pairs per year. </i>")
                )
              ))
            ),
    ),


    ########## first captures over years ##########

    tabItem(tabName = "ring_count",
            h2(HTML("Beringungszahlen &ndash; <i> Ringing totals </i>")), br(),
            fluidRow(
              valueBoxOutput("total_catch_sp", width = 3),
              valueBoxOutput("total_catch", width = 3)
            ),
            fluidRow(
              box(title = "Beringungen pro Jahr nach Art", status = "primary",
                  plotOutput("ring_count"),
                  fluidRow(column(6, selectInput("ring_count_art", "Art", unique(data_ring$Art[order(data_ring$Nr)]), 
                              selected = "Rotkehlchen", width = 300)),
                           column(6, br(), htmlOutput("ring_count_art_name"))
                  ),
                  htmlOutput("ring_count_text")
              ),
              div(id = "ring_count_ges",
                  box(title = "Beringungen pro Jahr gesamt", status = "warning",
                      plotOutput("ring_count_all"),
                      htmlOutput("ring_count_all_text")
                  )
              ),
              hidden(div(id = "ring_count_box",
                         box(title = "Beringungen pro Jahr nach Art", status = "primary",
                             plotOutput("ring_count_2"),
                             fluidRow(column(6, selectInput("ring_count_art_2", "Art", unique(data_ring$Art[order(data_ring$Nr)]), 
                                         selected = "Rotkehlchen", width = 300)),
                                      column(6, br(), htmlOutput("ring_count_art_name_2"))
                             ),
                             htmlOutput("ring_count_text_2")
                            )
                         )
                     )
            ),
    ),


    ########## yearly data ##########
    
    tabItem(tabName = "year_data",
            h2(HTML("Jahresübersicht &ndash; <i> Annual reports </i>")), br(),
            fluidRow(
              valueBoxOutput("year_arten", width = 3),
              valueBoxOutput("year_catch", width = 3)
            ),
            fluidRow(
              box(title = "Beringungen pro Pentade", status = "primary",
                  plotOutput("year_sp"),
                  fluidRow(column(6, selectInput("year_art", "Art", unique(artenl$Art[artenl$year_plot=="TRUE"]), selected = "Rotkehlchen", width = 300)),
                           column(6, br(), htmlOutput("year_art_name"))
                  ),
                  htmlOutput("year_text")
              ),
              div(id = "year_box_ges",
                  box(title = "Beringungen aller Arten pro Pentade", status = "warning",
                      plotOutput("year_ges"),
                      htmlOutput("year_text_ges")
                  )
              ),
              hidden(div(id = "year_box",
                box(title = "Beringungen pro Pentade", status = "primary",
                    plotOutput("year_sp_2"),
                    fluidRow(column(6, selectInput("year_art_2", "Art", unique(artenl$Art[artenl$year_plot=="TRUE"]), selected = "Rotkehlchen", width = 300)),
                             column(6, br(),  uiOutput("year_art_name_2"))
                    ),
                    htmlOutput("year_text_2")
                )
              ))
            ),
            fluidRow(
              box(title = "Frühjahr", status = "primary",
                  HTML("Saisonübersicht mit minimalen, maximalen und durchschnittlichen Fangzahlen sowie der Abweichung vom Mittel seit 2000. &ndash;
                       <i> Seasonal report for spring with minimal, maximal and mean numbers of first captures including the deviation from the mean since 2000. </i>"), br(), br(),
                  div(class="scroll-table", DTOutput("year_s"))
              ),
              box(title = "Herbst", status = "primary",
                  HTML("Saisonübersicht mit minimalen, maximalen und durchschnittlichen Fangzahlen sowie der Abweichung vom Mittel seit 2000. &ndash;
                       <i> Seasonal report for autumn with minimal, maximal and mean numbers of first captures including the deviation from the mean since 2000. </i>"), br(), br(),
                  div(class="scroll-table", DTOutput("year_a"))
              )
            )
    ),


    ########## phenology ##########

    tabItem(tabName = "pheno",
            h2(HTML("Phänologie &ndash; <i> Phenology </i>")),
            fluidRow(column(6, htmlOutput("pheno_species")),
                     hidden(div(id="pheno_species_2", column(6, htmlOutput("pheno_species_2"))))
            ),
            fluidRow(
              box(id="p_box1", title = "Auftreten im Jahresverlauf nach Beringungsdaten", status = "primary", collapsible = TRUE,
                  plotOutput("ringing"),
                  htmlOutput("ringing_text")
              ),
              hidden(div(id = "pheno_box_1",
                         box(title = "Auftreten im Jahresverlauf nach Beringungsdaten", status = "primary", collapsible = TRUE,
                             plotOutput("ringing_2"),
                             htmlOutput("ringing_text_2")
                            )
                         )
                     )
            ),
            fluidRow(
              box(title = "Auftreten im Jahresverlauf nach Beobachtungsdaten", status = "primary", collapsible = TRUE,
                  plotOutput("beobachtung"), br(),
                  htmlOutput("beobachtung_text")
              ),
              hidden(div(id = "pheno_box_2",
                         box(title = "Auftreten im Jahresverlauf nach Beobachtungsdaten", status = "primary", collapsible = TRUE,
                             plotOutput("beobachtung_2"), br(),
                             htmlOutput("beobachtung_text_2")
                            )
                         )
                     )
            ),
            fluidRow(
              box(title = "Phänologie über die Jahre nach Beringungsdaten", status = "primary", collapsible = TRUE,
                  plotOutput("pheno_year"),
                  tableOutput("pheno_year_table"),
                  htmlOutput("pheno_year_text")
              ),
              hidden(div(id = "pheno_box_3",
                         box(title = "Phänologie über die Jahre nach Beringungsdaten", status = "primary", collapsible = TRUE,
                             plotOutput("pheno_year_2"),
                             tableOutput("pheno_year_table_2"),
                             htmlOutput("pheno_year_text_2")
                            )
                         )
                     )
            )
    ),


    ########## vagrants ##########

    tabItem(tabName = "vag",
            h2(HTML("Seltenheiten &ndash; <i> Vagrants </i>")), 
            htmlOutput("vagrant_species"),
            fluidRow(
              box(title = "Tage mit Nachweisen pro Jahr", status = "primary",
                  plotOutput("vag_year"),
                  HTML("Dargestellt ist die Anzahl der Tage mit Nachweisen pro Jahr. &ndash; <i> Number of days with records per year. </i>")
              ),
              box(title = "Phänologie", status = "primary",
                  plotOutput("vag_pheno"),
                  HTML("Dargestellt ist die Anzahl der Tage mit Nachweisen pro Dekade. &ndash; <i> Number of days with records per 10d </i>")
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
