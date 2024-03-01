
##############################
###
### SERVER
###
##############################

server <- function(input, output, session) {

  
  ########## observeEvents when sidebar is collapsed ##########
  
  observeEvent(input$sidebarCollapsed, {
    toggle("GOie")
    toggle("sidbar_bvk")
    toggle("sidbar_ring_count")
    toggle("sidbar_year_data")
    toggle("sidbar_pheno")
    toggle("sidbar_vag")
    toggle("sidbar_Wiederfundkarte")
    toggle("social_links")
  })

  
  ##############################
  ### info text and map
  ##############################
  
  output$intro <- renderUI({
    includeMarkdown("info_text.md")
  })
  
  output$overview_map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      addMarkers(long.oie, lat.oie)
  })
  
  
  ##############################
  ### data info
  ##############################
  
  output$ring_info <- renderText(
    paste(as.character(max(data_ring$DATE, na.rm=T)), "(für Plot klicken)")
  )
  
  output$beob_info <- renderText(
    paste(as.character(max(beob$DATE, na.rm=T)), "(für Plot klicken)")
  )
  
  output$rec_info <- renderText(
    paste("bis", as.character(max(data$FDAJ)))
  )
  
  
  ########## ringing data ##########
  
  tmp.pen <- data_ring %>%     
    group_by(Year, pentade, yday) %>%
    summarise(.groups = "drop_last") %>%
    mutate(n=length(yday))
  
  output$ring_i <- renderPlot({data_info(tmp.pen, tmp.pen$pentade,tmp.pen$Year, 5)})
  
  
  ########## observation data ##########
  
  tmp.dec <- beob %>%     
    group_by(DATE_YEAR, DATE_DECADE, DATE_JDAY) %>%
    summarise(.groups = "drop_last") %>%
    mutate(n=length(DATE_JDAY))
  
  output$beob_i <- renderPlot({data_info(tmp.dec, tmp.dec$DATE_DECADE, tmp.dec$DATE_YEAR, 10)})
  
  
  ########## observeEvents for plots in popup ##########
  
  observeEvent(input$openModal1, {
    showModal(
      modalDialog(title = "Beringungsdaten",
                  plotOutput("ring_i"),
                  footer = tags$p(align = "left", HTML("Anzahl Tage mit Beringungen pro Pentade. &ndash; <i> Days with ringing activity per 5d. </i>")), easyClose = TRUE)
    )
  })
  
  observeEvent(input$openModal2, {
    showModal(
      modalDialog(title = "Beobachtungsdaten",
                  plotOutput("beob_i"),
                  footer = tags$p(align = "left", HTML("Anzahl Tage mit Beobachtungen pro Dekade. &ndash; <i> Days with observation activity per 10d. </i>")), easyClose = TRUE)
    )
  })
  
  
  ##############################
  ### species list
  ##############################
  
  
  ########## value box ##########
  
  output$sp_ges <- output$sp_ges_2 <- renderValueBox({
    
    max(artenl$Nr, na.rm=T) %>%
      valueBox(subtitle=HTML("Nachgewiesene Arten &ndash; <br> <i> species recorded </i>"), icon=icon("crow"), color="light-blue")
    
  })
  
  
  ########## text ##########
  
  output$Artenliste_text <- renderText({
    
    HTML('Vollständige Liste aller auf der Greifswalder Oie nachgewiesenen Vogelarten. Ausnahmeerscheinungen sind mit einem "A" vor dem Artnamen versehen. &ndash;
    <i> Complete list of all bird species recorded on Greifswalder Oie. Vagrants are indicated by an "A". </i>') 
    
  })
  
  
  ########## tables ##########
  
  output$artenliste <- function() {
    artenl[, c(1,7,2:4)] %>%
      filter(Art!="Birkenzeisig") %>%   # exclude Birkenzeisig
      mutate(Dz_Status = ifelse(Dz_Status=="Ausnahmeerscheinung",
                                cell_spec("A"),
                                cell_spec("")),
            Art = ifelse(is.na(Nr)==F,
                              cell_spec(Art, bold = T),
                              cell_spec(Art, bold = F))
      ) %>%
      kable(escape = F, col.names = NULL) %>%
      kable_styling(c("striped", "bordered")) %>%
      column_spec(5, italic = T)
  }
  
  output$gf <- function() {
    gf <- rbind(
      c("Schwarzschwan", "Black Swan", "Cygnus atratus"),
      c("Nymphensittich", "Cockatiel", "Nymphicus hollandicus"),
      c("Mohrenkopfpapagei", "Senegal Parrot", "Poicephalus senegalus"),
      c("Glanzsittich", "Scarlet-chested Parrot", "Neophema splendida"),
      c("Grünschwanz-Glanzstar", "Greater Blue-eared Starling", "Lamprotornis chalybaeus")
    )
    
    gf %>%
      kable() %>%
      kable_styling(c("striped", "bordered")) %>%
      column_spec(1, bold = T) %>%
      column_spec(3, italic = T) 
  }
  
  
  ##############################
  ### breeding birds
  ##############################
  
  
  ########## value box ##########
  
  output$sp_ges_bvk <- renderValueBox({
    
    length(unique(bvk$Art)) %>%
      valueBox(subtitle=HTML("Brutvogelarten &ndash; <br> <i> breeding bird species </i>"), icon=icon("crow"), color="light-blue")
    
  })
  
  
  ########## main content ##########
  
  ##### for selected species #####
  
  tmp.bvk <- reactive({
    bvk[bvk$Art == input$bvk_art,]
  })
  
  output$bvk <- renderPlot({
    breeding(data=tmp.bvk())
  })
  
  ##### species per year #####

  tmp.bvk_arten <- bvk %>%
    filter(count > 0) %>%
    group_by(year, Art) %>%
    summarise(.groups = "drop_last") %>%
    mutate(n=length(Art))

  output$bvk_arten <- renderPlot(
   
    ggplot(tmp.bvk_arten, aes(year, n)) +
      geom_line(size=2, colour=js) + geom_point(size=4, colour=js) + xlab("Jahr") + ylab("Anzahl") + 
      scale_x_continuous(limits = c(1993, year_end+1), breaks = seq(1995, 2030, 5)) +
      ylim(c(0,max(tmp.bvk_arten$n, na.rm=T)*1.1)) +
      theme_classic(base_size = 20)
    
  )
  
  
  ########## compare species ##########
  
  tmp.bvk.2 <- reactive({
    bvk[bvk$Art == input$bvk_art_2,]
  })
  
  output$bvk_2 <- renderPlot({
    breeding(data=tmp.bvk.2())
  })
  
  
  ########## observeEvent compare species ##########
  
  observeEvent(input$compare_bvk, {
    hide("compare_bvk")
    shinyjs::show("compare_bvk_2")
    toggle("bvk_box")
    toggle("bvk_box_arten")
  })
  
  observeEvent(input$compare_bvk_2, {
    shinyjs::show("compare_bvk")
    hide("compare_bvk_2")
    toggle("bvk_box")
    toggle("bvk_box_arten")
  })
  
  
  ########## species names ##########
  
  output$bvk_art_name <- renderText({
    HTML(paste(artenl$Species[artenl$Art==input$bvk_art][1], "<br> 
               <i>", artenl$`wiss. Name`[artenl$Art==input$bvk_art][1], "</i>"))
  })
  
  output$bvk_art_name_2 <- renderText({
    HTML(paste(artenl$Species[artenl$Art==input$bvk_art_2][1], "<br> 
               <i>", artenl$`wiss. Name`[artenl$Art==input$bvk_art_2][1], "</i>"))
  })


  ##############################
  ### first captures over years
  ##############################
  
  
  ########## value boxes ##########
  
  output$total_catch_sp <- renderValueBox({
    
    if (input$select_season == "Frühjahr") 
      out <- length(unique(data_ring$Art[data_ring$DATE%in%s.season]))-3   # -Buch_x_Berg, Sprosser_x_Nachtigall, rotst. Blk
    if (input$select_season == "Herbst") 
      out <- length(unique(data_ring$Art[data_ring$DATE%in%a.season]))-3   # -Buch_x_Berg, Sprosser_x_Nachtigall, rotst. Blk
    if (input$select_season == "gesamtes Jahr") 
      out <- length(unique(data_ring$Art))-3   # -Buch_x_Berg, Sprosser_x_Nachtigall, rotst. Blk
    
    out %>% 
      valueBox(subtitle=HTML("Beringte Arten &ndash; <br> <i> ringed species </i>"), icon=icon("crow"), color="light-blue")
    
  })
  
  output$total_catch <- renderValueBox({
    
    if (input$select_season == "Frühjahr")
      out <- length(data_ring$RTYPE[data_ring$RTYPE=="e" & data_ring$DATE%in%s.season])
    if (input$select_season == "Herbst")
      out <- length(data_ring$RTYPE[data_ring$RTYPE=="e" & data_ring$DATE%in%a.season])
    if (input$select_season == "gesamtes Jahr") 
      out <- length(data_ring$RTYPE[data_ring$RTYPE=="e"])
    
    out %>%
    format(big.mark = ".", decimal.mark = ",") %>%
    valueBox(subtitle=HTML("Beringte Vögel &ndash; <br> <i> ringed individuals </i>"), icon=icon("database"), color="light-blue") 
    
  })


  ########## for selected species ##########
  
  ##### spring #####
  
  sum_s <- reactive({data_ring %>%
      filter(Month%in%1:7, RTYPE=="e", Art==input$ring_count_art) %>%
      group_by(Year) %>%
      summarise(n=n(), .groups = 'drop')
  })
  
  ##### autumn #####
  
  sum_a <- reactive({data_ring %>%
      filter(Month%in%8:12, RTYPE=="e",  Art==input$ring_count_art) %>%
      group_by(Year) %>%
      summarise(n=n(), .groups = 'drop')
  })
  
  observe({
    
    ##### merge #####
    
    dummy <- data.frame(Year=1994:year_end)
    
    sum_s <- full_join(dummy, sum_s(), by="Year") %>%
      mutate(season="Frühjahr") %>%
      replace(is.na(.), 0)
    
    sum_a <- full_join(dummy, sum_a(), by="Year") %>%
      mutate(season="Herbst") %>%
      replace(is.na(.), 0)
    
    sum <- rbind(sum_s, sum_a)
    
    ##### plot #####
    
    output$ring_count <- renderPlot({
      plot_ring_count(data=sum, select=input$select_season)
    })
    
  })
  
  
  ########## for compare species ##########
  
  ##### spring #####
  
  sum_s_2 <- reactive({data_ring %>%
      filter(Month%in%1:7, RTYPE=="e",  Art==input$ring_count_art_2) %>%
      group_by(Year) %>%
      summarise(n=n(), .groups = 'drop')
  })
  
  ##### autumn #####
  
  sum_a_2 <- reactive({data_ring %>%
      filter(Month%in%8:12, RTYPE=="e",  Art==input$ring_count_art_2) %>%
      group_by(Year) %>%
      summarise(n=n(), .groups = 'drop')
  })
  
  observe({
    
    ##### merge #####
    
    dummy <- data.frame(Year=1994:year_end)
    
    sum_s_2 <- full_join(dummy, sum_s_2(), by="Year") %>%
      mutate(season="Frühjahr") %>%
      replace(is.na(.), 0)
    
    sum_a_2 <- full_join(dummy, sum_a_2(), by="Year") %>%
      mutate(season="Herbst") %>%
      replace(is.na(.), 0)
    
    sum_2 <- rbind(sum_s_2, sum_a_2)
    
    ##### plot #####
    
    output$ring_count_2 <- renderPlot({
      plot_ring_count(data=sum_2, select=input$select_season)
      
    })
    
  })
  
  
  ########## observeEvent compare species ##########
  
  observeEvent(input$compare_ring_count, {
    hide("compare_ring_count")
    shinyjs::show("compare_ring_count_2")
    toggle("ring_count_box")
    toggle("ring_count_ges")
  })
  
  observeEvent(input$compare_ring_count_2, {
    shinyjs::show("compare_ring_count")
    hide("compare_ring_count_2")
    toggle("ring_count_box")
    toggle("ring_count_ges")
  })
  

  ########## for all first captures ##########
  
  ##### spring #####
  
  sum_s_all <- data_ring %>%
    filter(Month%in%1:7, RTYPE=="e") %>%
    group_by(Year) %>%
    summarise(n=n(), .groups = 'drop')
  
  ##### autumn #####
  
  sum_a_all <- data_ring %>%
    filter(Month%in%8:12, RTYPE=="e") %>%
    group_by(Year) %>%
    summarise(n=n(), .groups = 'drop')
  
  ##### merge #####
  
  dummy <- data.frame(Year=1994:year_end)
  
  sum_s_all <- full_join(dummy, sum_s_all, by="Year") %>%
    mutate(season="Frühjahr") %>%
    replace(is.na(.), 0)
  
  sum_a_all <- full_join(dummy, sum_a_all, by="Year") %>%
    mutate(season="Herbst") %>%
    replace(is.na(.), 0)
  
  sum_all <- rbind(sum_s_all, sum_a_all)
  
  ##### plot #####
  
  output$ring_count_all <- renderPlot({
    plot_ring_count(data=sum_all, select=input$select_season)
    
  })
  
  
  ########## species names ##########
  
  output$ring_count_art_name <- renderText({
    HTML(paste(artenl$Species[artenl$Art==input$ring_count_art][1], "<br> 
               <i>", artenl$`wiss. Name`[artenl$Art==input$ring_count_art][1], "</i>"))
  })
  
  output$ring_count_art_name_2 <- renderText({
    HTML(paste(artenl$Species[artenl$Art==input$ring_count_art_2][1], "<br> 
               <i>", artenl$`wiss. Name`[artenl$Art==input$ring_count_art_2][1], "</i>"))
  })
  
  
  ########## text ##########
  
  output$ring_count_text <- output$ring_count_text_2 <- renderText({
    
    if (input$select_season == "gesamtes Jahr")
      out <- HTML("Dargestellt ist die Anzahl der Erstfänge pro Jahr für das Frühjahr (blaue Balken) und den Herbst (grüne Balken). &ndash; 
                  <i> Number of first captures per year during spring (blue bars) and autumn (green bars). </i>")
    if (input$select_season == "Herbst")
      out <- HTML("Dargestellt ist die Anzahl der Erstfänge pro Jahr für den Herbst. &ndash; 
                  <i> Number of first captures per year during autumn. </i>")
    if (input$select_season == "Frühjahr")
      out <- HTML("Dargestellt ist die Anzahl der Erstfänge pro Jahr für das Frühjahr. &ndash; 
                  <i> Number of first captures per year during spring. </i>")
    
    out
    
  })
  
  output$ring_count_all_text <- renderText({
    
    if (input$select_season == "gesamtes Jahr")
      out <- HTML("Dargestellt ist die Gesamtzahl der Erstfänge pro Jahr für das Frühjahr (blaue Balken) und den Herbst (grüne Balken). &ndash; 
                  <i> Total number of first captures per year during spring (blue bars) and autumn (green bars). </i>")
    if (input$select_season == "Herbst")
      out <- HTML("Dargestellt ist die Gesamtzahl der Erstfänge pro Jahr für den Herbst. &ndash; 
                  <i> Total number of first captures per year during autumn. </i>")
    if (input$select_season == "Frühjahr")
      out <- HTML("Dargestellt ist die Gesamtzahl der Erstfänge pro Jahr für das Frühjahr. &ndash; 
                  <i> Total number of first captures per year during spring. </i>")
    
    out
    
  })

  
  ##############################
  ### yearly data
  ##############################
  
  
  ########## value boxes ##########
  
  output$year_arten <- renderValueBox({
    
    length(unique(beob$NAME_SPECIES[beob$DATE_YEAR==input$year & beob$NAME_SPECIES%in%artenl$Art])) %>%
      valueBox(subtitle=HTML("Nachgewiesene Arten &ndash; <br> <i> species recorded </i>"), icon=icon("crow"), color="light-blue") 
    
  })
  
  output$year_catch <- renderValueBox({
    
    length(tmp.sum_ges()$RTYPE) %>%  
      format(big.mark = ".", decimal.mark = ",") %>%
      valueBox(subtitle=HTML("Beringte Vögel &ndash; <br> <i> ringed individuals </i>"), icon=icon("database"), color="light-blue")
    
  })
  
  
  ########## for all species ##########
  
  tmp.sum_ges <- reactive({data_ring %>%
      filter(RTYPE=="e", Year==input$year)
  })
  
  tmp.mean_ges <- data_ring %>%
      filter(RTYPE=="e")
    
  output$year_ges <- renderPlot({
      
    plot_year(tmp.sum_ges(), tmp.mean_ges)
      
  })
    

  ########## for selected species ##########
  
  tmp.sum <- reactive({data_ring %>%
      filter(RTYPE=="e", Art==input$year_art, Year==input$year)
  })
  
  tmp.mean <- reactive({data_ring %>%
      filter(RTYPE=="e", Art==input$year_art)
  })
  
  observe({
    
    output$year_sp <- if (artenl$ringing_plot[artenl$Art==input$year_art][1] == "TRUE") {
      
      renderPlot({

        plot_year(tmp.sum(), tmp.mean())
        
      })
    }
  })
  
  
  ########## for compare species ##########
  
  tmp.sum_2 <- reactive({data_ring %>%
      filter(RTYPE=="e", Art==input$year_art_2, Year==input$year)
  })
  
  tmp.mean_2 <- reactive({data_ring %>%
      filter(RTYPE=="e", Art==input$year_art_2)
  })
  
  observe({
    
    output$year_sp_2 <- if (artenl$ringing_plot[artenl$Art==input$year_art_2][1] == "TRUE") {
      
      renderPlot(
        
        plot_year(tmp.sum_2(), tmp.mean_2())
        
      )
    }
  })
  
  
  ########## species names ##########
  
  output$year_art_name <- renderText({
    HTML(paste(artenl$Species[artenl$Art==input$year_art], "<br> 
               <i>", artenl$`wiss. Name`[artenl$Art==input$year_art], "</i>"))
  })
  
  output$year_art_name_2 <- renderUI({
    HTML(paste(artenl$Species[artenl$Art==input$year_art_2], "<br> 
               <i>", artenl$`wiss. Name`[artenl$Art==input$year_art_2], "</i>"))
  })
  
  
  ########## texts ##########
  
  output$year_text <- output$year_text_2 <- renderText({
    HTML(paste("Dargestellt ist die Anzahl der Erstfänge pro Pentade im Jahr", input$year, 
               "(blaue Balken) sowie die durchschnittliche Fangzahl seit", year_start, "(gestrichelte Linie). &ndash;
               <i> Number of first captures per 5d in", input$year, "(blue bars) together with the mean since", year_start, "(dashed line). </i>"))
  })
  
  output$year_text_ges <- renderText({
    HTML(paste("Dargestellt ist die Gesamtzahl der Erstfänge aller Arten pro Pentade im Jahr", input$year, 
               "(blaue Balken) sowie die durchschnittliche Fangzahl seit", year_start, "(gestrichelte Linie). &ndash;
               <i> Total number of first captures of all species per 5d in", input$year, "(blue bars) together with the mean since", 
               year_start, "(dashed line). </i>"))
  })
  
  
  ########## tables ##########
  
  table_s <- reactive({table_year(data_ring, input$year, season="s")})
  table_a <- reactive({table_year(data_ring, input$year, season="a")})
  
  observe({
    
    output$year_s <- DT::renderDataTable(table_s(), selection="single")
    output$year_a <- DT::renderDataTable(table_a(), selection="single")
    
    ##### row selections #####
    
    selectRows(dataTableProxy("year_s"), which(table_s()$Art == input$year_art))
    selectRows(dataTableProxy("year_a"), which(table_a()$Art == input$year_art))
    
  })
  
  
  ########## observeEvent table select ##########
  
  observeEvent(input$year_s_rows_selected, {
    updateSelectInput(session, "year_art", selected = table_s()[input$year_s_rows_selected,1])
  })
  
  observeEvent(input$year_a_rows_selected, {
    updateSelectInput(session, "year_art", selected = table_a()[input$year_a_rows_selected,1])
  })
  
  
  ########## observeEvent compare species ##########
  
  observeEvent(input$compare_year, {
    hide("compare_year")
    shinyjs::show("compare_year_2")
    toggle("year_box")
    toggle("year_box_ges")
  })
  
  observeEvent(input$compare_year_2, {
    shinyjs::show("compare_year")
    hide("compare_year_2")
    toggle("year_box")
    toggle("year_box_ges")
  })
  

  ##############################
  ### phenology
  ##############################
  
  
  ########## for selected species ##########

  ##### ringing data #####

  tmp.ring <- reactive({data_ring %>%
      filter(RTYPE=="e", Art==input$sp)
  })
  
  ring_type <-  reactive({artenl$ringing_type[artenl$Art==input$sp][1]})

  observe({
    
    output$pheno_species <- renderText({
      HTML(paste(h3(input$sp), artenl$Species[artenl$Art==input$sp][1], "&ndash; <i>", 
                 artenl$`wiss. Name`[artenl$Art==input$sp][1], "</i><br><br>"))
    })

    output$ringing <- if (artenl$ringing_plot[artenl$Art==input$sp][1] == "TRUE") {

      renderPlot(

        # type=0: nur durchschnittliche Fangzahlen
        # type=1: berechnete Fangzahlen mit Altersverhältnis
        # type=2: berechnete Fangzahlen mit Alters- und Geschlechtsverhältnis
        # type=3: Fangzahlen gesamt
        pheno(d=tmp.ring(), type = ring_type())

      )

    }

    output$ringing_text <- renderText({
      
      if (input$show_descriptions == TRUE) {

        if (artenl$ringing_plot[artenl$Art==input$sp][1] == "TRUE") {
  
          if (ring_type() == 0)
            out <- HTML("Dargestellt ist die durchschnittliche Anzahl der Erstfänge pro Pentade seit 2000, die Fangzeiten sind grau hinterlegt. &ndash; 
            <i> Mean number of first captures per 5d since 2000, catching seasons are given in grey. </i>")
          if (ring_type() == 1)
            out <- HTML("Dargestellt ist die durchschnittliche Anzahl der Erstfänge pro Pentade seit 2000 (blaue Balken),
            die Fangzeiten sind grau hinterlegt. Die Anteile der Alterklassen sind dem unteren Diagrammen zu entnehmen,
            der Anteil unbestimmter Vögel wird in Prozent für das Frühjahr und den Herbst angegeben und ist grau dargestellt. &ndash;
            <i> Mean number of first captures per 5d since 2000 (blue bars), catching seasons are given in grey. 
            Chart below shows proportions of age classes, proportions of unidentified birds are given for spring and autumn and shown in grey. </i>")
          if (ring_type() == 2)
            out <- HTML("Dargestellt ist die durchschnittliche Anzahl der Erstfänge pro Pentade seit 2000 (blaue Balken),
            die Fangzeiten sind grau hinterlegt. Die Anteile der Alterklassen und Geschlechter sind den unteren Diagrammen zu entnehmen,
            der Anteil unbestimmter Vögel wird in Prozent für das Frühjahr und den Herbst angegeben und ist grau dargestellt. &ndash;
            <i> Mean number of first captures per 5d since 2000 (blue bars), catching seasons are given in grey.
            Charts below show proportions of age classes and sexes, proportions of unidentified birds are given for spring and autumn and shown in grey. </i>")
          if (ring_type() == 3)
            out <- HTML("Dargestellt ist die Gesamtzahl der Erstfänge pro Pentade über alle Jahre, die Fangzeiten sind grau hinterlegt. &ndash;
            <i> Total number of first captures per 5d over all years, catching seasons are given in grey. </i>")
  
        } else out <- HTML("Zu wenige Daten oder keine standardisierte Erfassung. &ndash; <i> Data deficit or not standardised monitored. </i>")
  
        out
  
      }
    
    })

  })


  ##### observation data #####

  beobachtung <- reactive({
    beob[beob$NAME_SPECIES==input$sp,]
  })
  
  beob_type <- reactive({artenl$beobachtung_plot[artenl$Art==input$sp][1]})

  observe({
    
    if (beob_type() == 1) {

      scarce <- data.frame(date=unique(beobachtung()$DATE), decade=ceiling(yday(unique(beobachtung()$DATE))/10)) %>%
                group_by(decade) %>%
                summarise(n=n(), .groups = "drop")

    }
    
    if (beob_type() == 2) {
    
      # mean max per 10d #
      
      tmp.max <- beobachtung() %>%
        filter(DATE_YEAR>=year_start) %>%
        group_by(DATE_YEAR, DATE_DECADE) %>%
        summarise(max=max(TOTAL_COUNT), .groups = 'drop_last') %>%
        group_by(DATE_DECADE) %>%
        mutate(mean=mean(max, na.rm = TRUE), n_years=length(max)) %>%
        distinct(DATE_DECADE, .keep_all = TRUE) %>%
        arrange(DATE_DECADE) 
      
      tmp.max <- left_join(data.frame(DATE_DECADE=c(1:37)), tmp.max[,c(2,4,5)], by="DATE_DECADE")
      
      tmp.max[is.na(tmp.max)] <- 0
      
      # % in comlete lists #
      
      tmp.beob <- beobachtung() %>%
        filter(FULL_FORM==1) %>%
        group_by(DATE_DECADE, DATE_JDAY) %>%
        summarise(.groups = "drop_last") %>%
        mutate(n=length(DATE_JDAY))
  
      t.list <- beob %>%
        filter(FULL_FORM==1) %>%
        group_by(DATE_DECADE, DATE_JDAY) %>%
        summarise(.groups = "drop_last") %>%
        mutate(n.list=length(DATE_JDAY))
  
      tmp.rel <- merge(tmp.beob, t.list, by="DATE_DECADE", full=T)   # merge
      tmp.rel$q <- tmp.rel$n/tmp.rel$n.list   # % complete lists with records
      
    }
    
    output$beobachtung <- renderPlot(plot_obs(tmp.max, tmp.rel, scarce, 
                                              type = beob_type()))

    output$beobachtung_text <- renderText({
      
      if (input$show_descriptions == TRUE) {
      
        if (beob_type() == 0)
          out <- HTML("Zu wenige Daten oder keine standardisierte Erfassung. &ndash; <i> Data deficit or not standardised monitored. </i>")
        if (beob_type() == 1)
          out <- HTML("Dargestellt ist die Anzahl der Tage mit Beobachtungen pro Dekade. &ndash; <i> Number of days with observations per 10d. </i>")
        if (beob_type() == 2)
          out <- HTML(paste("Dargestellt ist das mittlere Dekandenmaximum seit", year_start, "(oben, 
                die Farbe der Punkte gibt die Anzahl Jahre mit Beobachtungsdaten an. 
                Je mehr Jahre mit Beobachtungen vorliegen, desto belastbarer das Dekadenmaximum) 
                sowie der relative Anteil von vollständigen Beobachtungslisten mit Nachweisen pro Dekade (unten). &ndash;
                <i> Mean 10d-maximum since", year_start, "(upper, colour of dots refers to the number of years with observations. 
                The more years with data are available, the more reliable is the 10d-max.) 
                together with the relative amount of complete cecklistes with observations per 10d (lower). </i>"))
  
        out
        
      }

    })

  })
  
  
  ##### phenology over year #####
  
  p_year.s <- reactive({   # spring
    data_ring[data_ring$Art==input$sp &
              data_ring$RTYPE=="e" &
              data_ring$DATE%in%s.season &
              data_ring$Year%in%year_start:year_end,]
  })

  p_year.a <- reactive({   # autumn
    data_ring[data_ring$Art==input$sp &
                data_ring$RTYPE=="e" &
                data_ring$DATE%in%a.season &
                data_ring$Year%in%year_start:year_end,]
  })

  observe({
    output$pheno_year <- if (artenl$pheno_year[artenl$Art==input$sp][1] == "TRUE") {
      renderPlot(zug(data1=p_year.a(), data2=p_year.s()))
    }
    
    output$pheno_year_table <- if (artenl$pheno_year[artenl$Art==input$sp][1] == "TRUE") {
      function() {pheno_table %>%
        kable(escape=F, align = "r") %>%
        kable_styling(bootstrap_options = "striped", full_width = F)
      }
    }
    
    output$pheno_year_text <- renderText({
      
      if (input$show_descriptions == TRUE) {

        if (artenl$pheno_year[artenl$Art==input$sp][1] == "TRUE") {
  
          out <- HTML("Dargestellt ist der jährliche Median (Punkte) mit den 0,25/0,75 (dicke vertikale Linien) und
                 0,1/0,9 Quantilen (dünne vertikale Linien) sowie der Durchzugsmedian über alle Jahre (blaue Linie)
                 und lineare Trends für Median und 0,1/0,9 Quantile (gestrichelte Linien). &ndash;
                 <i> Migration timing over years: annual median (dots) together with 0.25/0.75 (thick vertival lines) and 
                 0.1/0.9 quantiles (thin vertical lines), median over all years (blue line)
                 and linear trends for median and 0.1/0.9 quantiles (dashed lines). </i>")
  
        } else out <- HTML("Zu wenige Daten oder keine standardisierte Erfassung. &ndash; <i> Data deficit or not standardised monitored. </i>")
  
        out
        
      }

    })
    
  })

  
  ########## for compare species ##########
  
  ##### ringing data #####
  
  tmp.ring_2 <- reactive({data_ring %>%
      filter(RTYPE=="e", Art==input$sp_2)
  })
  
  ring_type_2 <- reactive({artenl$ringing_type[artenl$Art==input$sp_2][1]})
  
  observe({
    
    output$pheno_species_2 <- renderText({
      HTML(paste(h3(input$sp_2), artenl$Species[artenl$Art==input$sp_2][1], "&ndash; <i>", 
                 artenl$`wiss. Name`[artenl$Art==input$sp_2][1], "</i><br><br>"))
    })
    
    output$ringing_2 <- if (artenl$ringing_plot[artenl$Art==input$sp_2][1] == "TRUE") {
      
      renderPlot(
        
        # type=0: nur durchschnittliche Fangzahlen
        # type=1: berechnete Fangzahlen mit Altersverhältnis
        # type=2: berechnete Fangzahlen mit Alters- und Geschlechtsverhältnis
        # type=3: Fangzahlen gesamt
        pheno(d=tmp.ring_2(), type = ring_type_2())
        
      )
      
    }
    
    output$ringing_text_2 <- renderText({
      
      if (input$show_descriptions == TRUE) {
      
        if (artenl$ringing_plot[artenl$Art==input$sp_2][1] == "TRUE") {
          
          if (ring_type_2() == 0)
            out <- HTML("Dargestellt ist die durchschnittliche Anzahl der Erstfänge pro Pentade seit 2000, die Fangzeiten sind grau hinterlegt. &ndash; 
            <i> Mean number of first captures per 5d since 2000, catching seasons are given in grey. </i>")
          if (ring_type_2() == 1)
            out <- HTML("Dargestellt ist die durchschnittliche Anzahl der Erstfänge pro Pentade seit 2000 (blaue Balken),
            die Fangzeiten sind grau hinterlegt. Die Anteile der Alterklassen sind dem unteren Diagrammen zu entnehmen,
            der Anteil unbestimmter Vögel wird in Prozent für das Frühjahr und den Herbst angegeben und ist grau dargestellt. &ndash;
            <i> Mean number of first captures per 5d since 2000 (blue bars), catching seasons are given in grey. 
            Chart below shows proportions of age classes, proportions of unidentified birds are given for spring and autumn and shown in grey. </i>")
          if (ring_type_2() == 2)
            out <- HTML("Dargestellt ist die durchschnittliche Anzahl der Erstfänge pro Pentade seit 2000 (blaue Balken),
            die Fangzeiten sind grau hinterlegt. Die Anteile der Alterklassen und Geschlechter sind den unteren Diagrammen zu entnehmen,
            der Anteil unbestimmter Vögel wird in Prozent für das Frühjahr und den Herbst angegeben und ist grau dargestellt. &ndash;
            <i> Mean number of first captures per 5d since 2000 (blue bars), catching seasons are given in grey.
            Charts below show proportions of age classes and sexes, proportions of unidentified birds are given for spring and autumn and shown in grey. </i>")
          if (ring_type_2() == 3)
            out <- HTML("Dargestellt ist die Gesamtzahl der Erstfänge pro Pentade über alle Jahre, die Fangzeiten sind grau hinterlegt. &ndash;
            <i> Total number of first captures per 5d over all years, catching seasons are given in grey. </i>")
          
        } else out <- HTML("Zu wenige Daten oder keine standardisierte Erfassung. &ndash; <i> Data deficit or not standardised monitored. </i>")
        
        out
        
      }
      
    })
    
  })
  
  
  ##### observation data #####
  
  beobachtung_2 <- reactive({
    beob[beob$NAME_SPECIES==input$sp_2,]
  })
  
  beob_type_2 <- reactive({artenl$beobachtung_plot[artenl$Art==input$sp_2][1]})
  
  observe({
    
    if (beob_type_2() == 1) {
      
      scarce_2 <- data.frame(date=unique(beobachtung_2()$DATE), decade=ceiling(yday(unique(beobachtung_2()$DATE))/10)) %>%
        group_by(decade) %>%
        summarise(n=n(), .groups = "drop")
      
    }
    
    if (beob_type_2() == 2) {
    
      # mean max per 10d #
      
      tmp.max_2 <- beobachtung_2() %>%
        filter(DATE_YEAR>=year_start) %>%
        group_by(DATE_YEAR, DATE_DECADE) %>%
        summarise(max=max(TOTAL_COUNT), .groups = 'drop_last') %>%
        group_by(DATE_DECADE) %>%
        mutate(mean=mean(max, na.rm = TRUE), n_years=length(max)) %>%
        distinct(DATE_DECADE, .keep_all = TRUE) %>%
        arrange(DATE_DECADE) 
      
      tmp.max_2 <- left_join(data.frame(DATE_DECADE=c(1:37)), tmp.max_2[,c(2,4,5)], by="DATE_DECADE")
      
      tmp.max_2[is.na(tmp.max_2)] <- 0
      
      # % in comlete lists #
      
      tmp.beob_2 <- beobachtung_2() %>%
        filter(FULL_FORM==1) %>%
        group_by(DATE_DECADE, DATE_JDAY) %>%
        summarise(.groups = "drop_last") %>%
        mutate(n=length(DATE_JDAY))
      
      t.list_2 <- beob %>%
        filter(FULL_FORM==1) %>%
        group_by(DATE_DECADE, DATE_JDAY) %>%
        summarise(.groups = "drop_last") %>%
        mutate(n.list=length(DATE_JDAY))
      
      tmp.rel_2 <- merge(tmp.beob_2, t.list_2, by="DATE_DECADE", full=T)   # merge
      tmp.rel_2$q <- tmp.rel_2$n/tmp.rel_2$n.list   # % complete lists with records
      
    }
    
    output$beobachtung_2 <- renderPlot(plot_obs(tmp.max_2, tmp.rel_2, scarce_2, 
                                              type = beob_type_2()))
    
    output$beobachtung_text_2 <- renderText({
      
      if (input$show_descriptions == TRUE) {
      
        if (beob_type_2() == 0)
          out <- HTML("Zu wenige Daten oder keine standardisierte Erfassung. &ndash; <i> Data deficit or not standardised monitored. </i>")
        if (beob_type_2() == 1)
          out <- HTML("Dargestellt ist die Anzahl der Tage mit Beobachtungen pro Dekade. &ndash; <i> Number of days with observations per 10d. </i>")
        if (beob_type_2() == 2)
          out <- HTML(paste("Dargestellt ist das mittlere Dekandenmaximum seit", year_start, "(oben, 
                die Farbe der Punkte gibt die Anzahl Jahre mit Beobachtungsdaten an. 
                Je mehr Jahre mit Beobachtungen vorliegen, desto belastbarer das Dekadenmaximum) 
                sowie der relative Anteil von vollständigen Beobachtungslisten mit Nachweisen pro Dekade (unten). &ndash;
                <i> Mean 10d-maximum since", year_start, "(upper, colour of dots refers to the number of years with observations. 
                The more years with data are available, the more reliable is the 10d-max.) 
                together with the relative amount of complete cecklistes with observations per 10d (lower). </i>"))
        
        out
        
      }
      
    })
    
  })
  
  
  ##### phenology over year #####
  
  p_year.s_2 <- reactive({   # spring
    data_ring[data_ring$Art==input$sp_2 &
                data_ring$RTYPE=="e" &
                data_ring$DATE%in%s.season &
                data_ring$Year%in%year_start:year_end,]
  })
  
  p_year.a_2 <- reactive({   # autumn
    data_ring[data_ring$Art==input$sp_2 &
                data_ring$RTYPE=="e" &
                data_ring$DATE%in%a.season &
                data_ring$Year%in%year_start:year_end,]
  })
  
  observe({
    output$pheno_year_2 <- if (artenl$pheno_year[artenl$Art==input$sp_2][1] == "TRUE") {
      renderPlot(zug(data1=p_year.a_2(), data2=p_year.s_2()))
    }
    
    output$pheno_year_table_2 <- if (artenl$pheno_year[artenl$Art==input$sp_2][1] == "TRUE") {
      function() {pheno_table %>%
          kable(escape=F, align = "r") %>%
          kable_styling(bootstrap_options = "striped", full_width = F)
      }
    }
    
    output$pheno_year_text_2 <- renderText({
      
      if (input$show_descriptions == TRUE) {
      
        if (artenl$pheno_year[artenl$Art==input$sp_2][1] == "TRUE") {
          
          out <- HTML("Dargestellt ist der jährliche Median (Punkte) mit den 0,25/0,75 (dicke vertikale Linien) und
                 0,1/0,9 Quantilen (dünne vertikale Linien) sowie der Durchzugsmedian über alle Jahre (blaue Linie)
                 und lineare Trends für Median und 0,1/0,9 Quantile (gestrichelte Linien). &ndash;
                 <i> Migration timing over years: annual median (dots) together with 0.25/0.75 (thick vertival lines) and 
                 0.1/0.9 quantiles (thin vertical lines), median over all years (blue line)
                 and linear trends for median and 0.1/0.9 quantiles (dashed lines). </i>")
          
        } else out <- HTML("Zu wenige Daten oder keine standardisierte Erfassung. &ndash; <i> Data deficit or not standardised monitored. </i>")
        
        out
        
      }
      
    })
    
  })

  
  ########## observeEvent compare species ##########
  
  observeEvent(input$compare_pheno, {
    hide("compare_pheno")
    toggle("show_sp_2")
    toggle("pheno_species_2")
    toggle("pheno_box_1")
    toggle("pheno_box_2")
    toggle("pheno_box_3")
  })
  
  observeEvent(input$compare_pheno_2, {    
    shinyjs::show("compare_pheno")
    toggle("show_sp_2")
    toggle("pheno_species_2")
    toggle("pheno_box_1")
    toggle("pheno_box_2")
    toggle("pheno_box_3")
  })

  
  ##############################
  ### vagrants
  ##############################

  vagrant <- reactive({c(
    data_ring$DATE[data_ring$Art==input$vag_sp],
    as.Date(beob$DATE[beob$NAME_SPECIES==input$vag_sp])
    )})

  observe({
    
    
    ########## species ##########
    
    output$vagrant_species <- renderText({
      HTML(paste(h3(input$vag_sp), artenl$Species[artenl$Art==input$vag_sp], "&ndash; <i>", 
                 artenl$`wiss. Name`[artenl$Art==input$vag_sp], "</i><br><br>"))
    })


    ########## per year ##########

    vag <- data.frame(date=unique(vagrant()), year=year(unique(vagrant()))) %>%
      group_by(year) %>%
      summarise(n=n(), .groups = "drop")

    output$vag_year <- renderPlot(
      
      vag_year(vag)

    )


    ########## per 10d ##########

    vag_pheno <- data.frame(date=unique(vagrant()), decade=ceiling(yday(unique(vagrant()))/10)) %>%
      group_by(decade) %>%
      summarise(n=n(), .groups = "drop")

    output$vag_pheno <- renderPlot(

      vag_pheno(vag_pheno)

    )

  })


  ##############################
  ### recovery map
  ##############################
  
  
  ########## show map_panel only on desktop device ##########
  
  output$map_panel <- renderUI({
    if (!input$isMobile) {
      div(id="map_panel", 
          absolutePanel(top = 80, right = -280, width = 600, draggable = TRUE, 
                        box(title = "Info", status = "success",
                            infoBoxOutput("rec_map_ringed", width = 12),
                            infoBoxOutput("rec_map_rate", width = 12),
                            infoBoxOutput("max_TAD", width = 12),
                            HTML(paste0(
                              "<div class='infohover action-button' id='show_TAD'>",
                              "<i class='fa fa-search-location' style='font-size: 2em;'></i>",
                              "<div class='direct' id='popup_TAD'>",
                              "<h3>Info</h3>",
                              "<p>hier klicken, um den ältesten Vogel auf der Karte zu sehen.</p>",
                              "</div>",
                              "</div>"
                            )),
                            HTML(paste0(
                              "<div class='infohover action-button' id='show_speed'>",
                              "<i class='fa fa-tachometer-alt' style='font-size: 2em;'></i>",
                              "<div class='direct'>",
                              "<h3>Info</h3>",
                              "<p>hier klicken, um den Vogel mit dem schnellsten Zug auf der Karte zu sehen.</p>",
                              "</div>",
                              "</div>"
                            )),
                            collapsible = TRUE
                        )
          )
      )
    }
  })
    
  
  ########## data ##########

  data_map <- reactive({

    ring_1 <- get_month(input$season_ring[1])
    ring_2 <- get_month(input$season_ring[2])
    rec_1  <- get_month(input$season_rec[1])
    rec_2  <- get_month(input$season_rec[2])

    if (ring_1 <= ring_2)
      if (rec_1 <= rec_2)
        subset(data, Art == input$Art & (BDAM >= ring_1 & BDAM <= ring_2) &
                 (FDAM >= rec_1 & FDAM <= rec_2))
      else
        subset(data, Art == input$Art & (BDAM >= ring_1 & BDAM <= ring_2) &
                 (FDAM >= rec_1 | FDAM <= rec_2))
    else
      if (rec_1 <= rec_2)
      subset(data, Art == input$Art & (BDAM >= ring_1 | BDAM <= ring_2) &
               (FDAM >= rec_1 & FDAM <= rec_2))
      else
      subset(data, Art == input$Art & BDAM >= ring_1 | BDAM <= ring_2 &
               (FDAM >= rec_1 | FDAM <= rec_2))

  })
  
  data_map_2 <- reactive({
    
    ring_1 <- get_month(input$season_ring[1])
    ring_2 <- get_month(input$season_ring[2])
    rec_1  <- get_month(input$season_rec[1])
    rec_2  <- get_month(input$season_rec[2])
    
    if (ring_1 <= ring_2) 
      if (rec_1 <= rec_2)
        subset(data, Art == input$Art_2 & (BDAM >= ring_1 & BDAM <= ring_2) & 
                 (FDAM >= rec_1 & FDAM <= rec_2))
    else 
      subset(data, Art == input$Art_2 & (BDAM >= ring_1 & BDAM <= ring_2) & 
               (FDAM >= rec_1 | FDAM <= rec_2))
    else
      if (rec_1 <= rec_2)
        subset(data, Art == input$Art_2 & (BDAM >= ring_1 | BDAM <= ring_2) & 
                 (FDAM >= rec_1 & FDAM <= rec_2))
    else
      subset(data, Art == input$Art_2 & BDAM >= ring_1 | BDAM <= ring_2 & 
               (FDAM >= rec_1 | FDAM <= rec_2))
    
  })
  
  
  ########## for selected species ##########
  
  ##### leaflet map #####

  observe({
    
    data_map <- data_map()
    data_map$long <- jitter(data_map$long, factor=2, amount=0.01)
    data_map$lat <- jitter(data_map$lat, factor=2, amount=0.01)

    output$map <- map(data=data_map)

    g <- leafletProxy("map", data = data_map) %>%
      clearShapes() %>% clearPopups()

    g

    # lines for direct recaptures #

    if (input$direct) {

      map_direct(g, data_map)

    }
    
    if (length(data_map$RNF)>0) {

      # point colors #
      
      if (input$map_color=="Entfernung") {
        pal <- colorNumeric(palette = "viridis", domain = data_map$ENT)
        col_values <- data_map$ENT
        unit <- "Entfernung [km]"
      }
      
      if (input$map_color=="Tagesdifferenz") {
        pal <- colorNumeric(palette = "viridis", domain = data_map$TAD)
        col_values <- data_map$TAD
        unit <- "Zeitdifferenz [d]"
      }
      
      if (input$map_color=="Zuggeschwindigkeit") {
        if (all(is.na(data_map$speed))) {
          dom <- c(0,10)
        } else
          dom <- data_map$speed
        pal <- colorNumeric(palette = "viridis", domain = dom)
        col_values <- data_map$speed
        unit <- "Geschwindigkeit [km/d]"
      }
      
      # points for recoveries #
  
      if (input$map_color%in%c("Beringungsort", "Fundzustand")) {
        if (input$map_color=="Beringungsort") {
          g <- g %>%
            addMarkers(lng = long.oie, lat = lat.oie) %>% clearControls() %>%
            addCircles(lng = ~long, lat = ~lat, color = ~rec_pal(type), radius = 100, weight = 20, layerId = ~RNF) %>%
            fitBounds(~min(c(long,long.oie)), ~min(c(lat, lat.oie)), ~max(c(long, long.oie)), ~max(c(lat, lat.oie))) %>%
            addLegend("bottomright",
                      labels = c("Beringungsorte von auf der Greifswalder Oie kontrollierten Vögeln", "Fundorte von auf der Greifswalder Oie beringten Vögeln"),
                      colors = c("#73D055FF", "#440154FF"))
          g
        } else {
          factpal <- colorFactor(palette = c("#3c8dbc", "#FFC000", "blue"), 
                                 levels = c("lebend", "tot", "unbekannt"))
          g <- g %>%
            addMarkers(lng = long.oie, lat = lat.oie) %>% clearControls() %>%
            addCircles(lng = ~long, lat = ~lat, color = ~factpal(dead_or_alive), radius = 100, weight = 20, layerId = ~RNF) %>%
            fitBounds(~min(c(long,long.oie)), ~min(c(lat, lat.oie)), ~max(c(long, long.oie)), ~max(c(lat, lat.oie))) %>%
            addLegend("bottomright", title = "Fundzustand", pal = factpal, values = ~dead_or_alive, na.label = "Beringungsorte")
          g
        }
      } else {
        g <- g %>%
          addMarkers(lng = long.oie, lat = lat.oie) %>% clearControls() %>%
          addCircles(lng = ~long, lat = ~lat, color = ~pal(col_values), radius = 100, weight = 20, layerId = ~RNF) %>%
          fitBounds(~min(c(long,long.oie)), ~min(c(lat, lat.oie)), ~max(c(long, long.oie)), ~max(c(lat, lat.oie))) %>%
          addLegend("bottomright", title = unit, pal = pal, values = col_values, opacity = 0.8, labFormat = labelFormat(digits=0, big.mark=""))
        g
      }
      
      if (input$map_color=="Zuggeschwindigkeit") {
        g <- g %>%
          addLegend("bottomleft", title = "Es werden nur Wiederfunde innerhalb von 100 Tagen berücksichtigt", colors = list())
        g
      }
      
    }
    
    # show a popup when info icons is clicked #
    
    observeEvent(input$show_TAD, {
      data_map <- data_map()
      if (length(data_map$RNF)>0) {
        data_max_TAD <- data_map %>% filter(TAD == max(TAD))
        leafletProxy("map") %>% clearPopups() %>%
          fitBounds(min(c(data_map$long,long.oie)), min(c(data_map$lat, lat.oie)), max(c(data_map$long, long.oie)), max(c(data_map$lat, lat.oie)))
        
        showRecPopup(data_max_TAD$RNF, data_max_TAD$lat, data_max_TAD$long, "map")
        
      }
    }, ignoreInit=TRUE)
    
    observeEvent(input$show_speed, {
      data_map <- data_map()
      if (length(data_map$RNF)>0 & !all(is.na(data_map$speed))) {
        data_max_speed <- data_map %>% filter(speed == max(speed, na.rm=T))
        leafletProxy("map") %>% clearPopups() %>%
          fitBounds(min(c(data_map$long,long.oie)), min(c(data_map$lat, lat.oie)), max(c(data_map$long, long.oie)), max(c(data_map$lat, lat.oie)))
        
        showRecPopup(data_max_speed$RNF, data_max_speed$lat, data_max_speed$long, "map")
        
      }
    }, ignoreInit=TRUE)

  })
  
  ##### infoBoxes #####
  
  map_ringed <- reactive({
    
    ring_1 <- get_month(input$season_ring[1])
    ring_2 <- get_month(input$season_ring[2])
    
    if (ring_1 <= ring_2)
      subset(data_ring, Art == input$Art & RTYPE=="e" & Month >= ring_1 & Month <= ring_2)
    else
      subset(data_ring, Art == input$Art & RTYPE=="e" & (Month >= ring_1 | Month <= ring_2))
  })
  
  map_rec <- reactive({
    
    ring_1 <- get_month(input$season_ring[1])
    ring_2 <- get_month(input$season_ring[2])
    
    if (ring_1 <= ring_2)
      subset(data, Art == input$Art & BDAM >= ring_1 & BDAM <= ring_2 & FORT!="Greifswalder Oie")
    else
      subset(data, Art == input$Art & (BDAM >= ring_1 | BDAM <= ring_2) & FORT!="Greifswalder Oie")
  })
  
  observe({

    ringed <- length(map_ringed()$RTYPE)
    fund_rate <- round(length(unique(map_rec()$RNF)) / ringed *100, 2)

    output$rec_map_ringed <- renderInfoBox(
      infoBox("", ringed, subtitle = paste0("auf der Oie beringt (", input$season_ring[1], "-", input$season_ring[2], ")"), 
               icon=icon("ring"), color="light-blue")
    )

    output$rec_map_rate <- renderInfoBox(
        infoBox("", subtitle = "Wiederfundrate von auf der Oie beringten Vögeln", paste(fund_rate, "%"), icon=icon("percent"), color="light-blue")
    )
    
    output$max_TAD <- renderInfoBox(
        infoBox("", subtitle = "max. Zeitdifferenz", value=paste(round(max(data_map()$TAD)/365, 1), "Jahr(e)"), 
                icon = icon("hourglass-half"), color = "light-blue")
    )
    
  })
  
  ##### show a popup at the given location #####

  observe({
    leafletProxy("map") %>% clearPopups()
    event <- input$map_shape_click
    if (is.null(event))
      return()
    showRecPopup(event$id, event$lat, event$lng, "map")
  })

  
  ########## for compare species ##########

  ##### leaflet map 1 #####

  observe({
    
    data_map <- data_map()
    data_map$long <- jitter(data_map$long, factor=2, amount=0.01)
    data_map$lat <- jitter(data_map$lat, factor=2, amount=0.01)

      output$map_1 <- map(data=data_map, col="Beringungsort")

    g_1 <- leafletProxy("map_1", data = data_map) %>%
      clearShapes() %>% clearPopups()

    g_1

    # lines for direct recaptures #

    if (input$direct) {

      map_direct(g_1, data_map)

    }

    # points for recoveries #

    g_1 <- g_1 %>%
      addMarkers(lng = long.oie, lat = lat.oie) %>%
      addCircles(lng = ~long, lat = ~lat, color = ~rec_pal(type), radius = 100, weight = 20, layerId = ~RNF) %>%
      fitBounds(~min(c(long,long.oie)), ~min(c(lat, lat.oie)), ~max(c(long, long.oie)), ~max(c(lat, lat.oie)))

    g_1

  })

  ##### leaflet map 2 #####

  observe({
    
    data_map_2 <- data_map_2()
    data_map_2$long <- jitter(data_map_2$long, factor=2, amount=0.01)
    data_map_2$lat <- jitter(data_map_2$lat, factor=2, amount=0.01)
    
    output$map_2 <- map(data=data_map_2, col="Beringungsort")

    g_2 <- leafletProxy("map_2", data = data_map_2) %>%
      clearShapes() %>% clearPopups()
    
    g_2

    # lines for direct recaptures #

    if (input$direct) {
      
      map_direct(g_2, data=data_map_2)

    }

    # points for recoveries #

    g_2 <- g_2 %>%
      addMarkers(lng = long.oie, lat = lat.oie) %>%
      addCircles(lng = ~long, lat = ~lat, color = ~rec_pal(type), radius = 100, weight = 20, layerId = ~RNF) %>%
      fitBounds(~min(c(long,long.oie)), ~min(c(lat, lat.oie)), ~max(c(long, long.oie)), ~max(c(lat, lat.oie)))

    g_2
    
  })


  ##### show a popup at the given location #####

  observe({
    leafletProxy("map_1") %>% clearPopups()
    event <- input$map_1_shape_click
    if (is.null(event))
      return()
    showRecPopup(event$id, event$lat, event$lng, "map_1")
  })

  observe({
    leafletProxy("map_2") %>% clearPopups()
    event <- input$map_2_shape_click
    if (is.null(event))
      return()
    showRecPopup(event$id, event$lat, event$lng, "map_2")
  })
  
  
  ########## observeEvents if no data ##########
  
  observeEvent(input$season_ring, {
    if (length(data_map()$RNF)==0) {
      hide("map_panel")
      showModal(
        modalDialog(title = "Oops!",
                    "Keine Daten. Wähle einen Zeitraum von maximal einem Jahr aus, um Daten anzuzeigen.",
                    footer = NULL, easyClose = TRUE)
      )
    } else shinyjs::show("map_panel")
    
  })
  
  observeEvent(input$season_rec, {
    if (length(data_map()$RNF)==0) {
      hide("map_panel")
      showModal(
        modalDialog(title = "Oops!",
                    "Keine Daten. Wähle einen Zeitraum von maximal einem Jahr aus, um Daten anzuzeigen.",
                    footer = NULL, easyClose = TRUE)
      )
    } else shinyjs::show("map_panel")
    
  })
  
  observeEvent(input$Art, {
    if (length(data_map()$RNF)==0) {
      hide("map_panel")
      showModal(
        modalDialog(title = "Oops!",
                    "Keine Daten, bitte den Beringungs- bzw. Wiederfundzeitraum erweitern.",
                    footer = NULL, easyClose = TRUE)
      )
    } else shinyjs::show("map_panel")
    
  })
  
  
  ########## observeEvent compare species ##########
  
  observeEvent(input$compare_Wiederfundkarte, {
    hide("compare_Wiederfundkarte")
    toggle("show_Art_2")
    toggle("main_map")
    toggle("compare_map")
    toggle("map_panel")
    toggle("map_color")
    updateCheckboxInput(session, "direct", value = 0)
    updateSelectInput(session, "map_color", selected = "Beringungsort")
  })
  
  observeEvent(input$compare_Wiederfundkarte_2, {
    shinyjs::show("compare_Wiederfundkarte")
    toggle("show_Art_2")
    toggle("main_map")
    toggle("compare_map")
    toggle("map_panel")
    toggle("map_color")
    updateCheckboxInput(session, "direct", value = 0)
  })
  
  
  ########## observeEvent map_color ##########

  observeEvent(input$map_color, {
    updateCheckboxInput(session, "direct", value = 0)
  })
  
  ##############################
  ### about
  ##############################
  
  output$about <- renderUI({
    includeMarkdown("about_text.md")
  })
  
}
