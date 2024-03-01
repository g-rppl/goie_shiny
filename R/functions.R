
##############################
###
### FUNCTIONS FOR GOIE_SHINY
###
##############################
# 
# - function for data info: data_info
# - function for breeding data: breeding
# - function for first captures per year: plot_ring_count
# - function for yearly data: plot_year
# - function for yearly table: table_year
# - function for mean phenology: pheno 
# - function for observation data: plot_obs
# - function for phenology over years: zug
# - function for vagrant data: vag_year
# - function for vagrant data: vag_pheno
# - function to get No. of Month: get_month
# - function for map data: map
# - function for direct recaptures: map_direct
# - show a popup at the given location: showRecPopup


##############################
### function for data info: data_info
##############################

data_info <- function(data=NULL, x=NULL, y=NULL, col_breaks) {
  
  if (col_breaks== 5){at1 <- 0.5; at2 <- 72.5; at.1 <- 4; at.2 <- 70}
  if (col_breaks==10){at1 <- 0.5; at2 <- 36.5; at.1 <- 2; at.2 <- 35}
  
  rbPal <- colorRampPalette(c("grey90", js))
  data$Col <- rbPal(col_breaks)[as.numeric(cut(log(data$n), breaks = col_breaks))]
  
  layout(matrix(c(1,2), 1, 2,), width=c(6,1))
  
  par(mar=c(2,5,1,0))
  plot(x, y, las=1, pch=15, cex=1.5, col=data$Col, xaxt="n", ylim=c(1994, max(y, na.rm=T)),
       ylab = "", xlab = "", cex.axis=1.2)
  
  axis(side=1, labels=F,  at=seq(at1, at2, length=13))
  axis(side=1, labels=c("J","F","M","A","M","J","J","A","S","O","N","D"),  
       at=seq(at.1, at.2, length=12), tick=F, line=-0.3, cex.axis=1.2)
  mtext(side=2, line=3.5, cex=1.5, "Jahr")
  
  par(mar=c(9,1,9,0.5))
  legend_image <- as.raster(matrix(rbPal(col_breaks), ncol=1))
  plot(c(0,2), c(0,1), type = 'n', axes=F, xlab = '', ylab = '')
  text(x=1.5, y=c(0, 1), labels = c(col_breaks, 1))
  rasterImage(legend_image, 0,0,1,1)

}


##############################
### function for breeding data: breeding
##############################

breeding <- function(data=NULL) {
  
  gg <- ggplot(data, aes(x=year, y=count)) +
    geom_bar(stat="identity", fill=js, width=0.8) + xlab("Jahr") + ylab("Anzahl") + 
    scale_x_continuous(limits = c(1993, year_end+1), breaks = seq(1995, 2030, 5)) +
    scale_y_continuous(breaks = function(x) unique(floor(pretty(seq(0, (max(x) + 1) * 1.1))))) +
    theme_classic(base_size = 20)
  
  return(gg)
  
}


##############################
### function for first captures per year: plot_ring_count
##############################

plot_ring_count <- function(data=NULL, select=NULL) {
  
  if (select!="gesamtes Jahr") {
    
    data <- data %>%
      filter(season==select)
    
  }
  
  gg <- ggplot(data) + 
    geom_bar(aes(x=Year, y=n, fill=season), stat = 'identity', width=0.8) +
    xlab("Jahr") + ylab("Erstfänge") + labs(fill="") +
    scale_x_continuous(limits = c(1993, year_end+1), breaks = seq(1995, 2030, 5)) +
    scale_y_continuous(breaks = function(x) unique(floor(pretty(seq(0, (max(x) + 1) * 1.1))))) +
    scale_fill_manual(values=c("Frühjahr"=js, "Herbst"="#73D055FF")) +
    theme_classic(base_size = 20) + theme(legend.position="top")
  
  return(gg)

}


##############################
### function for yearly data: plot_year
##############################

plot_year <- function(tmp_sum=NULL, tmp_mean=NULL) {

  tmp_sum <- tmp_sum %>%
    group_by(pentade) %>%
    summarise(n=n(), .groups = 'drop')
  
  tmp_mean <- tmp_mean %>%
    filter(RTYPE=="e", Year%in%2000:year_end) %>%
    group_by(pentade) %>%
    summarise(n=n(), q=length(RTYPE)/(year_end-1999), .groups = 'drop')
  
  max <- max(c(tmp_sum$n, tmp_mean$q))
  
  par(mar=c(2,4.7,0.5,0.5), cex.axis=1.2, cex.lab=1.5)
  
  plot(tmp_sum, type="h", xaxt="n", xlab="", ylab="", las=1, xlim=c(3,71), ylim=c(0,1.1*max), 
       lwd=8, lend=1, col=js, cex.main=1.5, bty="L")
  lines(tmp_mean$q ~ tmp_mean$pentade, subset=tmp_mean$pentade%in%14:33, lwd=5, lty=3)     # mean Frühjahr
  lines(tmp_mean$q ~ tmp_mean$pentade, subset=tmp_mean$pentade%in%42:63, lwd=5, lty=3)     # mean Herbst
  
  axis(side=1, labels=F,  at=seq(1,73, length=13))
  axis(side=1, labels=c("J","F","M","A","M","J","J","A","S","O","N","D"),  
       at=seq(4,70, length=12), tick=F, line=-0.5)
  mtext(side=2, "Anzahl", cex=1.5, line=3.5)

}


##############################
### function for yearly table: table_year
##############################

table_year <- function(data=NULL, year_select=NULL, season=NULL) {

  min_ring <- 10   # min 1st captures for deviation
  
  if (season == "s") {
    d <- data %>% filter(RTYPE=="e", DATE%in%s.season) 
    year_select <- as.numeric(year_select)-1994   # column of selected year
  }
  if (season == "a") {
    d <- data %>% filter(RTYPE=="e", DATE%in%a.season)
    year_select <- as.numeric(year_select)-1993   # column of selected year
  }
  
  tmp <- table(d$Art, d$Year)
  
  sp <- sort(unique(d$Art))
  
  t <- data.frame(matrix(nrow=length(sp), ncol=6))
  colnames(t) <- c("Art", "Min", "Max", "Mittel", "Aktuell", "Abweichung [%]")
  
  for (i in sp) {
    
    t[i,1] <- i
    t[i,2] <- min(tmp[i,6:ncol(tmp)])   # min 2000 - year_end
    t[i,3] <- max(tmp[i,1:ncol(tmp)])   # max 1995 - year_end
    t[i,4] <- round(mean(tmp[i,6:ncol(tmp)]), 0)   # mean 2000 - year_end
    t[i,5] <- tmp[i,year_select]   # selected year
    t[i,6] <- ifelse(t[i,4]>=min_ring, round(t[i,5] / t[i,4] * 100, 0), NA)   # deviation
    
    rownames(t) <- NULL
    
    t <- t %>%
      filter(Aktuell>0, !is.na(t$Art)) %>%
      arrange(desc(Aktuell))

  }
  
  return(t)

}


##############################
### function for mean phenology: pheno 
##############################

########## helper function: fn ##########

fn <- function(x) {round(length(x)/(year_end-year_start+1),0)}


########## main function ##########

pheno <- function(d=NULL, type=2, ylab="Erstfänge (Pentadenmittel)", col=js) {
  
  segwidth <- 7   # width of segments
  
  if (type==3) {
    
    cexlables <- 1
    
    dat <- d %>%
      group_by(RTYPE, pentade) %>%
      summarise(n=n(), .groups = "drop_last")
    
    upr <- ifelse(!all(is.na(dat$n)), max(dat$n, na.rm=T), NA)
    
    par(mar=c(2,5,1,1))
    
    plot(dat$n ~ dat$pentade, type="n", main="",  xlim=c(2,72), ylim=c(0,upr),
         xaxt="n", xlab="", ylab="", las=1, cex.axis=cexlables)
    
    rect(15.5,upr+100,32.4,-100, col=rgb(0.5,0.5,0.5,1/3), border="white")
    rect(42.5,upr+100,62.1,-100, col=rgb(0.5,0.5,0.5,1/3), border="white")
    
    segments(dat$pentade, 0, dat$pentade, as.numeric(dat$n), lwd=segwidth, lend=1, col="black")
    
    axis(side=1, labels=F,  at=seq(0.5,72.5, length=13))
    axis(side=1, labels=c("J","F","M","A","M","J","J","A","S","O","N","D"),  
         at=seq(4,70, length=12), tick=F, line=-0.6)
    mtext(side=2, line=3.4, cex=1.2, "Erstfänge gesamt")
    
  } else {
  
    dat <- d %>%
      filter(Year%in%year_start:year_end) %>%
      group_by(RTYPE, pentade) %>%
      summarise(n=n(), plot=fn(RTYPE), .groups = "drop_last")
    
    upr <- ifelse(!all(is.na(dat$plot)), max(dat$plot, na.rm=T), NA)
    
    
    ########## plot ##########
    
    if (type==0) {cexlables <- 1}
    
    if (type==1) {
      layout(matrix(c(1,2), 2, 1, byrow=T), heights=c(3,1))
      cexlables <- 1
    }
    
    if (type==2) {
      layout(matrix(c(1,2,3), 3, 1, byrow=T), heights=c(3,1,1))
      cexlables <- 1.5
    }
    
    ##### mean first captures per pentade #####
    
    par(mar=c(2,5,1,1))
    
    plot(dat$plot ~ dat$pentade, type="n", main="",  xlim=c(2,72), ylim=c(0,upr),
         xaxt="n", xlab="", ylab="", las=1, cex.axis=cexlables)
    
    rect(15.5,upr+100,32.4,-100, col=rgb(0.5,0.5,0.5,1/3), border="white")
    rect(42.5,upr+100,62.1,-100, col=rgb(0.5,0.5,0.5,1/3), border="white")
    
    segments(dat$pentade, 0, dat$pentade, dat$plot, lwd=segwidth, lend=1, col=col)
    
    axis(side=1, labels=F,  at=seq(0.5,72.5, length=13))
    axis(side=1, labels=c("J","F","M","A","M","J","J","A","S","O","N","D"),  
         at=seq(4,70, length=12), tick=F, line=-0.3, cex.axis=cexlables)
    mtext(side=2, line=3.4, cex=1.2, ylab)
    
    if (type %in% c(1,2)) {
    
      ##### proportions of age classes #####
      
      # autumn #
      
      data <- d %>%
        filter(Year%in%year_start:year_end, pentade>=37, AGE%in%c(2,3,4)) %>%
        group_by(pentade, RTYPE, AGE) %>%
        summarise(n=n(), mean=fn(RTYPE), .groups = "drop_last") %>% 
        mutate(pen_sum=sum(mean), rel=ifelse(pen_sum>=min_pen_sum, mean / pen_sum, NA)) %>%
        complete(AGE) %>% filter(AGE%in%c(2,3,4)) %>% arrange(pentade) %>% 
        ungroup() %>% complete(pentade, AGE)
      
      par(mar=c(1,5,0,1))
      
      plot(c(0,1) ~ c(2,72), type="n", xaxt="n", xlab="", yaxt="n", ylab="")
      
      abline(v=37, lty=2, lwd=2)
      
      mtext(side=2, line=3.4, cex=1.2, "Alter")
      
      if (sum(data$rel, na.rm=T)!=0) {
        
        segments(data$pentade[data$AGE==4], 1, data$pentade[data$AGE==4], 
                 1-data$rel[data$AGE==4], lwd=segwidth, lend=1, col=col.ad)   # adult
        segments(data$pentade[data$AGE==3], 1-data$rel[data$AGE==4], data$pentade[data$AGE==3], 
                 data$rel[data$AGE==3], lwd=segwidth, lend=1, col="grey")   # unknown
        segments(data$pentade[data$AGE==3], 0, data$pentade[data$AGE==3], 
                 data$rel[data$AGE==3], lwd=segwidth, lend=1, col=col.juv)   # 1st cy
        
        text(73.5,0.1, "KJ1 ", pos=2, cex=cexlables); text(73,0.85, "ad.", pos=2, cex=cexlables)
        
        unid <- round(nrow(subset(d, pentade>37 & AGE%in%2))
                      /nrow(subset(d, pentade>37 & AGE%in%c(2,3,4)))*100, 1)
        text(73,0.5, paste(unid,"%"), pos=2, cex=cexlables-0.3)
        
      }
      
      # spring #
      
      data <- d %>%
        filter(Year%in%year_start:year_end, pentade<37, AGE%in%c(4,5,6)) %>%
        group_by(pentade, RTYPE, AGE) %>%
        summarise(n=n(), mean=fn(RTYPE), .groups = "drop_last") %>% 
        mutate(pen_sum=sum(mean), rel=ifelse(pen_sum>=min_pen_sum, mean / pen_sum, NA)) %>%
        complete(AGE) %>% filter(AGE%in%c(4,5,6)) %>% arrange(pentade) %>% 
        ungroup() %>% complete(pentade, AGE)
      
      if (!all(is.na(data$rel)) & mean(data$rel, na.rm=T)!=1) {
        
        segments(data$pentade[data$AGE==6], 1, data$pentade[data$AGE==6], 
                 1-data$rel[data$AGE==6], lwd=segwidth, lend=1, col=col.ad)   # adult
        segments(data$pentade[data$AGE==4], 1-data$rel[data$AGE==6], data$pentade[data$AGE==4], 
                 data$rel[data$AGE==5], lwd=segwidth, lend=1, col="grey")   # unknown
        segments(data$pentade[data$AGE==5], 0, data$pentade[data$AGE==5],
                 data$rel[data$AGE==5], lwd=segwidth, lend=1, col=col.juv)   # 2nd cy
        
        text(0,0.1, "KJ2 ", pos=4, cex=cexlables); text(0,0.85, "ad.", pos=4, cex=cexlables)
        
        unid <- round(nrow(subset(d, pentade<37 & AGE%in%4))
                      /nrow(subset(d, pentade<37 & AGE%in%c(4,5,6)))*100, 1)
        text(0,0.5, paste(unid,"%"), pos=4, cex=cexlables-0.3)
        
      }
    
    }
    
    if (type == 2) {
    
      ##### proportions of sexes #####
      
      # autumn #
      
      data <- d %>%
        group_by(pentade, RTYPE, SEX) %>%
        summarise(n=n(), mean=fn(RTYPE), .groups = "drop_last") %>% complete(SEX) %>%
        mutate(pen_sum=sum(mean), rel=ifelse(pen_sum>=min_pen_sum, mean / pen_sum, NA)) %>%
        filter(pentade>37) %>% ungroup() %>% complete(pentade, SEX)
      
      plot(c(0,1) ~ c(2,72), type="n", xaxt="n", xlab="", yaxt="n", ylab="")
      
      abline(v=37, lty=2, lwd=2)
      
      mtext(side=2, line=3.4, cex=cexlables-0.3, "Geschlecht")
      
      if (!all(is.na(data$rel)) & mean(data$rel, na.rm=T)!=1) {
        
        segments(data$pentade[data$SEX==2], 1, data$pentade[data$SEX==2], 
                 1-data$rel[data$SEX==2], lwd=segwidth, lend=1, col="turquoise4")
        segments(data$pentade[data$SEX==0], 1-data$rel[data$SEX==2], data$pentade[data$SEX==0], 
                 data$rel[data$SEX==1], lwd=segwidth, lend=1, col="grey")
        segments(data$pentade[data$SEX==1], 0, data$pentade[data$SEX==1], 
                 data$rel[data$SEX==1], lwd=segwidth, lend=1, col="firebrick")
        
        text(73.5,0.1, "\u2642", pos=2, cex=cexlables*2); text(73.5,0.8, "\u2640", pos=2, cex=cexlables*2)
        
        unid <- round(sum(data$n[data$SEX==0], na.rm = T)/sum(data$n, na.rm = T)*100, 1)
        text(73,0.5, paste(unid,"%"), pos=2, cex=cexlables-0.3)
        
      }
      
      # spring #
      
      data <- d %>%
        group_by(pentade, RTYPE, SEX) %>%
        summarise(n=n(), mean=fn(RTYPE), .groups = "drop_last") %>% complete(SEX) %>%
        mutate(pen_sum=sum(mean), rel=ifelse(pen_sum>=min_pen_sum, mean / pen_sum, NA))%>%
        filter(pentade<37) %>% ungroup() %>% complete(pentade, SEX)
      
      if (!all(is.na(data$rel)) & mean(data$rel, na.rm=T)!=1) {
        
        segments(data$pentade[data$SEX==2], 1, data$pentade[data$SEX==2], 
                 1-data$rel[data$SEX==2], lwd=segwidth, lend=1, col="turquoise4")
        segments(data$pentade[data$SEX==0], 1-data$rel[data$SEX==2], data$pentade[data$SEX==0], 
                 data$rel[data$SEX==1], lwd=segwidth, lend=1, col="grey")
        segments(data$pentade[data$SEX==1], 0, data$pentade[data$SEX==1], 
                 data$rel[data$SEX==1], lwd=segwidth, lend=1, col="firebrick")
        
        text(-0.5,0.1, "\u2642", pos=4, cex=3); text(-0.5,0.8, "\u2640", pos=4, cex=3)
        
        unid <- round(sum(data$n[data$SEX==0], na.rm = T)/sum(data$n, na.rm = T)*100, 1)
        text(0,0.5, paste(unid,"%"), pos=4, cex=1.2)
        
      }
    }
  }
}  


##############################
### function for observation data: plot_obs
##############################

plot_obs <- function(data1=NULL, data2=NULL, scarce=NULL, type=NULL) {
  
  if (type == 0) return()
  
  if (type == 1) {
    
    plot(scarce$decade, scarce$n, type="h", bty="L", col=js, lwd=10, lend=1, xlim=c(1, 37),
         ylim=c(0, max(scarce$n, na.rm=T)*1.1), ylab="Tage mit Beobachtungen", xaxt="n", yaxt="n", xlab="", cex.lab=1.2)
    axis(side=1, labels=F, at=seq(0.5,37.5, length=13))
    axis(side=1, labels=c("J","F","M","A","M","J","J","A","S","O","N","D"),
         at=seq(2,36, length=12), tick=F, line=-0.3, cex.axis=1.2)
    axis(side=2, 1:30, at=1:30, cex.axis=1.2, las=1)
    
  }
  
  if (type == 2) {
  
    rbPal <- colorRampPalette(c('white', js))
    data1$Col <- rbPal(year_end-year_start+1)[as.numeric(cut(data1$n_years, breaks = max(data1$n_years)))]
    
    layout(matrix(c(1:3), 3, 1, byrow=T), heights=c(1,5,2))
    par(mar=c(0,2,1,1))
    
    legend_image <- as.raster(matrix(rbPal(year_end-year_start+1), nrow=1))
    plot(c(0,1), c(0,2), type = 'n', axes=F, xlab = '', ylab = '')
    text(x=c(0.75, 1), y = 1.5, labels = c(0, year_end-year_start+1), cex = 1.2)
    rasterImage(legend_image, 0.75,0,1,1)
    
    par(mar=c(0.5,6.5,0,1))
    
    plot(data1$DATE_DECADE, data1$mean, type="l", xaxt="n", col=js, lwd=4, bty="L", las=1,
         ylab="", ylim=c(0, max(data1$mean)*1.1), cex.axis=1.5)
    points(data1$DATE_DECADE, data1$mean, col=js, pch=21, bg=data1$Col, cex=3)
    mtext(side=2, line=4.5, cex=1.2, "Mittleres Dekadenmax.")
    
    par(mar=c(2,6.5,0,1))
    plot(data2$DATE_DECADE, data2$n, type="n", las=1, xaxt="n", yaxt="n", ylim=c(0.035,0.965),
         xlim=c(1,37), ylab = "", xlab = "")
    
    rect(data2$DATE_DECADE-0.5, 0, data2$DATE_DECADE+0.5, data2$q, border=NA, col=js)
    
    axis(side=1, labels=F,  at=seq(0.5,37.5, length=13))
    axis(side=1, labels=c("J","F","M","A","M","J","J","A","S","O","N","D"),  
         at=seq(2,36, length=12), tick=F, line=-0.2, cex.axis=1.5)
    axis(side=2, labels=c(0, "100 %"), at=0:1, las=1, cex.axis=1.5)
  
  }
  
}


##############################
### function for phenology over years: zug 
##############################

zug <- function(data1=NULL, data2=NULL, seasons="b", col1=js, col2=js, lab1="Herbst", lab2="Frühjahr", print=TRUE) {
  
  if (seasons=="b") {
    
    year_start.1 <- year_start.a
    year_start.2 <- year_start.s
    
    lab_season.1 <- c("Aug","Sep","Okt")
    lab_season.2 <- c("Mär","Apr","Mai")
    
    y_lim.1 <- c(213,310)
    y_lim.2 <- c(74,160)
    
    tick.1 <- c(213,244,274,305)
    tick.2 <- c(91,121,152)
    
    lab_at.1 <- c(228,259,289)
    lab_at.2 <- c(76,106,136)
    
  }
  
  if (seasons=="s") {
    
    year_start.1 <- year_start.2 <- year_start.s
    
    lab_season.1 <- lab_season.2 <- c("Mär","Apr","Mai")
    
    y_lim.1 <- y_lim.2 <- c(74,160)
    
    tick.1 <- tick.2 <-  c(91,121,152)
    
    lab_at.1 <- lab_at.2 <- c(76,106,136)
    
  }
  
  if (seasons=="a") {
    
    year_start.1 <- year_start.2 <- year_start.a
    
    lab_season.1 <- lab_season.2 <- c("Aug","Sep","Okt")
    
    y_lim.1 <- y_lim.2 <- c(213,310)
    
    tick.1 <- tick.2 <- c(213,244,274,305)
    
    lab_at.1 <- lab_at.2 <- c(228,259,289)
    
  }
  
  t.1.mean <- median(data1$yday)   # median for all plot 1
  t.2.mean <- median(data2$yday)   # median for all plot 2
  
  n.1 <- length(data1$yday)     # n for 1st plot
  n.2 <- length(data2$yday)     # n for 2nd plot
  
  
  ########## data ##########
  
  t.1 <- t.2 <- data.frame(Jahr=c(min(c(year_start.1, year_start.2)):year_end), q10=NA, q25=NA, median=NA, q75=NA, q90=NA)
  
  ##### 1st plot #####
  
  for (i in year_start.1:year_end){
    
    tmp <- subset(data1, Year==i)
    
    t.1[t.1$Jahr==i, 2:6] <- quantile(tmp$yday, probs = c(0.1, 0.25, 0.5, 0.75, 0.9))
    
  }
  
  ##### 2nd plot #####
  
  for (i in year_start.2:year_end){
    
    tmp <- subset(data2, Year==i)
    
    t.2[t.2$Jahr==i, 2:6] <- quantile(tmp$yday, probs = c(0.1, 0.25, 0.5, 0.75, 0.9))
    
  }
  
  if (print==TRUE) {
    
    ########## plot ##########
    
    par(mfrow=c(2,1), mar=c(0,3,0,0), omi=c(0.7,0,0.2,0.3))
    
    ##### 1st #####
    
    plot(t.1$Jahr, t.1$median, ylim=y_lim.1, xaxt="n", yaxt="n", ylab="", type="n")
    
    abline(h=t.1.mean, lwd=4, col=col1)   # median over all years
    
    abline(lm(t.1$q10 ~ t.1$Jahr), lty=3)   # trends
    lm.1 <- lm(t.1$median ~ t.1$Jahr)
    abline(lm.1, lwd=3, lty=2)              # trends
    abline(lm(t.1$q90 ~ t.1$Jahr), lty=3)   # trends
    
    segments(t.1$Jahr, t.1$q25, t.1$Jahr, t.1$q75, lwd=6, col=col1)
    segments(t.1$Jahr, t.1$q10, t.1$Jahr, t.1$q90, lwd=3, col=col1)
    points(t.1$Jahr, t.1$median, pch=19, cex=2, cex.axis=1.2, col=col1)
    
    mtext(side=2, lab1, line=1.7, cex=1.2)
    mtext(side=4, paste("n =", n.1))
    axis(side=2, labels=F,  at=tick.1)
    axis(side=2, labels=lab_season.1, at=lab_at.1, tick=F, line=-0.5)
    
    ##### 2nd #####
    
    plot(t.2$Jahr, t.2$median, ylim=y_lim.2, yaxt="n", ylab="", type="n")
    
    abline(h=t.2.mean, lwd=4, col=col2)   # median over all years
    
    abline(lm(t.2$q10 ~ t.2$Jahr), lty=3)   # trends
    lm.2 <- lm(t.2$median ~ t.2$Jahr)
    abline(lm.2, lwd=3, lty=2)              # trends
    abline(lm(t.2$q90 ~ t.2$Jahr), lty=3)   # trends
    
    segments(t.2$Jahr, t.2$q25, t.2$Jahr, t.2$q75, lwd=6, col=col2)
    segments(t.2$Jahr, t.2$q10, t.2$Jahr, t.2$q90, lwd=3, col=col2)
    points(t.2$Jahr, t.2$median, pch=19, cex=2, cex.axis=1.2, col=col2)
    
    mtext(side=2, lab2, line=1.7, cex=1.2)
    mtext(side=4, paste("n =", n.2))
    
    axis(side=2, labels=F,  at=tick.2)
    axis(side=2, labels=lab_season.2, at=lab_at.2, tick=F, line=-0.5)
    mtext(side=1, "Jahr", cex=1.2, line=2.3)
    
    
    ########## table ##########
    
    tab <- data.frame(c(lab1, lab2), Median=NA, Differenz=NA)
    colnames(tab)[1] <- ""
    
    ##### median #####
    
    tab[1,2] <- paste0(day(as.Date(round(t.1.mean, 0), origin="0001-01-01")), ". ",
                       month(as.Date(round(t.1.mean, 0), origin="0001-01-01"), label =TRUE))   # date for non-leap year
    tab[2,2] <- paste0(day(as.Date(round(t.2.mean, 0), origin="0001-01-01")), ". ",
                       month(as.Date(round(t.2.mean, 0), origin="0001-01-01"), label =TRUE))   # date for non-leap year
    
    ##### delta #####
    
    tab[1,3] <- paste(round(tail(lm.1$fitted.values, n=1) - lm.1$fitted.values[1], 0), "Tage")
    tab[2,3] <- paste(round(tail(lm.2$fitted.values, n=1) - lm.2$fitted.values[1], 0), "Tage")
    
    pheno_table <<- tab
    
  }  
  
}


##############################
### function for vagrant data: vag_year
##############################

vag_year <- function(data=NULL) {
  
  limx <- ifelse(min(data$year, na.rm = T)<1990, min(data$year, na.rm = T), 1990)

  plot(data$year, data$n, type="h", bty="L", col=js, lwd=10, lend=1, xlim=c(limx, year_end), yaxt="n",
       ylim=c(0, max(data$n, na.rm=T)*1.1), ylab="Tage mit Nachweisen", xlab="Jahr", cex.lab=1.5, cex.axis=1.5)
  axis(side=2, 1:30, at=1:30, cex.axis=1.5, las=1)

}


##############################
### function for vagrant data: vag_pheno
##############################

vag_pheno <- function(data=NULL) {
  
  plot(data$decade, data$n, type="h", bty="L", col=js, lwd=10, lend=1, xlim=c(1, 37),
       ylim=c(0, max(data$n, na.rm=T)*1.1), ylab="Tage mit Nachweisen", xaxt="n", yaxt="n", xlab="", cex.lab=1.5)
  axis(side=1, labels=F, at=seq(0.5,37.5, length=13))
  axis(side=1, labels=c("J","F","M","A","M","J","J","A","S","O","N","D"),
       at=seq(2,36, length=12), tick=F, line=-0.3, cex.axis=1.5)
  axis(side=2, 1:10, at=1:10, cex.axis=1.5, las=1)
  
}


##############################
### function to get No. of Month: get_month
##############################

get_month <- function(x) {
  
  if (x%in%c("Jan","+Jan")) x <-  1
  if (x%in%c("Feb","+Feb")) x <-  2
  if (x%in%c("Mär","+Mär")) x <-  3
  if (x=="Apr") x <-  4
  if (x=="Mai") x <-  5
  if (x=="Jun") x <-  6
  if (x=="Jul") x <-  7
  if (x=="Aug") x <-  8
  if (x=="Sep") x <-  9
  if (x=="Okt") x <- 10
  if (x=="Nov") x <- 11
  if (x=="Dez") x <- 12
  
  return(x)
  
}


##############################
### function for map data: map
##############################

map <- function(data=NULL, col=NULL) {

    renderLeaflet({leaflet(data) %>% 
    addProviderTiles(providers$CartoDB.Positron) %>%
    addMarkers(lng = long.oie, lat = lat.oie) %>%
    addCircles(lng = ~long, lat = ~lat, color = ~rec_pal(type), radius = 100, weight = 20, layerId = ~RNF) %>%
    fitBounds(~min(c(long,long.oie)), ~min(c(lat, lat.oie)), ~max(c(long, long.oie)), ~max(c(lat, lat.oie))) %>%
    addLegend("bottomright",
              labels = c("Beringungsorte von auf der Greifswalder Oie kontrollierten Vögeln", "Fundorte von auf der Greifswalder Oie beringten Vögeln"),
              colors = c("#73D055FF", "#440154FF"))
    })
    
}


##############################
### function for direct recaptures: map_direct
##############################

map_direct <- function(g, data=NULL) {

  if (nrow(data) != 0) {
    for (i in unique(data$RNF)) {
      if (data[data$RNF==i,"BDAJ"][1] == data[data$RNF==i & data$TAD==min(data$TAD[data$RNF==i]),"FDAJ"][1] &
          data[data$RNF==i,"BDAM"][1] %in% 1:7 & data[data$RNF==i & data$TAD==min(data$TAD[data$RNF==i]),"FDAM"][1] %in% 1:7) {

        g <- addPolylines(g, lat = c(tail(data[data$RNF==i, "lat"], 1), lat.oie),
                             lng = c(tail(data[data$RNF==i, "long"], 1), long.oie),
                             color = rec_pal(data$type[data$RNF==i]), layerId = paste0("_", i))
      }
      if (data[data$RNF==i,"BDAJ"][1] == data[data$RNF==i & data$TAD==min(data$TAD[data$RNF==i]),"FDAJ"][1] &
          data[data$RNF==i,"BDAM"][1] %in% 6:12 & data[data$RNF==i & data$TAD==min(data$TAD[data$RNF==i]),"FDAM"][1] %in% 6:12) {

        g <- addPolylines(g, lat = c(tail(data[data$RNF==i, "lat"], 1), lat.oie),
                             lng = c(tail(data[data$RNF==i, "long"], 1), long.oie),
                             color = rec_pal(data$type[data$RNF==i]), layerId = paste0("_", i))
      }
      if (data[data$RNF==i,"BDAJ"][1]+1 == data[data$RNF==i & data$TAD==min(data$TAD[data$RNF==i]),"FDAJ"][1] &
          data[data$RNF==i,"BDAM"][1] %in% 8:12 & data[data$RNF==i & data$TAD==min(data$TAD[data$RNF==i]),"FDAM"][1] %in% 1:3) {

        g <- addPolylines(g, lat = c(tail(data[data$RNF==i, "lat"], 1), lat.oie),
                             lng = c(tail(data[data$RNF==i, "long"], 1), long.oie),
                             color = rec_pal(data$type[data$RNF==i]), layerId = paste0("_", i))
      }
    }
  }

  return(g)

}


##############################
### show a popup at the given location: showRecPopup
##############################

showRecPopup <- function(RNF, lat, lng, proxy) {
  
  if (substr(RNF[1], 1, 1) == "_")
    RNF <- substr(RNF, 2, nchar(RNF))
  
  selectedRNF_all <- data[data$RNF == RNF[1],] %>% 
    arrange(FDAJ, FDAM, FDAT) %>%
    mutate(Datum=paste0(FDAT, ".", FDAM, ".", FDAJ),
           nach=paste(TAD, "Tagen"))
  history <- kable(selectedRNF_all[,c("Datum", "nach")], align = "r") %>%
    kable_styling(full_width = T)
  selectedRNF <- selectedRNF_all[1,]   # only first row if multiple encounters
  
  content <- as.character(tagList(
    tags$h4(substr(selectedRNF$RNF, 1, 2), ". . . .", 
            substr(selectedRNF$RNF, nchar(as.character(selectedRNF$RNF)), nchar(as.character(selectedRNF$RNF)))),
    paste0("beringt am: ", selectedRNF$BDAT, ".", selectedRNF$BDAM, ".", selectedRNF$BDAJ), br(),
    if (length(selectedRNF_all$RNF)>1)
         HTML(paste0("<I>", nrow(selectedRNF_all), " mal kontrolliert <br> erstmals am: ", 
                     selectedRNF$FDAT, ".", selectedRNF$FDAM, ".", selectedRNF$FDAJ, "</I> <br>"))
    else HTML(paste0("gefunden am: ", selectedRNF$FDAT, ".", selectedRNF$FDAM, ".", selectedRNF$FDAJ, "<br>")),
    if (selectedRNF$FUM2!=4) HTML(paste("nach", selectedRNF$TAD, "Tagen <br>"))
    else HTML("<I>nur Ring gefunden</I> <br>"),
    paste("Entfernung:", selectedRNF$ENT, "km"), br(),
    if (selectedRNF$BORT=="Greifswalder Oie")
      HTML(paste("Fundzustand:", selectedRNF$dead_or_alive, "<br>",
                 "Fundursache:", selectedRNF$Ursache)),
    if (length(selectedRNF_all$RNF)>1) 
      HTML(paste("<br>", history))
  ))
  
  leafletProxy(proxy) %>% addPopups(lng, lat, content, layerId = RNF)
  
}

