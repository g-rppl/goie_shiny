
##############################
###
### SETUP FOR GOIE_SPECIES
###
##############################
# 
# - load required package
# - load species lists
# - read data
# - define colours


##############################
### load required packages
##############################

library(shinydashboard)
library(shinydashboardPlus)
library(shinyjs)
library(shinyWidgets)
library(shiny.pwa)
library(shinycssloaders)
library(shinyalert)
library(leaflet)
library(ggplot2)
library(readxl)
library(dplyr)
library(tidyr)
library(kableExtra)
  options(knitr.kable.NA = '')
library(DT)
library(lubridate)
library(markdown)
library(knitr)

  
##############################
### species list
##############################

artenl <- read_excel("./data/Gesamtartenliste.xlsx")
artenl$Art <- as.factor(artenl$Art)


##############################
### breeding bird data
##############################

bvk <- read.csv("./data/GOie_breeding.csv")


##############################
### ringing data
##############################

data_ring <- read.csv("./data/GOie_ringing.csv")
data_ring$DATE <- as.Date(data_ring$DATE)


########## variables ##########

##### subsetting #####

year_start <- 2000   # standardised since 2000
year_end   <- 2022

year_start.s <- 2000   # first spring for zug() plot
year_start.a <- 2000   # first autumn for zug() plot

min_pen_sum <- 5      # min. mean sum per pentade for age / sex

##### catching seasons #####

s.season <- c(sapply(as.Date(paste(1995:year_end, "03-15", sep="-")), "+", seq(0, 86)))   # whole spring
a.season <- c(sapply(as.Date(paste(1994:year_end, "08-01", sep="-")), "+", seq(0, 97)))   # whole autumn

##### months #####

month_name <- c("Jan","Feb","Mär","Apr","Mai","Jun","Jul","Aug","Sep","Okt","Nov","Dez","+Jan","+Feb","+Mär")


##############################
### observation data
##############################

beob <- read.csv("./data/GOie_observation.csv")
beob_m <- read.csv("./data/GOie_observation_manuell.csv")
beob <- rbind(beob, beob_m)
beob$DATE <- as.Date(beob$DATE)


##############################
### geographic recovery data
##############################

long.oie <- 13.9162   # coordinates GOie
lat.oie  <- 54.2465   # coordinates GOie

data <- read.csv("./data/GOie_recovery.csv")

data$dead_or_alive[data$FORT=="Greifswalder Oie"] <- NA
data$Ursache[data$FORT=="Greifswalder Oie"] <- NA


##############################
### colours
##############################

js <- rgb(57,135,184, maxColorValue=255)   # Jordsand blue
col.juv <- "orange2"
col.ad <- "olivedrab"
col.m <- "firebrick"
col.f <- "turquoise4"

rec_pal <- colorFactor(palette = c("#440154FF", "#73D055FF"), 
                       levels = c("rec", "con"))
