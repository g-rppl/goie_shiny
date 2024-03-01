
##############################
###
### SHINY DASHBOARD GOIE
###
##############################


##############################
### data & setup
##############################

source("./R/setup.R")   ### source data & setup
#
# - breeding bird data:       bvk
# - ringing data:             data_ring
# - observation data:         beob  
# - geographic recovery data: data (rec & con)


##############################
### plot functions
##############################

source("./R/functions.R")   ### source functions
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
### ui
##############################

source("ui.R")   ### source ui


##############################
### server
##############################

source("server.R")   ### source server


##############################
### shiny app
##############################

shinyApp(ui, server)
