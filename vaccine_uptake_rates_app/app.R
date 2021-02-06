# vaccine uptake rates per city

library(shiny)
library(tidyverse)
library(shinythemes)


# Read data (see scripts/pre_process.R for details) -----------------------

vacci <- read_csv("data/vacci.csv")

vacci_agg <- vacci %>% 
    filter(!is.na(population)) %>% 
    group_by(CityName, dose, Date, population) %>% 
    summarize(total_vaccinated = sum(vaccinated)) %>% 
    mutate(dose = paste0("dose", dose)) %>% 
    pivot_wider(names_from = dose, values_from = total_vaccinated) %>% 
    mutate(dose1_prcnt = dose1/population,
           dose2_prcnt = dose2/population)

# Define UI for application that draws a histogram
ui <- tagList(
    navbarPage(theme = "cerulean")
)

# Define server logic required to draw a histogram
server <- function(input, output) {

}

# Run the application 
shinyApp(ui = ui, server = server)
