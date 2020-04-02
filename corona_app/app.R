#
# The Shiny app reads all Corona updates of confirmed, dead, and recovered in Israel and the world.

library(shiny)
library(shinydashboard)
library(DT)
library(tidyverse)

# include modules
source("R/mod_israel_data.R")
source("R/exponent_fit.R")

version_data <- "Version 0.0.3 last updated 2020-04-02."

# read Israel data from the googlesheets
# Deprecated - the date format is inconsistent.
# googlesheets4::sheets_deauth()
# israel_data <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1U4Ew2a4eb0xHXLJBrATM41QfUSHJHrRaY6wCANImb_s",
#                                          col_names = c("reporting_date", "death", "serious", 
#                                                        "medium", "mild", "recovered", "total",
#                                                        "comments"),
#                                          col_types = "cddddddc",
#                                          skip = 1) %>% 
#     filter(!is.na(total)) %>% 
#     mutate(reporting_date = lubridate::dmy(reporting_date)) %>% 
#     arrange(total) %>% 
#     group_by(reporting_date) %>% 
#     slice(NROW(reporting_date))

# Reading from Ilan Shaviv's data instead
shaviv_data <- read_csv("http://old.phys.huji.ac.il/~shaviv/ilan/Israel_paitentBreakDown2.csv") %>% 
    mutate_at(vars(dead:total), ~if_else(is.nan(.), 0, .)) %>% 
    filter(total != 0) %>% 
    mutate(reporting_date = as.POSIXct(possixTime, origin = "1970-01-01")) %>% 
    select(reporting_date, dead:total) %>% 
    janitor::clean_names() %>% 
    rename(moderate = medium,
           severe = serious,
           mild = light)

# Reading from Dardikman Hashkes (https://covid19data.co.il/)
dardikman_hashkes <- read_csv("https://raw.githubusercontent.com/idandrd/israel-covid19-data/master/IsraelCOVID19.csv",
                              skip = 1,
                              col_names = c("reporting_date", "total", "new_cases", "moderate", "severe", "dead")) %>% 
    mutate(reporting_date = as.POSIXct(reporting_date, format = "%d/%m/%Y"))

# Define UI for application that draws a histogram
ui <- dashboardPage(
    header = dashboardHeader(title = "Corona App"),
    sidebar = dashboardSidebar(
        sidebarMenu(
            menuItem("Israel", tabName = "israel_tab", icon = icon("map-marked")),
            # menuItem("Global", tabName = "global_tab", icon = icon("globe")),
            menuItem("About", tabName = "about", icon = icon("info"))
            )
        ),
    body = dashboardBody(
        tags$head(tags$link(rel = "stylesheet", href = "main.css")),
        tabItems(
            tabItem("israel_tab",
                    mod_israel_data_ui("israel_data_ui_1")),
            # tabItem("global_tab"),
            tabItem("about",
                    box(HTML("<div>The app was created by <a href='https://www.sarid-ins.co.il'>Sarid Research Institute</a>.</div>
                             <div>Get in touch <a href='mailto:adi@sarid-ins.co.il'>here</a></div>"),
                        status = "primary",
                        width = 12)
                    )
            ),
        tags$footer(id = "footer",
                    HTML(paste0("<img src=sarid_logo.png style='zoom: 35%;'> The system is developed by <a href = 'http://www.sarid-ins.com'>Sarid Research Institute</a>. ", version_data))
        )
    ),
    title = "Corona App",
    skin = "blue")

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
    # Tab for Israel
    callModule(mod_israel_data_server, "israel_data_ui_1", 
               israel_data_raw = list(shaviv_data = shaviv_data,
                                      dardikman_hashkes = dardikman_hashkes))
    
}

# Run the application 
shinyApp(ui = ui, server = server)
