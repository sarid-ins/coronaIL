# vaccine uptake rates per city

library(shiny)
library(tidyverse)
library(shinythemes)



# Read data (see scripts/pre_process.R for details) -----------------------

vacci <- read_csv("data/vacci.csv")


# List options ------------------------------------------------------------

cities_list <- vacci %>% 
    distinct(city_name) %>% 
    pull(city_name)

age_groups_list <- c("0-19",
                "20-39",
                "40-59",
                "60+")

dose_list <- c("Dose 1" = 1,
               "Dose 2" = 2)


# The chart creation ------------------------------------------------------

create_vaccination_data <- function(cities_select = c("תל אביב - יפו",
                                                      "נתניה",
                                                      "חיפה"), 
                                    dose_select = 1, 
                                    age_group_select = "60+") {
    vacci %>% 
        filter(city_name %in% cities_select,
               dose == dose_select,
               age_group %in% age_group_select) %>% 
        group_by(city_name, Date) %>% 
        summarize(prop = sum(vaccinated)/sum(population)) %>%
        mutate(prop = if_else(prop > 1, 1, prop)) %>% 
        ggplot(aes(x = Date, y = prop, color = city_name)) + 
        geom_line(size = 1) + 
        saridr::theme_sarid() + 
        ylab("Population [%]") + 
        ggtitle("\u202bחיסוני COVID19 בישראל לפי רשויות",
                subtitle = paste0("\u202bקבוצות גיל: ", 
                                  paste0(age_group_select, collapse = ";"))) +
        scale_y_continuous(labels = scales::percent_format(1)) + 
        guides(color = guide_legend("עיר")) +
        labs(caption = "\u202bמקור הנתונים: מאגר COVID-19 של משרד הבריאות מתחסנים על פי יישוב\n\u202bויז'ואליזציה מכון שריד: https://www.sarid-ins.co.il") + 
        theme(plot.title = element_text(hjust = 1),
              plot.subtitle = element_text(hjust = 1))
}

# The app UI --------------------------------------------------------------

ui <- fluidPage(theme = shinytheme("cerulean"), 
                title = "Vaccinations in Israel",
                sidebarLayout(
                    sidebarPanel(width = 4,
                                 h2("Chart parameters"),
                                 selectInput("choose_cities",
                                             label = "Towns",
                                             choices = cities_list,
                                             selected = c("תל אביב - יפו",
                                                          "ירושלים",
                                                          "חיפה",
                                                          "ראשון לציון"),
                                             multiple = T),
                                 selectInput("age_group_select",
                                             label = "Age group",
                                             choices = age_groups_list,
                                             multiple = T,
                                             selected = "60+"),
                                 radioButtons("dose", label = "Dose",
                                              choices = dose_list,
                                              selected = 1),
                                 actionButton("refresh_button",
                                              "Update chart",
                                              icon = icon("refresh"))
                                 ),
                    mainPanel(width = 8,
                              h1("Vaccination rates in Israel"),
                              plotOutput("out_chart"),
                              h4("Sources:"),
                              HTML("<p>Town population by age from <a href='https://data.gov.il/dataset/residents_in_israel_by_communities_and_age_groups/resource/64edd0ee-3d5d-43ce-8562-c336c24dbc1f'>MOI</a></p>"),
                              HTML("<p>Vaccination rates from <a href='https://data.gov.il/dataset/covid-19/resource/12c9045c-1bf4-478a-a9e1-1e876cc2e182'>MOH</a></p>"),
                              HTML("<p>App by <a href='https://www.sarid-ins.co.il'>Sarid Research Institute</a></p>"),
                              img(src = "logo_eng_2.png", width = "150px")
                    )
                )
)


# The app server logic ----------------------------------------------------

server <- function(input, output) {
    
    # create the plot
    output$out_chart <- renderPlot({
        
        input$refresh_button
        
        create_vaccination_data(cities_select = isolate(input$choose_cities),
                                dose_select = isolate(input$dose),
                                age_group_select = isolate(input$age_group_select))
    })
    
}

shinyApp(ui = ui, server = server)
