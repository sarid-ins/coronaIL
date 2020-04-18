#' israel_data UI Function
#'
#' @description Shiny module controlling the tab for Israel data
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_israel_data_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      box(radioButtons(ns("data_source"), label = "Choose data source",
                       choices = c("Shaviv (updates two times per day)" = "shaviv", 
                                   "Dardikman-Hashkes (updates once a day)" = "dardikman_hashkes"),
                       selected = "dardikman_hashkes",
                       inline = TRUE),
          width = 12,
          title = "Data source... to replace data source click on the (+) to the far right --->",
          collapsible = T,
          collapsed = T,
          status = "info",
          p("In both cases, data is extracted from MOH's telegram messages, but in different times. In long range both sources should be fairly consistent."),
          p("Shaviv (Ilan Shaviv) is processing and tidying the data from MOH Telegram reports."),
          HTML("<p>Dardikman-Hashkes: do the same and post them on 
                                 <a href = 'https://github.com/idandrd/israel-covid19-data'>github</a> 
                                 and <a href = 'https://covid19data.co.il/'>here</a>.</p>"))
    ),
    fluidRow(valueBoxOutput(ns("last_updated"), width = 3),
             valueBoxOutput(ns("currently_confirmed"), width = 3),
             valueBoxOutput(ns("currently_seriously_ill"), width = 3),
             valueBoxOutput(ns("currently_deaths"), width = 3),
             ),
    fluidRow(box(title = "Plot properties", 
                 status = "primary",
                 selectInput(inputId = ns("plot_selector"),
                             label = "Show on plot",
                             choices = c("dead", "severe", "moderate", "mild", "recovered", "total"),
                             selected = "total",
                             multiple = F),
                 checkboxInput(ns("log_scale_chart"), label = "Log scale", value = T)
                 ),
             box(title = "Model fit controls",
                 status = "primary",
                 checkboxInput(ns("include_prediction"), "Include exponential fit", value = TRUE),
                 column(width = 9,
                        dateRangeInput(ns("train_set_cutoff"), label = "Train set range", 
                                       min = "2020-02-28",
                                       max = Sys.Date(),
                                       start = "2020-03-01", 
                                       end = Sys.Date() - 3)
                        ),
                 column(width = 3,
                        dateInput(ns("test_horizon"), label = "Extrapolation horizon",
                                  min = "2020-03-30",
                                  max = Sys.Date() + 10,
                                  value = Sys.Date() +3)
                        )
                 )
             ),
    fluidRow(
      box(width = 12, status = "primary",
          plotOutput(ns("tracking_plot"), 
                     brush = brushOpts(id = ns("plot_brush_location"),
                                       direction = "x"),
                     height = "400px"))
    ),
    fluidRow(
      box(width = 12, status = "primary",
          plotOutput(ns("log_ratio_plot"),
                     height = "400px"),
          footer = "The inspiration to add this chart came from Rob J. Hyndman's blog (https://robjhyndman.com/hyndsight/logratios-covid19/)")
    ),
    fluidRow(
      box(width = 12, status = "primary",
          downloadButton(ns("export_raw_data"), "Export raw data"),
          DTOutput(ns("plot_brush_DT")))
    ),
    fluidRow(
      textOutput(ns("debug_text"))
    )
    )
}
    
#' israel_data Server Function
#'
#' @noRd 
mod_israel_data_server <- function(input, output, session, israel_data_raw){
  ns <- session$ns
  
  use_source <- reactiveVal(1)
  last_updated <- reactiveVal({
    israel_data_raw[[1]] %>%
      pull(reporting_date) %>% 
      max()})
  
  observe({
    
    req(last_updated())
    req(use_source())
    
    if (input$data_source == "shaviv"){
      cat("\nUsing Shaviv\n")
      use_source(1)
      last_updated({
        israel_data_raw[[1]] %>%
          pull(reporting_date) %>% 
          max()})
    } else if (input$data_source == "dardikman_hashkes"){
      cat("\nUsing Dardikman and Hashkes\n")
      use_source(2)
      last_updated({
        israel_data_raw[[2]] %>%
          pull(reporting_date) %>% 
          max()})
    }
  })
  
  israel_plot_data <- reactive({
    req(use_source())
    
    if (input$include_prediction){
      exp_prediction <- exponent_predict(israel_data_raw[[use_source()]],
                                         train_set_cutoff = input$train_set_cutoff,
                                         test_set_range = input$test_horizon) %>% 
        filter(str_detect(series_type, input$plot_selector))
      
      israel_data_raw[[use_source()]] %>%
        pivot_longer(-reporting_date, "series_type", "value") %>%
        filter(series_type %in% input$plot_selector) %>% 
        bind_rows(exp_prediction) %>% 
        arrange(desc(series_type)) %>% 
        mutate(series_type = fct_inorder(series_type))
      
    } else {
      israel_data_raw[[use_source()]] %>%
        pivot_longer(-reporting_date, "series_type", "value") %>%
        filter(series_type %in% input$plot_selector)
    }
    
  })
  
  output$tracking_plot <- renderPlot({
    
    req(use_source())
    validate(need(NROW(israel_plot_data()) > 0, "Not available in current source, try changing data source."))
    
    my_color_scale <- c("dead" = "#d53e4f",
                        "severe" = "#fc8d59",
                        "moderate" = "#fee08b",
                        "mild" = "#e6f598",
                        "recovered" = "#99d594",
                        "total" = "#3288bd",
                        "fit: dead" = "#d53e4f",
                        "fit: severe" = "#fc8d59",
                        "fit: moderate" = "#fee08b",
                        "fit: mild" = "#e6f598",
                        "fit: recovered" = "#99d594",
                        "fit: total" = "#3288bd")
    
    israel_status_plot <- israel_plot_data() %>% 
      ggplot(aes(x = reporting_date, y = value, shape = series_type, linetype = series_type)) +
      geom_point() +
      geom_line() +
      saridr::theme_sarid() + 
      xlab("") + 
      ylab("# Cases") + 
      labs(caption = "Data source: MOH Telegram reports\nDashboard generated by Sarid Research Institute: https://www.sarid-ins.co.il") + 
      ggtitle(paste0("Israel daily Corona cases (", input$plot_selector, ")"))
      
    
    if (input$log_scale_chart) {
      israel_status_plot <- israel_status_plot + scale_y_log10()
    }
    
    if (input$include_prediction) {
      
      israel_status_plot <- israel_status_plot + 
        geom_vline(xintercept = as.numeric(input$train_set_cutoff)*24*60*60, color = "red")+
        annotate(geom = "text",
                 x = as.POSIXct((as.numeric(input$train_set_cutoff))*24*60*60, origin = "1970-01-01"), 
                 y = c(10, 10), 
                 label = c(" Train set -->","<-- Train set "), 
                 hjust = c(0, 1),
                 color ="red")
    }
    
    israel_status_plot

  })
  
  output$log_ratio_plot <- renderPlot({
    
    req(use_source())
    
    
    israel_data_raw[[use_source()]] %>% 
      pivot_longer(-reporting_date, "series_type", "value") %>%
      filter(series_type %in% "total") %>%
      mutate(log_ratio = log(value)-log(lag(value))) %>% 
      ggplot(aes(x = reporting_date, y = log_ratio)) + 
      geom_hline(yintercept = log(2)/c(2, 3, 4, 7, 14, 21), col = "grey") + 
      geom_point(alpha = 0.2) + 
      geom_smooth(method = "loess", se = FALSE) +
      scale_y_continuous(
        "Daily increase in cumulative cases",
        breaks = log(1+seq(0,60,by=10)/100),
        labels = paste0(seq(0,60,by=10),"%"),
        minor_breaks=NULL,
        sec.axis = sec_axis(~ {log(2)/.},
                            breaks = c(2, 3, 4, 7, 14, 21),
                            name = "Doubling time (days)")) +
      saridr::theme_sarid() +
      xlab("") + 
      ylab("# Cases") + 
      labs(caption = "Data source: MOH Telegram reports\nDashboard generated by Sarid Research Institute: https://www.sarid-ins.co.il") + 
      ggtitle("Israel log-ratio of daily Corona cases (total)") + 
      coord_cartesian(ylim = c(0, 0.6))
      
    
  })
  
  output$export_raw_data <- downloadHandler(filename = function(){
    paste0(Sys.Date(), " - ", input$data_source, ".csv")
  },
  content = function(file){
    write_csv(israel_data_raw[[use_source()]], file)
  })
  
  output$plot_brush_DT <- renderDT({
    
    validate(need(input$plot_brush_location, "Drag range on plot to view data (doesn't work on mobile devices)"))
    
    israel_data_raw[[use_source()]] %>% 
      filter(reporting_date >= input$plot_brush_location[1] &
               reporting_date <= input$plot_brush_location[2]) %>% 
      datatable(class = "compact", rownames = F, autoHideNavigation = T,
                extensions = "Buttons",
                options = list(dom = "lBtip",
                               buttons = c("csv", "excel")))
  })
  
  output$last_updated <- renderValueBox({
    reporting_day <- as.character(last_updated()) %>% str_sub(end = 10)
    reporting_time <- as.character(last_updated()) %>% str_sub(start = 12, end = 16)
    if (reporting_time == "00:00:00" | reporting_time == ""){
      reporting_time <- "Last updated"
    } else {
      reporting_time <- paste0("Last updated (", reporting_time, ")")
    }
    valueBox(value = reporting_day,
             subtitle = reporting_time,
             icon = icon("calendar"),
             color = "light-blue")
  })
  
  output$currently_confirmed <- renderValueBox(
    israel_data_raw[[use_source()]] %>% 
      slice(NROW(reporting_date)) %>% 
      pull(total) %>% 
      valueBox(
        subtitle = "Cumulative confirmed cases",
        icon = icon("procedures"),
        color = "aqua"
        )
  )
  
  output$currently_seriously_ill <- renderValueBox(
    israel_data_raw[[use_source()]] %>% 
      slice(NROW(reporting_date)) %>% 
      pull(severe) %>% 
      valueBox(
        subtitle = "Currently seriously Ill",
        icon = icon("syringe"),
        color = "orange")
  )
  
  output$currently_deaths <- renderValueBox(
    israel_data_raw[[use_source()]] %>% 
      slice(NROW(reporting_date)) %>% 
      pull(dead) %>% 
      valueBox(
        subtitle = "Cumulative deaths",
        icon = icon("skull"),
        color = "red")
  )
 
}
    
## To be copied in the UI
# mod_israel_data_ui("israel_data_ui_1")
    
## To be copied in the server
# callModule(mod_israel_data_server, "israel_data_ui_1")
 
