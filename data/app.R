require(stringr)
require(data.table)
require(lubridate)
require(shiny)
require(plotly)
require(shinyWidgets)
require(shinyjs)


load("cleanSubmissions.RData")

uniqInsts <- sort(unique(cleanSubmissions$institution))
uniqMajors <- sort(unique(cleanSubmissions$major))
uniqSems <- unique(cleanSubmissions$sem)

# UI definition ####
ui <- fluidPage(
  title = "GradCafe Statistics",
  
  sidebarLayout(
    # Inputs
    sidebarPanel(
      width = 2,
      
      useShinyjs(),
      # Select variable for institution
      pickerInput(inputId = "inst"
                  , label = "Institution:"
                  , choices = c("Any", uniqInsts)
                  # , selected = "Any"
                  , multiple = T
      )
      # Select variable for major
      , pickerInput(inputId = "major"
                    , label = "Major:"
                    , choices = c("Any", uniqMajors)
                    # , selected = "Any"
                    , multiple = T
      )
      # Select variable for degree
      , pickerInput(inputId = "degree"
                    , label = "Degree:"
                    , choices = c("Master's", "PhD")
                    , selected = c("Master's", "PhD")
                    , multiple = T
      )
      # Select variable for student type
      , pickerInput(inputId = "studentType"
                    , label = "Student Type:"
                    , choices = c("American" = "A"
                                  , "International, US degree" = "U"
                                  , "International, no US degree" = "I"
                                  , "Other" = "O")
                    , selected = c("A", "U", "I", "O")
                    , multiple = T
      )
      # Select variable for semester
      , pickerInput(inputId = "sem"
                    , label = "Semester:"
                    , choices = uniqSems
                    , selected = {
                      "F18"
                    }
                    , multiple = T
      )
      , tags$a("Donate to support me!", href = "https://paypal.me/alparibal"
               , target = "_blank", class = ".text-danger")
      , tags$br()
      , "or"
      , tags$br()
      , tags$a("Send feedback!", href = "mailto:fretpwner@gmail.com"
               , class = "text-warning")
    ),
    
    # Outputs
    mainPanel(
      width = 10
      , verticalLayout(
        tags$div(id = "divCharts",
          tags$hr()
          ,
          # fluidRow(
          plotlyOutput(outputId = "timeline")
          # )
          ,
          tags$hr()
          ,
          # fluidRow(
          plotlyOutput(outputId = "GREbox")
          # )
          ,
          tags$hr()
          ,
          # fluidRow(
          plotlyOutput(outputId = "GPAhist")
          # )
        )
        ,
        textOutput(outputId = "NoResText")
      )
    )
  )
)

# Server definition ####
server <- function(input, output){
  
  filteredData <- reactive({
    cleanSubmissions[(input$inst == "Any" | institution %in% input$inst) &
                       (input$major == "Any" | major %in% input$major) &
                       (input$degree == "Any" | degree %in% input$degree) &
                       (input$studentType == "Any" | studentType %in% input$studentType) &
                       (input$sem == "Any" | sem %in% input$sem)
                     ]
  })
  
  output$NoResText <- renderText({
    shinyjs::show(id = "divCharts")
    req(nrow(filteredData()) == 0)
    # shinyjs::hide(id = "timeline")
    # shinyjs::hide(id = "GREbox")
    # shinyjs::hide(id = "GPAhist")   
    shinyjs::hide(id = "divCharts")

    "There are no records for the current selection!"
  })
  
  output$timeline <- renderPlotly({
    req(nrow(filteredData()) > 0)
    timeline <- filteredData()[as.integer(str_sub(sem, start=2)) - (year(notif_date) %% 100) <= 1, .(notif_date, notif_result)]
    timeline <- timeline[, .(N = .N), by = .(notif_result, notif_date)]
    calendar <- CJ(notif_date = as.Date(timeline[, min(notif_date, na.rm = T)]:
                                          timeline[, max(notif_date, na.rm = T)]
                                        , origin = "1970-01-01")
                   , notif_result = timeline[, unique(notif_result)])
    timeline <- merge(timeline, calendar, by = c("notif_date", "notif_result"), all = T)
    setorder(timeline, notif_date)
    timeline[is.na(N), N := 0]
    timeline[, cumN := cumsum(N), by = .(notif_result)]
    
    plot_ly(timeline, x = ~notif_date, y = ~cumN, type = "scatter", mode = "lines"
            , linetype = ~notif_result) %>% 
      layout(title = "Decision Timeline"
             , xaxis = list(title = "Date")
             , yaxis = list(title = "Cumulative # of Notifications")
             , hovermode = "x") %>% 
      config(displaylogo = F, displayModeBar = "hover", collaborate = F
             # , scrollZoom = T
             , modeBarButtonsToRemove = list("hoverClosestCartesian"
                                             , "hoverCompareCartesian"
                                             , "toggleSpikelines"))
  })
  
  output$GREbox <- renderPlotly({
    req(nrow(filteredData()) > 0)
    GREbox <- melt(data = filteredData()[, .(gre_v, gre_q, notif_result)]
                   , id.vars = "notif_result", measure.vars = c("gre_v", "gre_q")
                   , variable.name = "Subject", value.name = "Score", na.rm = T)
    setnames(GREbox, "notif_result", "Result")
    GREbox[Subject == "gre_v", Subject := "Verbal"]
    GREbox[Subject == "gre_q", Subject := "Quant"]
    GREbox[, Result := paste0(Result, " (n="
                              , format(.N, format = "f", big.mark = ",")
                              ,")"), by = Result]
    plot_ly(GREbox, x = ~Subject, y = ~Score, color = ~Result, type = "box") %>%
      layout(title = "GRE Score Distributions"
             , boxmode = "group"
             , hovermode = "x") %>%
      config(displaylogo = F, displayModeBar = "hover", collaborate = F
             # , scrollZoom = T
             , modeBarButtonsToRemove = list("hoverClosestCartesian"
                                             , "hoverCompareCartesian"
                                             , "toggleSpikelines"))
  })
  
  output$GPAhist <- renderPlotly({
    req(nrow(filteredData()) > 0)
    GPAhist <- filteredData()[gpa > 2.5, .(gpa = round(gpa, 1), notif_result)]
    counts <- GPAhist[, .(N = .N), by = notif_result]
    setorder(counts, -N)
    GPAhist[, notif_result:= factor(notif_result
                                    , levels = counts$notif_result)]
    plot_ly(data = GPAhist, x = ~gpa, type = "histogram"
            , color = ~notif_result) %>%
      layout(title = "GPA Distribution"
             , barmode = "overlay"
             , hovermode = "x") %>%
      config(displaylogo = F, displayModeBar = "hover", collaborate = F
             # , scrollZoom = T
             , modeBarButtonsToRemove = list("hoverClosestCartesian"
                                             , "hoverCompareCartesian"
                                             , "toggleSpikelines"
                                             , "lasso2d"
                                             , "select2d"))
  })
}

shinyApp(ui = ui, server = server)
