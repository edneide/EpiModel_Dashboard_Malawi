library(shiny)
library(shinydashboard)
library(readr)
library(plotly)

setwd("~/Documents/GitHub/EpiModel_Dashboard_Malawi")

ui <- fluidPage(
  # fluid row 
  # fluidRow(
  #   column(width = 4, offset = 6, tags$h1("Policy Levers"), tags$hr()),
  # ),
  fluidRow(
           column(width = 6,
                  "Example of graph at national level",
                  plotlyOutput("fig")
           ),
           column(width = 6,
                  tags$h1("Policy Levers"),
                  tags$hr(),
                  column(width = 4.0, tags$h5("% Masking"),
                         numericInput('mask_perc', 
                                      label = "New %",
                                      value = 0, 
                                      min = 0,
                                      max = 100),
                         # Length of Intervention # Days
                         numericInput('time_intervention_mask',
                                      label = "Length of Intervention\n(# Days)",
                                      value = 0)
                  ),
                  column(width = 4.0, tags$h5("% Physical Distancing"),
                         numericInput('distancing_perc',
                                      label = "New %",
                                      value = 0,
                                      min = 0,
                                      max = 100),
                         numericInput('time_intervention_dist',
                                      label = "Length of Intervention\n(# Days)",
                                      value = 0)
                  )
           )
  ),#end fluid row
  
  # Fluidrow 2
  fluidRow(
    column(width = 1.5, offset = 6, 
           selectInput('level', 'Level of Interest', choices = c("National", "District", "TA"))),
  ),
  fluidRow(
    column(width = 1.5, offset = 7, submitButton("Run Report", width = '400px'))
  ),
##-----------------------------------------------------------------------------------------------##  
  tags$hr(),
  
  fluidRow(
    DT::DTOutput('table')
  )
) #--end fluid page--##

server <- function(input, output, session) {
  current_v2_stacked <- read_csv("current v2_stacked.csv")
  data <- current_v2_stacked
  data_total <- data %>% 
    dplyr::select(time, Total_Exposed, Total_Infected,
                  Total_Hospitalizations, Total_Critical,
                  Total_Recovered, Total_Dead)
  
  output$fig <- renderPlotly({
    fig <-  plot_ly(data_total, 
                    x = ~ time, 
                    y = ~ Total_Exposed, 
                    type = 'bar', 
                    name = 'Exposed')
    
    fig <- fig %>% add_trace(y = ~ Total_Infected, name = 'Infected')
    fig <- fig %>% add_trace(y = ~ Total_Hospitalizations, name = 'Hospitalizations')
    fig <- fig %>% add_trace(y = ~ Total_Critical, name = 'Critical care')
    fig <- fig %>% add_trace(y = ~ Total_Recovered, name = 'Recovered')
    fig <- fig %>% add_trace(y = ~ Total_Dead, name = 'Dead')
    fig <- fig %>% layout(xaxis = list(title = "Time (days)"),
                          yaxis = list(title = ''),
                          barmode = 'stack')
    
    fig
    
  })
  
  output$table <- DT::renderDT(
    DT::datatable(
      {data},
      extensions = 'Buttons',
      
      options = list(
        paging = TRUE,
        searching = TRUE,
        fixedColumns = TRUE,
        autoWidth = TRUE,
        ordering = TRUE,
        #dom = 'tB',
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel')
      ),
      
      class = "display"
    ))
}

shinyApp(ui, server)



