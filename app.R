#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(lazyeval)
library(plyr)
library(dplyr)
library(plotly)
library(DT)
library(countrycode)
load('fifa.RData')

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel(""),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         selectInput(inputId = "stat", label="Select variable",
                     choices = names(players[,19:53]))
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotlyOutput("plot"),
         dataTableOutput("table")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  col <- reactive({input$stat})
  
  filterPlayers <- function() {
    players <- players %>% group_by(nationality) %>% summarize_(avg = interp(~mean(x), x = as.name(col())))
    players$code <- countrycode(players$nationality, origin = "country.name", destination = "cowc")
    players
  }
  
  #players <- players %>% summarise_(paste0("avg = mean(",input$stat,")"))
  
  
  output$plot <- renderPlotly({
    
    #players <- players %>% group_by(nationality) %>% summarize_(avg = interp(~mean(x), x = as.name(col())))
    #players$code <- countrycode(players$nationality, origin = "country.name", destination = "cowc")
    
    players <- filterPlayers()
    l <- list(color = toRGB("grey"), width = 0.5)
    
    # specify map projection/options
    g <- list(
      showframe = FALSE,
      showcoastlines = FALSE,
      projection = list(type = 'Mercator')
    )
    
    p <- plot_geo(players) %>%
      add_trace(
        z = ~avg, color = ~avg, colors = 'Blues',
        text = ~nationality, locations = ~code, marker = list(line = l)
      ) %>%
      layout(
        geo = g
      )
    
  })
  
  output$table <- renderDataTable({
    players <- filterPlayers()
    datatable(players)
  })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)

