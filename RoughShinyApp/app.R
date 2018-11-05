library(shiny)
library(tidyverse)
library(dplyr)

data <- read_csv("road-accidents-2017.csv")

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Road Accidents in UK for 2017-18"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         sliderInput("accident_severity",
                     "Severity of accident:",
                     min = 1,
                     max = 3,
                     value = 1), 
         
         selectInput("road_type",
                     "Road Type:",
                    choices = c("1", "2", "3", "6", "7", "9"))
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("distPlot")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$distPlot <- renderPlot({
       x <- data %>%
         filter(Accident_Severity == input$accident_severity & Road_Type == input$road_type) %>%
         group_by(Day_of_Week) %>% 
         count()
       
       ggplot(data = x, aes(x = Day_of_Week, y = n)) +
         geom_line() + 
         labs(x = "Day of Week",
              y = "Count",
              title = "Number of accidents on a given day by Severity")
   })
}

# Run the application 
shinyApp(ui = ui, server = server)