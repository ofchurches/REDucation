library(shiny)
library(readr)
library(tidyverse)

X8_sa_enrolment_by_school <- read_csv("https://data.sa.gov.au/data/dataset/d385385a-6d79-4ef7-ab0f-0cfc583469f6/resource/7223f681-d340-4fc9-b92b-403d158f521f/download/8-sa-enrolment-by-school-.csv")

ui <- fluidPage(
  
  titlePanel("Biggest schools by enrollment"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      radioButtons("selected_type",
                   "Type of school",
                   unique(X8_sa_enrolment_by_school$School_Type)
      )
    ),
    
    mainPanel(
      plotOutput("biggestPlot")
    )
  )
)

server <- function(input, output) {
  
  output$biggestPlot <- renderPlot({
    
    X8_sa_enrolment_by_school %>%
      filter(School_Type == input$selected_type) %>%
      slice_max(order_by = Year_2021, n = 12) %>%
      ggplot(aes(x = Year_2021, 
                 y = reorder(School_Name, Year_2021), 
                 fill = School_Type)) + 
      geom_col(show.legend = FALSE) + 
      labs(title = "Biggest schools 2021", x = "Enrollments 2021", y = NULL)
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)