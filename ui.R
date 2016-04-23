library(shiny)

# Define UI for random distribution application 
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Bayes Hack: Labor"),
  
  # Sidebar with controls to select the random distribution type
  # and number of observations to generate. Note the use of the
  # br() element to introduce extra vertical spacing
  sidebarLayout(
    sidebarPanel(
      textInput("state", label = "Choose a state. (case sensitive)", value = "California"),
      
      br(),
      selectInput("occ", "Choose your occupation:", 
                  choices=unique(as.character(data$OCC_TITLE)))
    ),
    
    # Show a tabset that includes a plot, summary, and table view
    # of the generated distribution
    mainPanel(
      tabsetPanel(type = "tabs", 
                  tabPanel("State Level", tableOutput("table"))#, 
                  #tabPanel("Summary", verbatimTextOutput("summary")), 
                  #tabPanel("Table", tableOutput("table"))
      )
    )
  )
))