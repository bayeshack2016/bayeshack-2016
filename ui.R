library(shiny)
data=read.csv("stateLevelOccSal.csv")
# Define UI for random distribution application 
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Bayes Hack: Labor"),
  
  # Sidebar with controls to select the random distribution type
  # and number of observations to generate. Note the use of the
  # br() element to introduce extra vertical spacing
  sidebarLayout(
    sidebarPanel(
      selectInput("state", label = "Choose your state:", 
                  choices=unique(as.character(data$STATE)), selected = "California"),
      textInput("zip", label = "Choose your zipcode:", value = "94704"),
      
      br(),
      selectInput("occ", "Choose your occupation:", 
                  choices=unique(as.character(data$OCC_TITLE))),
      textInput("salary",label="What is your current annual salary?",value="50000"),
      textInput("hour",label="What is your current pay per hour?",value="10")
      
    ),
    
    # Show a tabset that includes a plot, summary, and table view
    # of the generated distribution
    mainPanel(
      tabsetPanel(type = "tabs", 
                  tabPanel("State Level", tableOutput("table"),plotOutput("plot")), 
                  tabPanel("Zip Code Level", tableOutput("table2"),plotOutput("plot2"),h3("In Blue: Salaries in Your State"),h3("In White: Salaries in Your Zipcode"))
                  #tabPanel("Summary", verbatimTextOutput("summary")), 
                  #tabPanel("Table", tableOutput("table"))
      )
    )
  )
))