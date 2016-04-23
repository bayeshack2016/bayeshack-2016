library(shiny)

# Define server logic for random distribution application
shinyServer(function(input, output) {
  data=read.csv("stateLevelOccSal.csv")
  # Reactive expression to generate the requested distribution.
  # This is called whenever the inputs change. The output
  # functions defined below then all use the value computed from
  # this expression
  
  
  # Generate a plot of the data. Also uses the inputs to build
  # the plot label. Note that the dependencies on both the inputs
  # and the data reactive expression are both tracked, and
  # all expressions are called in the sequence implied by the
  # dependency graph
  output$plot <- renderPlot({
    dist <- input$dist
    n <- input$n
    
    hist(data(), 
         main=paste('r', dist, '(', n, ')', sep=''))
  })
  
  # Generate a summary of the data
  output$summary <- renderPrint({
    summary(data())
  })
  
  # Generate an HTML table view of the data
  output$table <- renderTable({
    #data.frame(x=data())
    
    State=as.character(input$state)
    Occ=as.character(input$occ)
    ofInterest=subset(data,STATE==State)
    ofInterest=subset(ofInterest,OCC_TITLE==Occ)
    toDisplay=ofInterest[1,c("H_MEAN","A_MEAN")]
    names(toDisplay)=c("Hourly Mean", "Annual Mean")
    rownames(toDisplay)=NULL
    data.frame(toDisplay)
    
  })
  
})