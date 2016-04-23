library(shiny)

# Define server logic for random distribution application
shinyServer(function(input, output) {
  data=read.csv("stateLevelOccSal.csv")
  data2=read.csv("fullDataMSA.csv")
#
  lookup=read.csv("zipToMSA.csv")
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
    
    State=as.character(input$state)
    Occ=as.character(input$occ)
    ofInterest=subset(data,STATE==State)
    ofInterest=subset(ofInterest,OCC_TITLE==Occ)
    x=as.numeric(as.character(ofInterest[1,19:23]))
    
    #19:23 annual
    #14:17 hourly
    
    #x<- c(9.07, 11.27, 17.40, 28.32, 44.29)
    cum.p <- c(.1, .25, .5, .75, .9)
    prob <- c( cum.p[1], diff(cum.p), .1)
    
    
    freq <- 10000 
    # range of values beyond x to sample from
    init <- -(abs(min(x)) + 5)
    fin  <- 2*abs(max(x)) + 5
    
    ival <- c(init, x, fin) # generate the sequence to take pairs from
    len <- 100 # sequence of each pair
    s <- sapply(2:length(ival), function(i) {
      seq(ival[i-1], ival[i], length.out=len)
    })
    # sample from s, total of 10000 values with probabilities calculated above
    out <- sample(s, freq, prob=rep(prob, each=len), replace = T)
    hist(out,xlab="Annual $",main="")
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
  
  output$table2 <- renderTable({
    #data.frame(x=data())
    Occ=as.character(input$occ)
  MSA=lookup$msa[which(lookup$zip==input$zip)]
  #print(MSA)
  ofInterest=subset(data2,area==MSA)
  ofInterest=subset(ofInterest,occ.title==Occ)
    #State=as.character(input$state)
    # Occ=as.character(input$occ)
    # #ofInterest=subset(data,STATE==State)
    # ofInterest=subset(data2,OCC_TITLE==Occ)
  #toDisplay=rbind(cbind("Average Hourly Mean","Average Annual Mean"),
  #cbind(mean(ofInterest$H_MEAN,na.rm=T),mean(ofInterest$A_MEAN,na.rm=T)))
  toDisplay=ofInterest[1,c("h_mean","a_mean")]  
  #toDisplay=cbind(mean(as.numeric(as.character(ofInterest$h_mean)),na.rm=T),
   #                 mean(as.numeric(as.character(ofInterest$a_mean)),na.rm=T))
    
    names(toDisplay)=c("Hourly Mean","Annual Mean")
    rownames(toDisplay)=NULL
    data.frame(toDisplay)
    
  })
  
})