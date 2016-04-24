library(shiny)
require(gridExtra)
require(ggplot2)


distribution_generator<- function(x){  
  cum.p <- c(.1, .25, .5, .75, .9)
  prob <- c( cum.p[1], diff(cum.p), .1)
  freq <- 10000 
  len<-100
  # range of values beyond x to sample from
  range <- c(0, x, 2*max(x))
  s <- sapply(2:length(range), function(i) {
    seq(range[i-1], range[i], length.out=len)
  })
  # sample from s, total of freq values, with probabilities from prob 
  out <- sample(s, freq, prob=rep(prob, each=len), replace = T)
  return(out)  
}


# Define server logic for random distribution application
shinyServer(function(input, output) {
  #data=read.csv("stateLevelOccSal.csv")
  
  data=read.csv("stateLevelOccSal.csv")
  data$OCC_CODE_PREFIX=as.numeric(sapply(data$OCC_CODE, substring, 0, 2))
  
  membership=read.csv("union-occupation-membership.csv")
  salary=read.csv("union-occupation-wk-salary.csv")
  
  #membership$OCC_CODE = seq.int(nrow(membership))
  #salary$OCC_CODE = seq.int(nrow(salary))
  #write.csv(membership, "union-occupation-membership.csv")
  #write.csv(salary, "union-occupation-wk-salary.csv")
  
  union_occ_lookup=read.csv("union-occ-lookup.csv")
  
  data_with_union = data %>%
    left_join(union_occ_lookup, by=c("OCC_CODE_PREFIX" = "REG_OCC_CODE_PREFIX")) %>%
    left_join(salary, by=c("UNION_OCC_CODE" = "OCC_CODE"))
  
  data=data_with_union
  
  
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
    #print(nrow(ofInterest))
    #x=as.numeric(as.character(ofInterest[1,19:23]))
    inter=apply(ofInterest[1,19:23],2,function(x){as.character(x)})
   x= as.numeric(gsub(",","",inter))
   #x=x[-which(is.na(x))]
   inter=apply(ofInterest[1,14:17],2,function(x){as.character(x)})
   y= as.numeric(gsub(",","",inter))
   #y=y[-which(is.na(y))]
   # if(length(x)<5){
   #   
   # }else{
   # dist_x<- distribution_generator(x)
   # }
   # 
   # if(length(y)<5){
   #   
   # }else{
   # dist_y<-distribution_generator(y)
   # }

    #y=as.numeric(as.character(ofInterest[1,14:17]))
   # print(x)
  #  print(y)
   #plot(histgenA(x))
    #19:23 annual
    #14:17 hourly
    par(mfrow=c(1,2)) 
   
   
    # 
    # noX=F
    # noY=F
    # if(length(x)<5){
    #   noX=T
    # }else{
    #   df<- as.data.frame(dist_x)
    #   plot1=(ggplot(df, aes(x = dist_x)) + 
    #     geom_histogram(xlim = c(0, 3*max(dist_x)/4), binwidth = 0.75)+
    #     labs(title = "Distribution of Annual Salary", x = "Annual Salary", y= "Frequency")+
    #     geom_vline(data=df, aes(xintercept=as.numeric(as.character(input$salary)))))
    # }
    # if(length(y)<5){
    #   noY=T
    # }else{
    #     df<- as.data.frame(dist_y)
    #   plot2=(ggplot(df, aes(x = dist_y)) + 
    #     geom_histogram(xlim = c(0, 3*max(dist_y)/4), binwidth = 0.75)+
    #     labs(title = "Distribution of Hourly Wages", x = "Hourly Wage", y= "Frequency")+
    #     geom_vline(data=df, aes(xintercept=as.numeric(as.character(input$hour)))))
    #     
    # }
    # 
    # print(noX)
    # print(noY)
    # if(noX & noY){
    # }else if(noX){
    #   grid.arrange(plot2,ncol=1)
    # }else if(noY){
    #   grid.arrange(plot1,ncol=1)
    # }else{
    #   grid.arrange(plot1,plot2,ncol=2)
    # }
    #geom_vline
    #x<- c(9.07, 11.27, 17.40, 28.32, 44.29)
    cum.p <- c(.1, .25, .5, .75, .9)
    prob <- c( cum.p[1], diff(cum.p), .1)


    freq <- 10000
    # range of values beyond x to sample from
    init <- -(abs(min(x)) + 5)
    #print("here")
    fin  <- 2*abs(max(x)) + 5
    if(length(x)<5){

    }else{
    ival <- c(init, x, fin) # generate the sequence to take pairs from
    len <- 100 # sequence of each pair
    s <- sapply(2:length(ival), function(i) {
      seq(ival[i-1], ival[i], length.out=len)
    })
    # sample from s, total of 10000 values with probabilities calculated above
    out <- sample(s, freq, prob=rep(prob, each=len), replace = T)
    hist(out,xlab="Annual $",main="")
    abline(v=as.numeric(as.character(input$salary)),col="red")
    }
    init <- -(abs(min(y)) + 5)
    fin  <- 2*abs(max(y)) + 5
    if(length(y)<5){

    }else{
    ival <- c(init, y, fin) # generate the sequence to take pairs from
    len <- 100 # sequence of each pair
    s <- sapply(2:length(ival), function(i) {
      seq(ival[i-1], ival[i], length.out=len)
    })
    # sample from s, total of 10000 values with probabilities calculated above
    out <- sample(s, freq, prob=rep(prob, each=len), replace = T)
    hist(out,xlab="Hourly $",main="")
    abline(v=as.numeric(as.character(input$hour)),col="red")
    }
  })
  
  output$plot2 <- renderPlot({
    
    State=as.character(input$state)
    Occ=as.character(input$occ)
    ofInterest=subset(data,STATE==State)
    ofInterest=subset(ofInterest,OCC_TITLE==Occ)
    #x=as.numeric(as.character(ofInterest[1,19:23]))
    inter=apply(ofInterest[1,19:23],2,function(x){as.character(x)})
    x= as.numeric(gsub(",","",inter))
   #x=x[-which(is.na(x))]
    inter=apply(ofInterest[1,14:17],2,function(x){as.character(x)})
    y= as.numeric(gsub(",","",inter))
    #y=y[-which(is.na(y))]
    
    cum.p <- c(.1, .25, .5, .75, .9)
    prob <- c( cum.p[1], diff(cum.p), .1)
    
    
    freq <- 10000 
    # range of values beyond x to sample from
    init <- -(abs(min(x)) + 5)
    fin  <- 2*abs(max(x)) + 5
    
    firstX=T
    firstY=T
    if(length(x)<5){
      firstX=F
    }else{
      ival <- c(init, x, fin) # generate the sequence to take pairs from
      len <- 100 # sequence of each pair
      s <- sapply(2:length(ival), function(i) {
        seq(ival[i-1], ival[i], length.out=len)
      })
      # sample from s, total of 10000 values with probabilities calculated above
      out1 <- sample(s, freq, prob=rep(prob, each=len), replace = T)
      #hist(out,xlab="Annual $",main="")
      #abline(v=as.numeric(as.character(input$salary)),col="red")
    }
    init <- -(abs(min(y)) + 5)
    fin  <- 2*abs(max(y)) + 5
    
    if(length(y)<5){
      firstY=F
    }else{
      ival <- c(init, y, fin) # generate the sequence to take pairs from
      len <- 100 # sequence of each pair
      s <- sapply(2:length(ival), function(i) {
        seq(ival[i-1], ival[i], length.out=len)
      })
      # sample from s, total of 10000 values with probabilities calculated above
      out2 <- sample(s, freq, prob=rep(prob, each=len), replace = T)
      #hist(out,xlab="Hourly $",main="")
      #abline(v=as.numeric(as.character(input$hour)),col="red")
    }
    
    
    # if(length(x)<5){
    #   
    # }else{
    # dist_x<- distribution_generator(x)
    # }
    # 
    # if(length(y)<5){
    #   
    # }else{
    # dist_y<-distribution_generator(y)
    # }
    
    #y=as.numeric(as.character(ofInterest[1,14:17]))
    # print(x)
    #  print(y)
    #plot(histgenA(x))
    #19:23 annual
    #14:17 hourly
    #par(mfrow=c(1,2)) 
    
    
    # 
    # noX=F
    # noY=F
    # if(length(x)<5){
    #   noX=T
    # }else{
    #   df<- as.data.frame(dist_x)
    #   plot1=(ggplot(df, aes(x = dist_x)) + 
    #     geom_histogram(xlim = c(0, 3*max(dist_x)/4), binwidth = 0.75)+
    #     labs(title = "Distribution of Annual Salary", x = "Annual Salary", y= "Frequency")+
    #     geom_vline(data=df, aes(xintercept=as.numeric(as.character(input$salary)))))
    # }
    # if(length(y)<5){
    #   noY=T
    # }else{
    #     df<- as.data.frame(dist_y)
    #   plot2=(ggplot(df, aes(x = dist_y)) + 
    #     geom_histogram(xlim = c(0, 3*max(dist_y)/4), binwidth = 0.75)+
    #     labs(title = "Distribution of Hourly Wages", x = "Hourly Wage", y= "Frequency")+
    #     geom_vline(data=df, aes(xintercept=as.numeric(as.character(input$hour)))))
    #     
    # }
    # 
    # print(noX)
    # print(noY)
    # if(noX & noY){
    # }else if(noX){
    #   grid.arrange(plot2,ncol=1)
    # }else if(noY){
    #   grid.arrange(plot1,ncol=1)
    # }else{
    #   grid.arrange(plot1,plot2,ncol=2)
    # }
    #geom_vline
    #x<- c(9.07, 11.27, 17.40, 28.32, 44.29)
    # cum.p <- c(.1, .25, .5, .75, .9)
    # prob <- c( cum.p[1], diff(cum.p), .1)
    # 
    # 
    # freq <- 10000
    # # range of values beyond x to sample from
    # init <- -(abs(min(x)) + 5)
    # fin  <- 2*abs(max(x)) + 5
    # if(length(x)<5){
    #   
    # }else{
    #   ival <- c(init, x, fin) # generate the sequence to take pairs from
    #   len <- 100 # sequence of each pair
    #   s <- sapply(2:length(ival), function(i) {
    #     seq(ival[i-1], ival[i], length.out=len)
    #   })
    #   # sample from s, total of 10000 values with probabilities calculated above
    #   out1 <- sample(s, freq, prob=rep(prob, each=len), replace = T)
    #   #hist(out,xlab="Annual $",main="")
    #   #abline(v=as.numeric(as.character(input$salary)),col="red")
    # }
    # init <- -(abs(min(y)) + 5)
    # fin  <- 2*abs(max(y)) + 5
    # if(length(y)<5){
    #   
    # }else{
    #   ival <- c(init, y, fin) # generate the sequence to take pairs from
    #   len <- 100 # sequence of each pair
    #   s <- sapply(2:length(ival), function(i) {
    #     seq(ival[i-1], ival[i], length.out=len)
    #   })
    #   # sample from s, total of 10000 values with probabilities calculated above
    #   out2 <- sample(s, freq, prob=rep(prob, each=len), replace = T)
    #   #hist(out,xlab="Hourly $",main="")
    #   #abline(v=as.numeric(as.character(input$hour)),col="red")
    # }
    
    ####
    Occ=as.character(input$occ)
    MSA=lookup$msa[which(lookup$zip==input$zip)]
    #print(MSA)
    ofInterest=subset(data2,area==MSA)
    ofInterest=subset(ofInterest,occ.title==Occ)
    #y=as.numeric(as.character(ofInterest[1,18:22]))
    #x=as.numeric(as.character(ofInterest[1,23:27]))
    
    inter=apply(ofInterest[1,23:27],2,function(x){as.character(x)})
    x= as.numeric(gsub(",","",inter))
    #x=x[-which(is.na(x))]
    inter=apply(ofInterest[1,18:22],2,function(x){as.character(x)})
    y= as.numeric(gsub(",","",inter))
    #y=y[-which(is.na(y))]
  
    #18:22 hourly
    # 23:27 annual
    par(mfrow=c(1,2)) 
    #x<- c(9.07, 11.27, 17.40, 28.32, 44.29)
    cum.p <- c(.1, .25, .5, .75, .9)
    prob <- c( cum.p[1], diff(cum.p), .1)
    
    
    freq <- 10000 
    # range of values beyond x to sample from
    init <- -(abs(min(x)) + 5)
    fin  <- 2*abs(max(x)) + 5
    
    if(length(x)<5){
      
    }else{
    ival <- c(init, x, fin) # generate the sequence to take pairs from
    len <- 100 # sequence of each pair
    s <- sapply(2:length(ival), function(i) {
      seq(ival[i-1], ival[i], length.out=len)
    })
    # sample from s, total of 10000 values with probabilities calculated above
    out <- sample(s, freq, prob=rep(prob, each=len), replace = T)
    hist(out,xlab="Annual $",main="")
    
    if(firstX==T){hist(out1,col="blue",add=T)}
    abline(v=as.numeric(as.character(input$salary)),col="red")
    }
     init <- -(abs(min(y)) + 5)
    fin  <- 2*abs(max(y)) + 5
    
    if(length(y)<5){
      
    }else{
    ival <- c(init, y, fin) # generate the sequence to take pairs from
    len <- 100 # sequence of each pair
    s <- sapply(2:length(ival), function(i) {
      seq(ival[i-1], ival[i], length.out=len)
    })
    # sample from s, total of 10000 values with probabilities calculated above
    out <- sample(s, freq, prob=rep(prob, each=len), replace = T)
    hist(out,xlab="Hourly $",main="")
    if(firstY==T){hist(out2,col="blue",add=T)}
    abline(v=as.numeric(as.character(input$hour)),col="red")
    }
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
    toDisplay=ofInterest[1,c("H_MEAN","A_MEAN","X2015.UNION.MEMBER","X2015.UNION.REPR","X2015.NON.UNION")]
    names(toDisplay)=c("Hourly Mean", "Annual Mean","Weekly Mean Union Member","Weekly Mean Union Represented","Weekly Mean No Union")
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