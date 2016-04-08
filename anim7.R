#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


attach(cars)
library(shiny)
library("splines")
years <- c(1973,1975,1977,1979,1981,1983,1985,1986,
           1987,1988,1989,1990,1991,1992,1993,1994,1995,1996,
           1997,1998,1999,2000,2001,2002,2003,2004,2005,2006)
salary <- c(-3.0,-8.0,-1.7,-10.2,-4.0,3.7,5.2,
            4.8,0.5,1.4,1.5,-0.7,0.4,-0.4,0.3,0.7,0.4,-0.3,1.6,
            2.0,1.0,0.1,2.2,0.6,0.2,-0.5,-0.3,1.3)
setwd("/Users/sjv/Dropbox/LectureShip/lectures/Bs1b/Bs1b/nonparamreg/apps")
li=c()
for (i in 1:7){
  fileName=paste("ccode",i, ".R",sep="")
  scode=readChar(fileName, file.info(fileName)$size)
  li=append(li,scode) 
}

fitloc0 = function(x0,x,y,lam,deg){
  w=dnorm((x-x0)/lam)
  db=data.frame(x=x,y=y)
  reg=lm(y~poly(x,deg),data=db,weights=w)
  return(predict(reg,newdata=data.frame(x=x0)))}


cloo = function (lam){
  erisk6=0.0
  for(i in 1:length(years)){
    rsalary=salary[-i]
    ryears=years[-i]
    
    erisk6=erisk6+(salary[i]- fitloc0(years[i],ryears,rsalary,lam,1))^2
  }
  erisk6/length(years)
}
lams=seq(1,8,length.out = 50)
cverisk=Vectorize(cloo)(lams)
optband=lams[which.min(cverisk)]

# Define UI for application that draws a histogram
ui <- shinyUI(fluidPage(
   
   # Application title
   titlePanel("Professor Salary Data Set"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         sliderInput("lam",
                     "Bandwith:",
                     min = 1,
                     max = 8,
                     value = optband, step=0.001),
         selectInput("select", label = h3("Method"), 
                     choices = list("OLS" = 1, "Piecewise Linear (knot at x-median)" = 2, "Piecewise Linear (knot at 1986)" = 3,
                        "Quadratic"=4, "Piecewise Quadratic (knot at x-median)"=5,"Smoothing spline with CV-LOO"=6,
                  "Local linear fit" =7), 
                     selected = 1)),

      
      # Show a plot of the generated distribution
      mainPanel(
        plotOutput("distPlot"), verbatimTextOutput("code"))
   )
))

# Define server logic required to draw a histogram
server <- shinyServer(function(input, output) {
   
   output$distPlot <- renderPlot({
     if(input$select==1){
       salary1 <- lm(salary ~ bs(years,degree=1))
       plot(years,salary,xlab="Years",ylab="Salary change",pch = 19,cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5,type="p",lwd=5)
       abline(h=0,lty=2)
       lines(years, fitted(salary1),lwd=5)
       
       erisk1=mean((salary1$residuals/(1-influence(salary1)$hat))^2)
       title(paste("CV Leave one out MSE ",erisk1))
       
     }
     if(input$select==2){
       plot(years,salary,xlab="Years",ylab="Salary change",pch = 19)
       abline(h=0,lty=2)
       salary2 <- lm(salary ~ bs(years,degree=1,df=2))
       lines(years,fitted(salary2))
       
       erisk2=0.0
       for(i in 1:length(years)){
         rsalary=salary[-i]
         ryears=years[-i]
         salary2t <- lm(rsalary ~ bs(ryears,degree=1,df=2))
         ps=predict(salary2t,data.frame(ryears=c(years[i])))
         erisk2=erisk2+(salary[i]- ps)^2
       }
       erisk2=erisk2/length(years)   
       title(paste("CV Leave one out MSE",erisk2))
       
       
     }
     if(input$select==3){
       salary3 <- lm(salary ~ bs(years,degree=1,knots=1986))
       plot(years,salary,xlab="Years",ylab="Salary change",pch = 19,cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5,type="p",lwd=5)
       abline(h=0,lty=2)
       lines(years,fitted(salary3),lwd=5)
      
       
       erisk3=0.0
       for(i in 1:length(years)){
         rsalary=salary[-i]
         ryears=years[-i]
         salary3t <- lm(rsalary ~ bs(ryears,degree=1,knots=1986))
         ps=predict(salary3t,data.frame(ryears=c(years[i])))
         erisk3=erisk3+(salary[i]- ps)^2
       }
       erisk3=erisk3/length(years)
       # removemean((salary3$residuals/(1-influence(salary3)$hat))^2)
       
       title(paste("CV Leave one out MSE",erisk3))
       
     }
     if(input$select==4){
       salary4 <- lm(salary ~ bs(years,degree=2,df=2))
       plot(years,salary,xlab="Years",ylab="Salary change",pch = 19,cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5,type="p",lwd=5)
       abline(h=0,lty=2)
       lines(years,fitted(salary4),cwd=5)
       
       erisk4=0.0
       for(i in 1:length(years)){
         rsalary=salary[-i]
         ryears=years[-i]
         salary4t <- lm(rsalary ~ bs(ryears,degree=2,df=2))
         ps=predict(salary4t,data.frame(ryears=c(years[i])))
         erisk4=erisk4+(salary[i]- ps)^2
       }
       erisk4=erisk4/length(years)
       title(paste("CV Leave one out MSE",erisk4))
     }
     if(input$select==5){
       salary5 <- lm(salary ~ bs(years,degree=2,df=3))
       plot(years,salary,xlab="Years",ylab="Salary change",pch = 19,cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5,type="p",lwd=5)
       abline(h=0,lty=2)
       lines(years,fitted(salary5),cwd=5)
       
       erisk5=0.0
       for(i in 1:length(years)){
         rsalary=salary[-i]
         ryears=years[-i]
         salary5t <- lm(rsalary ~ bs(ryears,degree=2,df=3))
         ps=predict(salary5t,data.frame(ryears=c(years[i])))
         erisk5=erisk5+(salary[i]- ps)^2
       }
       erisk5=erisk5/length(years)
       title(paste("CV Leave one out MSE",erisk5))
     }
     if(input$select==6){
       plot(years,salary,xlab="Year",ylab="Salary change in real terms",pch=20,cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5,type="p",lwd=5)
       sp.spline=smooth.spline(x = years, y = salary, cv = TRUE)
       
       erisk6=8.039253
       sp.spline$lambda
       sp.spline$data
       ts=seq(1975,2006,length.out = 50)
       sppredict=predict(sp.spline,data.frame(x=c(ts)))
       lines(unlist(sppredict$x),unlist(sppredict$y),cwd=5)
       title(paste("CV Leave one out MSE",erisk6))
     }
     if(input$select==7){
       
       par(mfrow=c(2,1))
       
       plot(lams,cverisk, xlab=expression(paste("Bandwith ",lambda)),ylab="CV LOO MSE")
       abline(v=input$lam,col="red",lwd=5)
       
       ts=seq(1973,2006,by=0.1)
       
       plot(years,salary,xlab="Year",ylab="Salary change in real terms",pch=20,cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5,type="p",lwd=5)
       lines(ts,Vectorize(function(x) fitloc0(x,years,salary,input$lam,1))(ts),lwd=5)
       title(paste("CV Leave one out MSE",cloo(input$lam)))
     }
     fileName=paste("ccode",input$select, ".R",sep="")
     scode=readChar(fileName, file.info(fileName)$size)
     output$code=renderText(scode)
     
   })
})

# Run the application 
shinyApp(ui = ui, server = server)

