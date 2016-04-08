#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(shiny)
setwd("/Users/sjv/Dropbox/LectureShip/lectures/Bs1b/Bs1b/nonparamreg/apps/robust")


scode=""#readChar("l1source.R", file.info(fileName)$size)
set.seed(1)

library(quantreg)
data(AirPassengers)

x <- as.numeric(time(AirPassengers))
y <- as.numeric(AirPassengers)


sel=c(TRUE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE)
x = x[sel]
y = log(y[sel])

yt=y[1]



# Define UI for application that draws a histogram
ui <- shinyUI(fluidPage(
   
   # Application title
   titlePanel("L1 regression"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         sliderInput("y1",
                     "y[1]:",
                     min = 0,
                     max = 10,
                     value = yt, step=0.1)
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
        plotOutput("distPlot"), verbatimTextOutput("code"))
   )
))

# Define server logic required to draw a histogram
server <- shinyServer(function(input, output) {
   
   output$distPlot <- renderPlot({
     
     y[1]=input$y1
     medianreg <- rq( y ~ x, tau=0.5 )# L1 regression
     fit <- lm(y ~x)
     plot(x,y)
     lines(x, fitted(medianreg))
     abline(fit,col="red")
     output$code=renderText(scode)
     
   })
})

# Run the application 
shinyApp(ui = ui, server = server)

