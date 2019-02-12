#
# Esta app es  para entender los test de hip?teis
# Caso particular de la comparacion de dos medias
# 
#
#    
#

library(shiny)
source("sim_difmedias.r")
# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Test de comparación de dos medias "),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(width=3,
        h3("Población 1"),
         numericInput("mu1",
                     "Media",
                     value = 30),
         numericInput("n1",
                      "Tamaño de la muestra",
                     min=2,max=100,
                      value = 4),
         h3("Población 2"),
         numericInput("mu2",
                      "Media",
                      value = 30),
         numericInput("n2",
                     "Tamaño de la muestra",
                     min=2,max=100,
                     value = 4),
         numericInput("sig",
                      "Desviación típica",
                      value = 10),
      
         numericInput("nsim",
                   "Simulaciones",
                   min=1,max=10000,
                   value = 5000),
        actionButton("Run","Ejecutar")
     ),
   
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("distPlot"),
         textOutput("delta"),
         textOutput("Pot")
         
         
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    result<-eventReactive(input$Run,{
      sal<-NULL
      # genera las diferencia de medias simuladas
      tt<-replicate(input$nsim,dif.medias(input$mu1,
          input$mu2,input$sig,input$n1,input$n2))
      sal$stT<-tt[1,]
      xmin<-min(-5,min(tt[1,]))
      xmax<-max(5,max(tt[1,]))
      sal$xmin<-xmin
      sal$xmax<-xmax
      delta<-(input$mu1-input$mu2)/
        (input$sig*sqrt(1/input$n1+1/input$n2))
      pot<-sum(tt[1,]<qt(0.025,df=(input$n1+input$n2-2))|
            tt[1,]>qt(0.975,df=(input$n1+input$n2-2)))/input$nsim   
      sal$pot<-pot
      sal$delta<-delta
      return(sal)
      })
   output$distPlot <- renderPlot({
      
      
      # draw the histogram 
      hist(result()$stT,main="",freq=FALSE,breaks=30,
           xlab=expression(bar(Y)[1.]-bar(Y)[2.]),
           axes=FALSE,ylab="",xlim=c(result()$xmin,
           result()$xmax),col="skyblue1",
           ylim=c(0,dt(0,df=(input$n1+input$n2-2))))
      axis(1)
      
      #if (input$mu1==input$mu2) {
        curve(dt(x,df=(input$n1+input$n2-2)),add=TRUE,
            col="red",lwd=2)
        abline(v=0,lty=2,col="red",lwd=2)
      #}
      abline(v=qt(0.975,df=(input$n1+input$n2-2)),lty=2
             ,col="red",lwd=2)
      abline(v=qt(0.025,df=(input$n1+input$n2-2)),lty=2
             ,col="red",lwd=2)
      
      if (input$mu1!=input$mu2){
        ncp<-round(result()$delta,4)
        curve(dt(x,df=(input$n1+input$n2-2),ncp=ncp),
        add=TRUE,col="blue",lwd=2)
      }  
   })

   output$delta<-renderText({
     paste("Parámetro no centralidad, delta= ",round(result()$delta,2))})

   output$Pot<-renderText({
      paste("Proporción H0 rechazadas=",round(result()$pot,3))})
}

# Run the application 
shinyApp(ui = ui, server = server)

