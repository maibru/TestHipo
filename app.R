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
ui <- fluidPage(theme=shinythemes::shinytheme("cerulean"),"Comparación de dos medias",

   
   # Application title
 #tabPanel("Test de de hipótesis",
   
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
         h3(""),
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
         #htmlOutput("et1"),
         h4("Test de Hipótesis"),
         textOutput("delta"),
         textOutput("Pot"),
         plotOutput("distPlot"),
         h4("intervalo de confianza"),
         textOutput("Cover"),
         plotOutput("Interval")
                  
         
      )
   )
# )
# tabPanel("Intervalo de Confianza", plotOutput("Interval")
# )        
          
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
      cov<-sum(apply(tt[2:3,],2,function(ic,val){ic[1]<=val & ic[2] >=val},val=(input$mu1-input$mu2)))/input$nsim
      sal$pot<-pot
      sal$cov<-cov
      sal$delta<-delta
      sal$li<-tt[2,]
      sal$ls<-tt[3,]
      return(sal)
      })
   output$distPlot <- renderPlot({
      
      
      # draw the histogram 
      hist(result()$stT,main="",freq=FALSE,breaks=30,
           xlab="T",
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
   output$et1<-renderText("Test de Hipótesis")
   output$delta<-renderText({
     paste("Parámetro no centralidad, delta= ",round(result()$delta,2))})

   output$Pot<-renderText({
      paste("Proporción H0 rechazadas=",round(result()$pot,3))})

   output$Interval<-renderPlot({
     pos<-1:50
     x<-plot(rnorm(100))
     mli<-min(result()$li[pos])
     mls<-max(result()$ls[pos])
     plot(x=c(result()$li[1],result()$ls[1]),y=c(1,1),axes=FALSE,xlab="límites",ylab="simulación",type="l",ylim=c(1,length(pos)+1),
          xlim=c(mli,mls))
     axis(1)
     axis(2)
     segments(x0=result()$li[pos],y0=pos,x1=result()$ls[pos],y1=pos,lwd=2)
     abline(v=(input$mu1-input$mu2),col="red",lwd=2)
     
 })
   
  output$Cover<-renderText({
    paste("Proporción de IC que contienen ",(input$mu1-input$mu2), "= ",round(result()$cov,3))
  }) 

}   
# Run the application 
shinyApp(ui = ui, server = server)

