#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)

load("/home/laurie/Desktop/flr/tutorials/advice_framework/data/covjac.RData")
load("/home/laurie/Desktop/flr/tutorials/advice_framework/data/eql.RData")
load("/home/laurie/Desktop/flr/tutorials/advice_framework/data/crvRef.RData")

rfpt$m  =unique(scen$m)[rfpt$m]
rfpt$sel=unique(scen$sel)[rfpt$sel]
rfpt$s  =unique(scen$s)[rfpt$s]

plotPDF<-function(linf=1,k=1,lmat=1,m1=1,
                  mScn=unique(scen$m),selScn=unique(scen$sel),sScn=unique(scen$s),debug="",rf=rfpt){
  ssbPDF<-function(linf=1,k=1,lmat=1,m1=1,cv=cov,jc=jac,rf=rfpt){
    
    set.seed(1234)
    diag(cv)=diag(cv)*c(linf,k,lmat,m1)
    sigma=(t(jc["ssb",])%*%cv%*%jc["ssb",])^0.5
    ddply(rf,.(m,sel,s), function(x,sigma)
      data.frame(value=rnorm(100,mean=x$ssb,sd=sigma)),sigma=sigma)}
  
  wt =dim(rf)[1] 
  rf =subset(rf,s%in%sScn&m%in%mScn&sel%in%selScn)
  save(rfpt,selScn,file="/home/laurie/Desktop/tmp/t.RData")
  
  wt =wt/dim(rf)[1]
  dat=ssbPDF(linf,k,lmat,m1,rf=rf)
  
  ggplot(dat,aes(x=value,weight=wt,group=paste(m,sel,s),fill=paste(m,sel,s)))+
    geom_density(col="grey",alpha=.5,position="stack")+
    xlab(debug)+
    theme_bw()+
    theme(legend.position="none")+
    scale_x_continuous(limits=c(0,200))}

#plotPDF(1,1,1,1,mScn=2,selScn=1:2)


# Define UI for application that draws a histogram
ui <- shinyUI(fluidPage(
   
   # Application title
   titlePanel("Uncertainty in BMSY"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        selectInput('mScn',   'M',           choices=unique(scen[,"m"]),   selected=unique(scen[,"m"]),   multiple=TRUE),
        selectInput('selScn', 'Selectivity', choices=unique(scen[,"sel"]), selected=unique(scen[,"sel"]), multiple=TRUE),
        selectInput('sScn',   'Steepness',   choices=unique(scen[,"s"]),   selected=unique(scen[,"s"]),   multiple=TRUE),
        sliderInput("linf", "Linf CV:", min=0.1, max=1, value= 1),
        sliderInput("k",    "k CV:",    min=0.1, max=1, value= 1),
        sliderInput("lmat", "lmat CV:", min=0.1, max=1, value= 1),
        sliderInput("m",    "m CV:",    min=0.1, max=1, value= 1)
      ),
      
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("distPlot")
      )
   )
))


# Define server logic required to draw a histogram
server <- shinyServer(function(input, output) {
   
   output$distPlot <- renderPlot({
     plotPDF(linf  =input$linf,
             k     =input$k,
             lmat  =input$lmat,
             m1    =input$m,
             mScn  =input$mScn,
             selScn=input$selScn,
             sScn  =input$sScn)
   })
})

# Run the application 
shinyApp(ui = ui, server = server)

