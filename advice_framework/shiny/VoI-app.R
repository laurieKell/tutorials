library(shiny)
library(plyr)
library(ggplot2)
library(diags)
library(grid)

load("/home/laurie/Desktop/flr/tutorials/advice_framework/data/covjac.RData")
load("/home/laurie/Desktop/flr/tutorials/advice_framework/data/eql.RData")
load("/home/laurie/Desktop/flr/tutorials/advice_framework/data/crvRef.RData")

rfpt$m  =unique(scen$m)[rfpt$m]
rfpt$sel=unique(scen$sel)[rfpt$sel]
rfpt$s  =unique(scen$s)[rfpt$s]

crv$m  =unique(scen$m)[crv$m]
crv$sel=unique(scen$sel)[crv$sel]
crv$s  =unique(scen$s)[crv$s]

plotPDF<-function(linf=1,k=1,lmat=1,m1=1,
                  mScn=unique(scen$m),selScn=unique(scen$sel),sScn=unique(scen$s),debug=expression(B[MSY]),
                  rf=rfpt,cr=crv){
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
  
  p1=ggplot(dat,aes(x=value,weight=wt,group=paste(m,sel,s),fill=paste(m,sel,s)))+
    geom_density(col="grey",alpha=.5,position="stack")+
    xlab(debug)+ylab("Relative Density")+
    theme_bw()+
    theme(legend.position="none")+
    scale_x_continuous(limits=c(0,200))
  
  m.=1;sel.=1;s.=1
  
  p2=ggplot(crv,aes(ssb,catch,group=paste(m,sel,s)))+
    theme_bw()+
    geom_line(col="grey90")+
    geom_point(aes(ssb,catch),data=rfpt,size=2.5,col="grey75",fill="grey90",shape=21)+
    geom_line(aes(ssb,catch),
              data=subset(crv,s%in%sScn&m%in%mScn&sel%in%selScn))+
    geom_point(aes(ssb,catch),size=2.5,shape=21,fill="black",col="grey90",
               data=subset(rf,s%in%sScn&m%in%mScn&sel%in%selScn))+
    xlab("SSB")+ylab("Yield")
  
  diags:::multiplot(p2,p1)}

#plotPDF(1,1,1,1,mScn=2,selScn=1:2)


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


server <- shinyServer(function(input, output) {
   
   output$distPlot <- renderPlot({
     plotPDF(linf  =input$linf,
             k     =input$k,
             lmat  =input$lmat,
             m1    =input$m,
             mScn  =input$mScn,
             selScn=input$selScn,
             sScn  =input$sScn)
   },height=650)
})

# Run the application 
shinyApp(ui = ui, server = server)

