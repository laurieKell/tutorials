library(shiny)
library(ggplot2)
library(mpb)
library(reshape)
library(kobe)
library(plyr)
library(grid)

bd=mpb:::sim(harvest=FLQuant(seq(0,1.5,length.out=40))*0.25)

hcrFn<-function(iYr  =50,
                frq  =3,
                ftar =0.70,
                btrig=0.80,
                fmin =0.01,
                blim =0.40,
                bndFMin=0.8,
                bndFMax=1.2,
                bndTMin=0.8,
                bndTMax=1.2,
                sd=0.15,
                text=""){
  
  blim=min(blim,btrig)
  
  set.seed(1234)
  
  ##hcr
  parHcr=mpb:::hcrParam(ftar =ftar *refpts(bd)['fmsy'],
                        btrig=btrig*refpts(bd)['bmsy'],
                        fmin =fmin *refpts(bd)['fmsy'],
                        blim =blim *refpts(bd)['bmsy'])
  
  ##plotHcr
  parPlot=FLPar(ftar=ftar, btrig=btrig, fmin=fmin, blim=blim)
  
  for (i in seq(39,iYr,frq))
    bd=fwd(bd,catch=hcr(bd,yr=i,byr=i-1,hyr=i+1:frq,
                        bndF  =c(bndFMin,bndFMax),
                        tac   =TRUE,
                        bndTac=c(bndTMin,bndTMax),
                        params=parHcr))
  
  pt =model.frame(mcf(FLQuants(stock  =rlnorm(100,log(stock(  bd)%/%bmsy(bd)),sd=sd),
                               harvest=rlnorm(100,log(harvest(bd)%/%fmsy(bd)),sd=sd),
                               yield  =rlnorm(100,log(  catch(bd)%/%msy(bd,params(bd))),sd=sd))),drop=T)
  
  trk=ddply(pt,.(year),with,data.frame(stock=median(stock),harvest=median(harvest)))
  
  p1=kobePhase(xlim=c(0,2),ylim=c(0,2))+
      geom_line( aes(stock,harvest),data=plotHcr(bd,params=parPlot),col="brown",size=1.5)+
      geom_point(aes(stock,harvest), fill="cyan",shape=21, size=3,
                 data=subset(pt,year==iYr))+
      geom_path(aes(stock,harvest), data=subset(trk,year<=iYr),col="blue")+
      geom_point(aes(stock,harvest), col="black",fill="blue",shape=21,
                 data=subset(trk,year==i),shape=21,size=5)+
      ggtitle(paste("Year :",i,text)) +
      theme(legend.position="none",
            plot.background=element_rect(fill="transparent",colour=NA),
            plot.title     =element_text(lineheight=.8, face="italic"))
    
    p2=ggplot(subset(melt(pt[,c("year","stock","harvest","yield")],id="year"),year>=39))+
      geom_hline(aes(yintercept=1),col="red")+
      geom_boxplot(aes(factor(year),value))+facet_grid(variable~.)+
      theme_bw()+xlab("Year")+ylab("")

  diags:::multiplot(p1,p2)}

ui<-shinyUI(
    fluidPage(
      mainPanel(
        tabsetPanel(
          #tabPanel("Summary", dataTableOutput("dis")),
          tabPanel("Plot",
                   # fluidRow(...)
                   plotOutput("plot1"),
                   plotOutput("plot2")
          )
        )
      ),
        
        sidebarPanel(
          sliderInput("btrig", "B trigger:",   min=0.1, max=2,   value=1.0),
          sliderInput("blim",  "B Limit:",     min=0.1, max=1,   value=0.8),
          sliderInput("ftar",  "F target:",    min=0.1, max=2,   value=1.0),
          sliderInput("fmin",  "F min:",       min=0,   max=0.3, value=0.1),
          sliderInput("bndFMin",  "Bound Min:",min=0,   max=1.0, value=0.1),
          sliderInput("bndFMax",  "Bound Max:",min=1,   max=2.0, value=2.0),
          sliderInput("iYr",   "Project to:",  min=40,  max=60,  value=40)
          )
        )
    )

 server<-shinyServer(function(input, output) {
    output$plot1 <- renderPlot({
      hcrFn(iYr   =input$iYr,
                  ftar  =input$ftar,
                  fmin  =input$fmin,
                  btrig =input$btrig,
                  blim  =input$blim,
                  bndFMin=input$bndFMin,
                  bndFMax=input$bndFMax)
    },height=800)
    
    #output$dis <- renderDataTable({})
  })

# Run the application 
shinyApp(ui = ui, server = server)

