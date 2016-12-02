



shinyServer(function(input, output) {

  load("D:/Repository/DGMARETuna/tutorials/advice_framework/data/alk.RData")

  L <- function(Linf,K,t,t0){
     return(Linf * (1-exp(-K*(t-t0))))}
  A <- function(Linf,K,Le,t0){
     return(log(-pmin(Le/Linf,0.99999) +1)/-K + t0)}

  output$ALKplot <- renderPlot({
  par(mfrow=c(1,3))
  for(iYr in c("1978","1989","2004")){
    tmp <- subset(alk,Cohort == iYr)
    tmp$pred <- L(input$Linf,input$K,tmp$Age,input$t0)
    plot(y=tmp$Length/10,x=tmp$Year-as.numeric(iYr),pch=19,cex=0.2,xlim=c(0,10),ylim=c(0,40),xlab="Age",ylab="Length")

    lines(y=L(input$Linf,input$K,0:10,input$t0),x=(0:10),col=2)
    text(5,0,labels=round(sum(sqrt((log(tmp$pred)-log((tmp$Length)/10))^2),na.rm=T),1))
  }
  })
  
  output$AGEplot <- renderPlot({
  par(mfrow=c(1,3))
  for(iYr in c("1978","1989","2004")){
    tmp <- subset(alk,Cohort == iYr)
    tmp$pred <- round(A(input$Linf,input$K,tmp$Length/10,input$t0))
    res <- merge(as.data.frame(table(tmp$Age)),as.data.frame(table(tmp$pred)),by="Var1",all=T)
    a <- barplot(sweep(t(res[,2:3]),1,rowSums(t(res[,2:3]),na.rm=T),"/"),beside=T,xlim=c(0,30),ylim=c(0,0.5),las=1,xlab="Age",ylab="Proportion")
    axis(1,at=colMeans(a),labels=res$Var1)
  }
  })
})
