


shinyServer(function(input, output) {

      calcFMSY <- function(M,Mat,W,Fprop,h){
        F = seq(0,1,0.01)
        M = input$M
        Ninit = 1000
        Mat = input$Mat
        W = input$W

        Fprop = input$Fprop
        h = input$h

        N <- Catch <- Surv <- SSB <- SSBeq <- RecR <- RecBH <- matrix(NA,nrow=100,ncol=length(F))
        N[1,] <- Ninit
        for(i in 1:99){
          Catch[i,] = F / (F+M) * N[i,] * (1-exp(-F-M))
          Surv[i,] = N[i,] * exp(-F-M)
          SSB[i,] = N[i,] * exp(-F*Fprop-M) * W * Mat
          SSBeq[i,] = Ninit * exp(-M) * W * Mat
          RecR[i,] = SSB[i,] * exp(1.25*log(5*h)*(1-SSB[i,]/SSBeq[i,]))
          RecBH[i,] = SSB[i,] / (1-((5*h-1)/4*h) * (1-SSB[i,]/SSBeq[i,]))

      #    newR <- reactive({
      #            switch(input$SR,
      #                   Ricker = RecR[i,],
      #                   BevertonHold = RecBH[i,])})

          N[i+1,] = Surv[i,] + RecR[i,]
        }
        FMSY = which.max(Catch[99,])
        MSY = Catch[99,FMSY]
      return(list(F,Catch[99,],F[FMSY],MSY))}
  output$MSYplot <- renderPlot({
    plot(  x=calcFMSY(input$M,input$Mat,input$W,input$Fprop,input$h)[[1]],
           y=calcFMSY(input$M,input$Mat,input$W,input$Fprop,input$h)[[2]],type="l",xlab="Fishing mortality",ylab="Yield")
    abline(v=calcFMSY(input$M,input$Mat,input$W,input$Fprop,input$h)[[3]],col="lightgrey",lty=2)
    abline(h=calcFMSY(input$M,input$Mat,input$W,input$Fprop,input$h)[[4]],col="lightgrey",lty=3)
  })
})