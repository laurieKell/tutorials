shinyUI(pageWithSidebar(

  # Application title
  headerPanel("MSY"),

  # Sidebar with controls to select the variable to plot against mpg
  # and to specify whether outliers should be included
  sidebarPanel(
#    selectInput("SR", "Stock-Recruit curve",
#                choices=c("Ricker",
#                          "BevertonHolt")),
    sliderInput("h",
                  "Steepness:",
                  min = 0.5,
                  max = 1,
                  value = 0.8),

    sliderInput("M",
                  "Natural mortality:",
                  min = 0.1,
                  max = 1,
                  value = 0.3),

    sliderInput("Fprop",
                  "Fishery before spawning",
                  min = 0,
                  max = 1,
                  value = 0.5),

    sliderInput("Mat",
                  "Maturity:",
                  min = 0,
                  max = 1,
                  value = 0.5),

    sliderInput("W",
                  "Fish weight:",
                  min = 0.1,
                  max = 1,
                  value = 0.3)
    ),

  mainPanel(
    plotOutput("MSYplot")
  )
))
