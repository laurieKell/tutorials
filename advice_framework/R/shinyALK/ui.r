shinyUI(pageWithSidebar(

  # Application title
  headerPanel("Growth"),

  # Sidebar with controls to select the variable to plot against mpg
  # and to specify whether outliers should be included
  sidebarPanel(

    sliderInput("Linf",
                  "Maximum length of a fish:",
                  min = 0,
                  max = 40,
                  value = 30,
                  step=0.5),

    sliderInput("K",
                  "Intrinsic growth rate:",
                  min = 0,
                  max = 0.6,
                  value = 0.3,
                  step=0.01),

    sliderInput("t0",
                  "Age of fish at size = 0 ",
                  min = -3,
                  max = 1,
                  value = -0.1,
                  step=0.1)
   ),

  mainPanel(
    plotOutput("ALKplot"),
    plotOutput("AGEplot")
  )
))
