#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

shinyUI(fluidPage(

  # Application title
  titlePanel("Multiple Factor Analysis"),

  sidebarLayout(
    sidebarPanel(
      sliderInput(inputId = "ncomps",
                  label = "Number of Components to Analyze",
                  min = 1,
                  max = 4,
                  value = 2),
      checkboxInput(inputId = "center",
                    label = "Center the data before analysis?",
                    value = TRUE),
      checkboxInput(inputId = "scale",
                    label = "Scale the data before analysis?",
                    value = TRUE),
      selectInput(inputId = "plot_type",
                  label = h3("Select plot type"),
                  choices = list(
                    "Eigenvalues" = "eigenvalues",
                    "Choice 2" = 2,
                    "Choice 3" = 3),
                  selected = 1)
    ),

    mainPanel(
      plotOutput("plot"),
      helpText('Notes:'),
      verbatimTextOutput('notes')
    )
  )

))
