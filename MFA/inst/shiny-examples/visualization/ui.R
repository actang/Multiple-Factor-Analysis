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
                  max = 10,
                  value = 2),
      checkboxInput(inputId = "center",
                    label = "Center the data before analysis?",
                    value = TRUE),
      checkboxInput(inputId = "scale",
                    label = "Scale the data before analysis?",
                    value = TRUE),
      selectInput(inputId = "plot_type",
                  label = "Select plot type",
                  choices = list(
                    "Eigenvalues" = "eigenvalues",
                    "Factor Scores" = "factor_scores",
                    "Partial Factor Scores" = "partial_factor_scores",
                    "Variable Loadings" = "variable_loadings"),
                  selected = 1),
      textInput(inputId = "wine_number",
                label = "Optional: Input Item Numbers [1, 12]",
                value = ""),
      textInput(inputId = "accessor_number",
                label = "Optional: Input Accessor Numbers [1, 10]",
                value = "")
    ),

    mainPanel(
      plotOutput("plot"),
      helpText('Notes:'),
      verbatimTextOutput('notes')
    )
  )

))
