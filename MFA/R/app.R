#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

ui <- shinyUI(fluidPage(

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

source("mfa.R")
source("MFAHelperFunctions.R")

server <- shinyServer(function(input, output) {

  output$plot <- renderPlot({
    url <- "https://raw.githubusercontent.com/ucb-stat243/stat243-fall-2016/master/problem-sets/final-project/data/wines.csv"
    col_names = c("ID", "1: cat pee", "1: passion fruit", "1: green pepper",
                  "1: mineral", "1: smoky", "1: citrus", "2: cat pee", "2: passion fruit",
                  "2: green pepper", "2: mineral", "2: tropical", "2: leafy",
                  "3: cat pee", "3: passion fruit", "3: green pepper", "3: mineral",
                  "3: grassy", "3: flinty", "4: cat pee", "4: passion fruit",
                  "4: green pepper", "4: mineral", "4: leafy", "5: cat pee",
                  "5: passion fruit", "5: green pepper", "5: mineral", "5: vegetal",
                  "5: hay", "6: cat pee", "6: passion fruit", "6: green pepper", "6: mineral",
                  "6: melon", "7: cat pee", "7: passion fruit", "7: green pepper",
                  "7: mineral", "8: cat pee", "8: passion fruit", "8: green pepper",
                  "8: mineral", "8: grass", "8: smoky", "9: cat pee", "9: passion fruit",
                  "9: green pepper", "9: mineral", "9: peach", "10: cat pee",
                  "10: passion fruit", "10: green pepper", "10: mineral", "titratable acidity",
                  "pH", "alcohol", "residual sugar")
    data <- read.csv(url, col.names = col_names)
    result <- mfa(
      data = data,
      sets = list(2:7, 8:13, 14:19, 20:24, 25:30, 31:35,
                  36:39, 40:45, 46:50, 51:54),
      supplData <- c(55:58),
      ncomps = input$ncomps,
      center = input$center,
      scale = input$scale)

    wine_number = as.integer(input$wine_number)
    if (is.na(wine_number) || wine_number > 12) {
      wine_number = 0
    }
    accessor_number = as.integer(input$accessor_number)
    if (is.na(accessor_number) || accessor_number > 10) {
      accessor_number = 0
    }

    switch(
      input$plot_type,
      "eigenvalues" = plot_eigenvalues(result),
      "factor_scores" = plot_factor_scores(result),
      "partial_factor_scores" = plot_partial_factor_scores(result, accessor_number, wine_number),
      "variable_loadings" = plot_variable_loadings(result, accessor_number)
    )
  })

  output$notes <- renderText({
    switch(
      input$plot_type,
      "eigenvalues" =
        paste('Eigenvalues of the input dataset (scale and center applicable)'),

      "factor_scores" =
        paste('Factor scores of the input dataset within two principal components'),

      "partial_factor_scores" =
        paste('Partial factor scores projected into the compromise as supplementary elements. Each assessor is represented by a dot, and for each item a line connects the item factor scores to the partial factors scores of a given assessor for this item'),

      "variable_loadings" =
        paste('Partial factor scores and variable loadings for the first two dimensions of the compromise space. The loadings have been re-scaled to have a variance equal the singular values of the compromise analysis.')
    )
  })

})

# Run the application
shinyApp(ui = ui, server = server)

