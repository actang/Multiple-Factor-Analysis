#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

source("temp.R")
library(shiny)

shinyServer(function(input, output) {

  output$plot <- renderPlot({
    result <- mfa(
      data = data,
      sets = list(2:7, 8:13, 14:19, 20:24, 25:30, 31:35,
                  36:39, 40:45, 46:50, 51:54),
      supplData <- c(55:58),
      ncomps = input$ncomps,
      center = input$center,
      scale = input$scale)

    switch(
      input$plot_type,
      "eigenvalues" = plot_eigenvalues(result)
    )
  })

  output$notes <- renderText({
    switch(
      input$plot_type,
      "eigenvalues" =
        paste('Eigenvalues of the input dataset (scale and center applicable)')
    )
  })

})

