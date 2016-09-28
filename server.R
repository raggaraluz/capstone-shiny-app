
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#
source('predictor.R')


library(shiny)

shinyServer(function(input, output, session) {

  prediction <- reactive({
      prediction <- get.prediction(input$text)
      prediction
  })  
  
  output$pred1 <- renderUI({
      if(nrow(prediction()) > 0) actionButton("action1", label = prediction()$wp[1], width = '100%')
      else actionButton('none1', label = '---', width = '100%')
  })
  
  output$pred2 <- renderUI({
      if(nrow(prediction()) > 1) actionButton("action2", label = prediction()$wp[2], width = '100%')
      else actionButton('none2', label = '---', width = '100%')
  })
  
  output$pred3 <- renderUI({
      if(nrow(prediction()) > 2) actionButton("action3", label = prediction()$wp[3], width = '100%')
      else actionButton('none3', label = '---', width = '100%')
  })
  

  observe({
      if(!is.null(input$action1) && input$action1 > 0) {
        isolate({
            t <- paste(input$text, prediction()$wp[1])
            updateTextInput(session, 'text', value = t)
        })
      }
      
      if(!is.null(input$action2) && input$action2 > 0) {
          isolate({
              t <- paste(input$text, prediction()$wp[2])
              updateTextInput(session, 'text', value = t)
          })
      }
      
      if(!is.null(input$action3) && input$action3 > 0) {
          isolate({
              t <- paste(input$text, prediction()$wp[3])
              updateTextInput(session, 'text', value = t)
          })
      }
  })
  
  output$stats <- renderTable(include.rownames = FALSE,
  {
      if (is.null(prediction()) || nrow(prediction()) == 0) {
          return()
      }

      prediction() %>% select(source, `predicted word` = wp,
                              `probability (%)` = percentage)
  })
})
