
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(shinydashboard)

textareaInput <- function(id, label, value = '', rows=20, cols=35, class="form-control", ...){
    tags$div(
        class="form-group shiny-input-container",
        tags$label('for'=id,label),
        tags$textarea(id=id,class=class,rows=rows,cols=cols,value, ...))
}


header <- dashboardHeader(title = "Text Predictor")

body <- dashboardBody(
    box(width = 12, title = "Input Text", solidHeader = T, status = 'primary',
              textareaInput("text", '', rows = 8, width = '100%')
    ),
    box(width = 12, title = "Prediction", solidHeader = T, background = 'blue',
              fluidRow(
                  column(width = 2, uiOutput('pred1')),
                  column(width = 2, uiOutput('pred2')),
                  column(width = 2, uiOutput('pred3'))
              ),
              p(' '),
              p(img(icon('info')), 'Click on any predicted word to add it to the text')
    ),
    box(width = 12, title = 'Stats', solidHeader = T, status = 'primary', collapsible = T, collapsed = T,
        tableOutput("stats"))
)

shinyUI(
    dashboardPage(header, dashboardSidebar(disable = TRUE), body)
)

