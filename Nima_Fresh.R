library(shiny)
library(tidyverse)
recipes <- read.csv('Hello_Nima', header = T, sep = '\t')
recipes <- recipes[(recipes$Portions == 2),]
ui <- fluidPage(
  tabsetPanel(
    tabPanel('Select number of Meals',
             numericInput(inputId = 'Portions',
                          label = 'Portions',
                          min = 2,
                          max = 4,
                          value = 2),
                  sliderInput(inputId = 'integer',
                              label = 'Numer of meals',
                              min = 1,
                              max = 7, 
                              value = 1),
                  selectInput(inputId = 'Type',
                              label = 'Type',
                              choices = c('Vegeterian' = 'Veg',
                                             'Chicken' = 'Kyckling',
                                             'Beef_Pork' = 'Blandfärs'),
                              multiple = TRUE),
                  textOutput("text"),
                  dataTableOutput('tbl')
    )
  )
)
server <- function(input, output){
  # t <- t(selected_meal)
  n_portions <- reactive(as.numeric(input$Portions))
  output$text <- renderText(as.integer(n_portions()))
  my_recipes <- reactive({
    tmp <- (recipes[recipes$Type %in% input$Type,4:ncol(recipes)])
    #rownames(tmp) <- recipes$Food
    tmp <- colSums(as.matrix(na.omit(tmp))) * as.integer(n_portions(), na.rm = T)
    t_tmp <- t(tmp)
    rownames(t_tmp) <- colnames(tmp)
    t_tmp
  })
  # output$tbl <- renderDataTable(rowsum({t(recipes[recipes$Type %in% input$Type,4:ncol(recipes)])}, na.rm = TRUE))
  output$tbl <- renderDataTable({my_recipes()})
  
}
shinyApp(ui = ui , server = server)


