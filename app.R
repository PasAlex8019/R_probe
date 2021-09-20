

library(shiny)
df <- read.csv('C:/Users/pas80/Documents/Titanic.csv')

ui <- fluidPage(

    titlePanel("Titanic Passengers"),

    fluidRow(
        column(6,
               selectInput('ubar', 'Barplot:',
                           choices = c(
                               'Survived' = 'survived',
                               'Pclass' = 'pclass',
                               'Sex' = 'sex'
                           ))
               ),
        column(6,
               selectInput('uhist', 'Histogram:',
                           choices = c(
                            'Age' = 'age',
                            'Fare' = 'fare'
                           ))
               )
        ),
        fluidRow(
            column(6,
                   selectInput('ucol1', 'Color 1:',
                               choices = c(
                                   'Red' = 'red',
                                   'Yellow' = 'yellow'
                               ))
                   ),
            column(6,
                   selectInput('ucol2', 'Color 2:',
                               choices = c(
                                   'Green' = 'green',
                                   'Blue' = 'blue'
                               ))
                   )
        ),
     

        mainPanel(
            fluidRow(splitLayout(cellWidths = c(
                "50%", "50%"), 
                plotOutput("distPlot1"), 
                plotOutput("distPlot2"))
                )
))

server <- function(input, output) {

    output$distPlot1 <- renderPlot({
        # generate bins based on input$bins from ui.R
        if(input$ubar == 'survived'){
            hist(df[, 3], col = input$ucol1, border = 'white',
                 xlab = 'Survived',
                 ylab = 'Count',
                 main = 'Histogram of Survived')
        }
        
        if(input$ubar == 'pclass'){
            hist(df[, 4], col = input$ucol1, border = 'white',
                 xlab = 'Pclass',
                 ylab = 'Count',
                 main = 'Histogram of Pclass')
        }
        
        if(input$ubar == 'sex'){
            hist(table(df[, 6]), col = input$ucol1, border = 'white',
                 xlab = 'Sex',
                 ylab = 'Count',
                 main = 'Histogram of Sex')
        }

    })
    
    output$distPlot2 <- renderPlot({
        if(input$uhist == 'age'){
            hist(df[, 7], col = input$ucol2, border = ' navy',
                 xlab = 'Age',
                 ylab = 'Count',
                 main = 'Histogram of Age')   
        }
        if(input$uhist == 'fare'){
            hist(df[, 11], col = input$ucol2, border = ' navy',
                 xlab = 'Fare',
                 ylab = 'Count',
                 main = 'Histogram of Fare')   
        }
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
