#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(session, input, output) {

    output$distPlot <- renderPlot({

        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white')

    })

    
    
    
    # "file" is the name of the csv input, create object that reads the csv and
    #  updates automatically any time the file is changed
    
    dat <- reactive({
        validate(need(input$file, ""))
        infile <- input$file
        df <- read.csv(infile$datapath,
                       header = input$header,
                       stringsAsFactors = FALSE)
        return(df)
    })
    
    
    # this reacts to the csv input and the selection of number of comparison groups
    #  to update the input options for the response and grouping variables
    observe({
        if(input$GroupNum == 1){
        
            updateSelectInput(session,"response",choices = colnames(dat()))
            
        } else {
            
            updateSelectInput(session,"response",choices = colnames(dat()))
            updateCheckboxGroupInput(session,"predictor",choices = colnames(dat()))
        
        }
    })
    
})


##### UI Elements Needed #####

# 'file' csv input

# 'header' does your csv have headers?

# 'GroupNum' Number of Groups to Compare

# 'response' options for response variable

# 'predictor' options for predictor variable























