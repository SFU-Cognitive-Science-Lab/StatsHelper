#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)


shinyServer(function(session, input, output) {

    
    
    ### Data Input
    
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
    
    
    
    ### Variable Selection Options
    
    # this reacts to the csv input and the selection of number of comparison groups
    #  to update the input options for the response and grouping variables
    observe({
        
        if(input$GroupNum == 1){

            updateSelectInput(session,"response",
                label = 'Please Select Your Dependent Variable',
                choices = colnames(dat()))
            
            ############################################################
            ### I don't know if this actually works, the intended purpose is to
            ###  remove the "grouping" question if GroupNum = 1, this needs to be tested
            updateSelectInput(session,"grouping", label = NULL, choices = NULL)
            ############################################################
            
        } else {
            
            if(input$DataFormat == 'Long'){
                
                updateSelectInput(session,"response",
                    label = 'Please Select Your Dependent Variable',
                    choices = colnames(dat()))
                updateSelectInput(session,"grouping",
                    label = 'Please Select Your Grouping Variable',
                    choices = colnames(dat()))
                
            } else {
                
                updateSelectInput(session,"response",
                    label = 'Please Select Your First Variable',
                    choices = colnames(dat()))
                updateSelectInput(session,"grouping",
                    label = 'Please Select Your Second Variable',
                    choices = colnames(dat()))
                
            }
        }
    })
    
    
    
    ### Running the Test
    
    # this takes the input from the variable options, then runs the required
    #  t test for the research question
    fit <- reactive ({
        if (input$GroupNum == 1){          
        
            fit1 <- t.test(input$response, data = dat())
       
        } else {
            
            if(input$DataFormat == 'Long'){
                
                fit1 <- t.test(input$response ~ input$grouping, data = dat())
                
            } else {
                
                fit1 <- t.test(input$response, input$grouping, data = dat())
                
            }
        } 
       
        return(fit1)
    })
    
    
    # this updates the ui to display the test results
    output$test.results <- renderUI({
        # the html code just saves the spacing and line breaks of the actual R output
        HTML(
            paste(
                c("<pre>", capture.output(print(fit())), "</pre>"),
                collapse = "<br>"
            )
        )
    })
    
})


##### UI Elements Needed #####

# 'file' csv input

# 'header' does your csv have headers?

# 'GroupNum' Number of Groups to Compare -- currently only supports 1 or 2

# 'DataFormat' is your data long or wide format? -- should be asked after GroupNum

# 'response' options for response variable

# 'grouping' options for grouping variable (or second variable in wide form)

# 'test.results' text output for the test results























