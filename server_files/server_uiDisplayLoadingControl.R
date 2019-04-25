#prevents loading data displays and controls without data
rv <- reactiveValues()
#prevents data visualization ui from displaying initially
rv$setupComplete <- FALSE

# looks for completion of data file import
observe({
    req(generalized_data())
    rv$setupComplete <- TRUE
    # the conditional panel reads this output
    output$setupComplete <- reactive({
        return(rv$setupComplete)
    })
    outputOptions(output, 'setupComplete', suspendWhenHidden=FALSE)
    
})