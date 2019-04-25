source("./global.R")

ui <- fluidPage(
    useShinyjs(),
    
    #UI for Dynlogo splash page
    source("./ui_files/ui_frontPage.R", local = TRUE)[1],
    
    #Only loads app contros and displays once data is imported
    conditionalPanel(condition = "output.setupComplete",
        source("./ui_files/ui_headerPanel.R",local = TRUE)[1],
        absolutePanel( top = 100, left = 1, right = 0,
            verticalLayout(
                #Filtering & Tranformation panel 
                source("./ui_files/ui_filteringTransformation.R", local = TRUE)[1],
                
                # distribution panel
                source("./ui_files/ui_distributionPanel.R", local = TRUE)[1],

                #Grouping/selecting
                source("./ui_files/ui_dataSummationAndThresholding.R", local = TRUE)[1],

                # Graph frame and controls(commented out code below)
                source("./ui_files/ui_graphPanel.R", local = TRUE)[1],
                
                # digital array plot
                source("./ui_files/ui_digitalArray.R")[1],

                #dynamic logo plot
                source("./ui_files/ui_dynamicLogo.R")[1],
 
                #Data Table window
                source("./ui_files/ui_dataTablePanel.R")[1]
            ),

            #Displays loading dialog if before inputing data
            conditionalPanel(condition = "!output.setupComplete",
            textOutput(outputId = "loading"))
        )
    )
)
