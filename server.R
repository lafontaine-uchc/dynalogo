options(shiny.maxRequestSize=100*1024^2)
server <- function(input, output){
    #prevent loading of data displays before data is loaded
    source("./server_files/server_uiDisplayLoadingControl.R",local = TRUE)
    
    source("./server_files/server_frontPage.R",local = TRUE)
    
    #Filtering & Transformation Panel
    source("./server_files/server_filteringTransformation.R",local = TRUE)
    
    #Data summation and thresholding panel
    source("./server_files/server_dataSummationAndThresholding.R",local = TRUE)
    
    #Dataprocessing steps
    source("./server_files/server_dataProcessing.R", local = TRUE)
    
    #Panel for Distribution
    source("./server_files/server_distributionPanel.R", local = TRUE)
    
    #Panel for graph and historgram
    source("./server_files/server_graphPanel.R", local = TRUE)
    
    #Panel for digital array
    source("./server_files/server_digitalArray.R", local = TRUE)
    
    #Panel for logoplot
    source("./server_files/server_logoPanel.R", local = TRUE)
    
    #Panel for data table
    source("./server_files/server_dataTablePanel.R", local = TRUE)
    
    #Modal for import
    source("./server_files/server_importModal.R", local = TRUE)

    output$SH2s<-renderText({input$chosen_SH2s})
    output$loading<-renderText({'No Data Selected'})
}