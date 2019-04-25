tagList(
    h4("Data Table"),
    splitLayout(
        pickerInput("table_display_selector",
                    "Select Data to Display in Table",
                    choices=c("Imported Data","Boxplot","Scatterplot","Histogram","Graph Data","Digital Binding","Logo Data"),
                    options = list(`actions-box` = TRUE),multiple = F,selected ="Logo Data"),
        textInput("downloadFileName", "Filename: ", "data.csv" ),
        downloadButton("downloadData", "Download Data Table" ),
        cellArgs = list (style = "overflow:visible")
    ),
    dataTableOutput(outputId = "sequences_text"),
    tags$style(type='text/css',"#downloadData { width:100%; margin-top: 25px;}")
)