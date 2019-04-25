downloadFigureUI<- function(id, filename = "no_filename", download_label = "no_label"){
    ns <- NS(id)
    #uiOutput(ns("reactive_Column_Dropdown"))
    tagList(
        splitLayout(
            textInput(ns("downloadFileName"), "Filename: ", filename ),
            pickerInput(inputId = ns("FileType"),label = "File Type",choices = c(".svg",".png"),selected = ".svg"),
            downloadButton(ns("download"), download_label),
            cellArgs = list (style = "overflow:visible")
        ),
        splitLayout(
            numericInput(inputId = ns("downloadWidth"), label = "Width: ", value = 7, min = 1, max = 15, step = 0.5),
            numericInput(inputId = ns("downloadHeight"), label = "Height: ", value = 5, min = 1, max = 15, step = 0.5)
        ),
        tags$style(type='text/css',paste0("#",ns("download")," { width:100%; margin-top: 25px;}"))
    )
}


downloadFigure <- function(input, output, session, figure) {
    output$download<-downloadHandler(
        filename = function(){paste0(input$downloadFileName, input$FileType)},
        content = function(file) {
            ggsave(file,figure(), width = input$downloadWidth, height = input$downloadHeight)
            #write.csv(display_data(), file)
        }
    )   
}

