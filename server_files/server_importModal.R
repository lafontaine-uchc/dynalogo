dataModal <- function(failed = FALSE) {
    modalDialog(
        size = "l",
        tags$head(
            # this changes the size of the popovers
            tags$style(".popover{width:200px;}")
        ),
        sidebarLayout(
            sidebarPanel(
                fileInput("file1",
                          "Choose Data File(s)",
                          multiple = TRUE,
                          accept = c("text/csv",
                                     "text/comma-separated-values,text/plain",
                                     ".csv")),
                bsPopover(id = "file1",
                          "Import Dataset",
                          "Import Dataset as one or multiple text files",
                          placement = "right"),
                
                # Input: Checkbox if file has header ----
                checkboxInput("header",
                              "Header",
                              TRUE),
                bsPopover(id = "header",
                          "Header: Yes or No",
                          "If the first row of the files contains column names, select this option",
                          placement = "right"),
                
                # Input: Select separator ----
                radioButtons("sep", "Separator",
                             choices = c(Comma = ",",
                                         Semicolon = ";",
                                         Tab = "\t"),
                             selected = "\t"),
                bsPopover(id = "sep",
                          "Separator",
                          "Are the data entries in the file separated by commas, semicolons or tabs",
                          placement = "right"),
                
                # Input: Select quotes ----
                radioButtons("quote", "Quote",
                             choices = c(None = "",
                                         "Double Quote" = '"',
                                         "Single Quote" = "'"),
                             selected = '"'),
                bsPopover(id = "quote",
                          "Quote Type",
                          "Are the data entries surrounded by single or double quotes",
                          placement = "right"),
                
          
                # Horizontal line ----
                tags$hr()
                
                #uiOutput("column_selection")
            ),
            mainPanel(
                
                dataTableOutput("contents"),
                
                tags$hr(),
                uiOutput("column_selection"),
                dataTableOutput("key")
            )
        ),

        div(tags$b("Please import a dataset before continuing", style = "color: red;")),
        
        footer = tagList(
            actionButton("cancel","Cancel"),
            actionButton("ok", "OK")
        )
    )
}

# Show modal when button is clicked.
observeEvent(input$show, {
    #hide("logoGif")
    hide("Description")
    hide("Description2")
    showModal(dataModal())
})
observeEvent(input$show2, {
    showModal(dataModal())
    hide(id = "headerbar")
})
observeEvent(input$cancel, {
    removeModal()
    if (rv$setupComplete==FALSE) {
        show(id = "Description")
        show("Description2")
    }
})

observeEvent(input$ok, {
    # Check that data object exists before exiting
    req(generalized_data())
    removeModal()
    shinyjs::show(id = "headerbar")
})


imported_data<-reactive({
    req(input$file1)
    df <- rbindlist(lapply(input$file1$datapath, fread, 
                           sep = input$sep,
                           quote = input$quote,
                           header = input$header))
    df
})


imported_key<-reactive({
    req(input$file2)
    df <- rbindlist(lapply(input$file2$datapath, fread, 
                           sep = input$sep2,
                           quote = input$quote2,
                           header = input$header))
    if(input$disp == "head") {
        return(head(df))
    }
    else {
        return(df)
    }
})
output$contents <- renderDataTable(
    imported_data(),
    options = list(scrollX = TRUE,pageLength = 1)
)
output$key <- renderDataTable(
    imported_key(),
    options = list(scrollX = TRUE,pageLength = 5)
)
output$text_test <- renderText(
    colnames(imported_data())
)
output$column_selection <- renderUI({
    headers<- colnames(imported_data())
    verticalLayout(
      fluidRow(
        column(width = 6,
               selectInput(inputId = "spot_id_selection",
                           label = "Name *",
                           choices = c("None",headers),
                           selected = "Name"),
               bsPopover(id = "spot_id_selection",
                         "Name of spots",
                         paste("Please select the column name which contains the",
                               "names for the positions on the array"),
                         placement = "right")
               ),
        column(width = 6,
               selectInput(inputId = "array_selection",
                           label = "Slide Template",
                           choices = c("None",headers),
                           selected = "Slide_template"),
               bsPopover(id = "array_selection",
                         "Array Configuration",
                         paste("For datasets with multiple array configurations.",
                               "Please select the name of the column which",
                               "contains the type of array."),
                         placement = "right")
               )
      ),
      fluidRow(
        column(width = 6,
               selectInput(inputId = "sequence_selection",
                           label = "Sequence *",
                           choices = c("None",headers),
                           selected = "Sequence"),
               bsPopover(id = "sequence_selection",
                         "Sequence",
                         paste("Please select the column which contains the",
                               "sequences associated with the positions in the array"),
                         placement = "right")
               ),
        column(width = 6,
               selectInput(inputId = "probe_selection",
                           label = "Probe *",
                           choices = c("None",headers),
                           selected = "Probe"),
               bsPopover(id = "probe_selection",
                         "Name of probe",
                         paste("Please select the name of the column which contains",
                               "the name of the probe used"),
                         placement = "right")
               )
      ),
      fluidRow(
        column(width = 6,
               selectInput(inputId = "signal_selection",
                           label = "Signal/Modified *",
                           choices = c("None",headers),
                           selected = "Signal"),
               bsPopover(id = "signal_selection",
                         "Signal",
                         paste("Please select the column wich contains the signal",
                               "or quantititative data for the positions in the array"),
                         placement = "right")
               ),
        column(width = 6,
               selectInput(inputId = "background_selection",
                           label = "Background/Unmodified",
                           choices = c("None",headers),
                           selected = "Background"),
               bsPopover(id = "background_selection",
                         "Unmodified/Background",
                         paste("This should be the second of the paired measurements",
                               "which corresponds most with the background or resting",
                               "state of the system."),
                         placement = "right")
               )
      ),
      fluidRow(
        column(width = 6,
               selectInput(inputId = "experiment_selection",
                           label = "Experiment #",
                           choices = c("None",headers),
                           selected = "Experiment_Number"),
               bsPopover(id = "experiment_selection",
                         "Experiment",
                         paste("If data set contains multiple experiments for a",
                               "given probe-array combination, please select the",
                               "column name which contains the identifier for",
                               "these experiments."),
                         placement = "right")
        ),
        column(width = 6,
               selectInput(inputId = "intra_experiment_replicates",
                           label = "Slide position",
                           choices = c("None",headers),
                           selected = "Slide_position"),
               bsPopover(id = "intra_experiment_replicates",
                         "replicate_selection",
                         paste("If your dataset contains replicates within",
                               "experiments. Please select the name of the column",
                               "which containes the identifiers for these replicates."),
                         placement = "right")
               )
      ),
      fluidRow(
        column(width = 6,
               selectInput(inputId = "row_location",
                           label = "Row of Spot", 
                           choices = c("None",headers),
                           selected = "Row"),
               bsPopover(id = "row_location",
                         "Row of Spot",
                         paste("Row location of the spot in the array. This is",
                               "necessary for visualizing an accurate digital",
                               "representation of the array."),
                         placement = "right")
        ),
        column(width = 6,
               selectInput(inputId = "column_location",
                           label = "Column of Spot",
                           choices = c("None",headers),
                           selected = "Column"),
               bsPopover(id = "column_location",
                         "Column of Spot",
                         paste("Column location of the spot in the array. This", 
                               "is necessary for visualizing an accurate digital",
                               "representation of the array."),
                         placement = "right")
        )
      ),
      p("* Required Fields")



    )
})