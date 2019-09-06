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
                tags$hr(),
                h5(textOutput(outputId = "warning_output"),style = "color: red;"),
                h5(textOutput(outputId = "warning_output2"), style = "color: red;"), 
                h5(textOutput(outputId = "warning_output3"), style = "color: red;"),
                h5(textOutput(outputId = "warning_output4"), style = "color: red;")
                #uiOutput("column_selection")
            ),
            mainPanel(
                
                dataTableOutput("contents"),
                
                tags$hr(),
                uiOutput("column_selection"),
                dataTableOutput("key")
            )
        ),
        
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
    length_sequences<-sapply(generalized_data()$Sequence,nchar)
    req(input$spot_id_selection != "None",
        (input$sequence_selection != "None"),
        (input$probe_selection != "None"),
        (input$signal_selection != "None"),
        (all(length_sequences == length_sequences[1]))
    )
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
output$warning_output <- renderText({warning_text()})
output$warning_output2 <- renderText({warning_text2()})
output$warning_output3 <- renderText({warning_text3()})
output$warning_output4 <- renderText({warning_text4()})
warning_text<-reactive({
    req(generalized_data())
    txt<-c()
    if (input$spot_id_selection == "None"){
        txt <-c(txt,"Missing Name")
    }
    if (input$sequence_selection== "None"){
        txt <-c(txt,"Missing Sequence")
    }
    if (input$probe_selection == "None"){
        txt <-c(txt,"Missing Probe")
    }
    if (input$signal_selection == "None"){
        txt <-c(txt,"Missing Signal")
    }
    paste(txt, collapse="\n")
})

warning_text2<-reactive({
    req(generalized_data())
    length_sequences <- sapply(generalized_data()$Sequence, nchar)
    if(! all(length_sequences == length_sequences[1])){
        "Not all sequences are the same length"
    }
})
warning_text3<-reactive({
    req(generalized_data())
    upper_sequences <- sapply(generalized_data()$Sequence, toupper)
    if(! all(upper_sequences == generalized_data()$Sequence)){
        "Not all sequences are uppercase"
    }
})
warning_text4<-reactive({
    req(generalized_data())
    letter_matrix <-sapply(generalized_data()$Sequence,strsplit,"")
    if(! all(unlist(letter_matrix) %in% c(AA_STANDARD,"X"))){
        "Sequences contain non standard AA library characters."
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
                           label = shiny::HTML("<p><span style='color: red'>Name *</span></p>"),
                           choices = c("None",headers),
                           selected = "Name"),
               bsPopover(id = "spot_id_selection",
                         "Name of spots",
                         paste("Please select the column name which contains the",
                               "names for the positions on the array. It should",
                               "identify one of the components in the protein",
                               "protein interaction being studied. This label",
                               "should be unique, and contain no duplicates within",
                               "the same experiment. "),
                         placement = "right")
               ),
        column(width = 6,
               selectInput(inputId = "array_selection",
                           label = shiny::HTML("<p><span>Slide Template</span></p>"),
                           choices = c("None",headers),
                           selected = "slide_template"),
               bsPopover(id = "array_selection",
                         "Array Configuration",
                         paste("For datasets with multiple array configurations.",
                               "Please select the name of the column which",
                               "contains the type of array. Often in binding",
                               "array experiments multiple arrays are used to increase",
                               "the amount of peptides to be tested with a single",
                               "probe. For other types of experiments this may",
                               "not be needed."),
                         placement = "right")
               )
      ),
      fluidRow(
        column(width = 6,
               selectInput(inputId = "sequence_selection",
                           label = shiny::HTML("<p><span style='color: red'>Sequence *</span></p>"),
                           choices = c("None",headers),
                           selected = "Sequence"),
               bsPopover(id = "sequence_selection",
                         "Sequence",
                         paste("Please select the column which contains the",
                               "sequences.",
                               "Similar to the Name column, this should be unique",
                               "within the experiment. These should be aligned and",
                               "of the same length. Shorter sequences can be padded",
                               " with the X character. See manual for more details"),
                         placement = "right")
               ),
        column(width = 6,
               selectInput(inputId = "experiment_selection",
                           label = shiny::HTML("<p><span>Experiment ID</span></p>"),
                           choices = c("None",headers),
                           selected = "Experiment_Number"),
               bsPopover(id = "experiment_selection",
                         "Experiment",
                         paste("If data set contains multiple experiments for a",
                               "given probe-array combination, please select the",
                               "column name which contains the identifier for",
                               "these experiments. For binding array experiments,",
                               "the experiment can refer to a specific incubation",
                               "or imaging run with an array or set of arrays."),
                         placement = "right")
        )
      ),
      fluidRow(
        column(width = 6,
               selectInput(inputId = "signal_selection",
                           label = shiny::HTML("<p><span style='color: red'>Signal/Modified *</span></p>"),
                           choices = c("None",headers),
                           selected = "Signal"),
               bsPopover(id = "signal_selection",
                         "Signal",
                         paste("Please select the column which contains the signal",
                               "or primary quantititative data for the positions in the array.",
                               "This should represent the strenth of the protein",
                               "protein interaction. This could refer to a modified",
                               "state when studying post translational modifications",
                               "and their effect on binding. "),
                         placement = "right")
               ),
        column(width = 6,
               selectInput(inputId = "background_selection",
                           label = shiny::HTML("<p><span>Background/Unmodified</span></p>"),
                           choices = c("None",headers),
                           selected = "Background"),
               bsPopover(id = "background_selection",
                         "Unmodified/Background",
                         paste("This should be the second of the paired measurements",
                               "which corresponds most with the background or resting",
                               "state of the system. This could refer to an unmodified",
                               "state when studying post translational modifications",
                               "and their effect on binding. This could also be used",
                               "to represent the background from the image used for",
                               "signal quantification."),
                         placement = "right")
               )
      ),
      fluidRow(
          column(width = 6,
                 selectInput(inputId = "probe_selection",
                             label = shiny::HTML("<p><span style='color: red'>Probe *</span></p>"),
                             choices = c("None",headers),
                             selected = "Probe"),
                 bsPopover(id = "probe_selection",
                           "Name of probe",
                           paste("Please select the name of the column which contains",
                                 "the name of the probe used. For other types of",
                                 "experiments this should be the identifier for the",
                                 "other component in the protein protein interaction"),
                           placement = "right")
          ),
      
        column(width = 6,
               selectInput(inputId = "intra_experiment_replicates",
                           label = shiny::HTML("<p><span>Slide position</span></p>"),
                           choices = c("None",headers),
                           selected = "slide_position"),
               bsPopover(id = "intra_experiment_replicates",
                         "replicate_selection",
                         paste("If your dataset contains replicates within",
                               "experiments. Please select the name of the column",
                               "which containes the identifiers for these replicates.",
                               "Often binding arrays can contain duplicates of the",
                               "array within a slide. This column can be used",
                               "to distinguish these or any other type of replicates",
                               "within the experiment. For an experiment where the",
                               "left and right sides of the slide are replicates",
                               "this column would contain the values left or right",
                               "to distinguish these."),
                         placement = "right")
               )
      ),
      fluidRow(
        column(width = 6,
               selectInput(inputId = "row_location",
                           label = shiny::HTML("<p><span>Row of Spot</span></p>"), 
                           choices = c("None",headers),
                           selected = "Row"),
               bsPopover(id = "row_location",
                         "Row of Spot",
                         paste("Row location of the spot in the array. This is",
                               "necessary for visualizing an accurate digital",
                               "representation of the array. "),
                         placement = "right")
        ),
        column(width = 6,
               selectInput(inputId = "column_location",
                           label = shiny::HTML("<p><span>Column of Spot</span></p>"),
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
      shiny::HTML("<b><span style='color: red'>Required Fields *</span></b>")



    )
})
