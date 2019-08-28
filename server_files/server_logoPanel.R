seq_processing <- function(temp_data,seq_len){
    #removes sequences which are either too long or too short
    #browser()
    #temp_data$new_sequence<-gsub("-","",as.character(temp_data$Sequence))
    #browser()
    temp_data$length<-nchar(temp_data$Sequence)
    temp_data<-temp_data[temp_data$length==input$sequence_length,]
    temp_data    
}
add_missing_AA_rows<- function(PSSM){
    character_library <- AA_STANDARD
    missing_rows_index <- !(character_library %in% rownames(PSSM))
    if (sum(missing_rows_index >0)){
        missing_row_characters<-character_library[missing_rows_index]
        missing_rows = matrix(rep(0, length(missing_row_characters) * ncol(PSSM)),
                              nrow=length(missing_row_characters),
                              dimnames = list(missing_row_characters))
        PSSM<-rbind(PSSM, missing_rows)
        PSSM<-PSSM[order(row.names(PSSM)), ]
    }
    PSSM
}

remove_non_AA_rows <- function(PSSM){
    character_library <- AA_STANDARD
    PSSM<-PSSM[character_library,]
    PSSM
}
logo_data<-reactive({
    #seq_processing(filtered_data(),input$sequence_length)
    filtered_data()
})
background_data <- reactive({
    #seq_processing(get_avgs(),input$sequence_length)
    get_avgs()
})
background_rates<- reactive({
#    background_data()$Sequence
    string_set <- Biostrings::AAStringSet(background_data()$Sequence)
    PSSM <- Biostrings::consensusMatrix(string_set, baseOnly=TRUE, as.prob = TRUE)
    PSSM <- add_missing_AA_rows(PSSM)
    PSSM <- remove_non_AA_rows(PSSM)
})
foreground_rates<- reactive({
    #    background_data()$Sequence
    string_set <- Biostrings::AAStringSet(logo_data()$Sequence)
    PSSM <- Biostrings::consensusMatrix(string_set, baseOnly=TRUE, as.prob = TRUE)
    PSSM <- add_missing_AA_rows(PSSM)
    PSSM <- remove_non_AA_rows(PSSM)
})

gglogo<-reactive({
    if (input$logo_type == "EDLOGO"){
        validate(
            need(nrow(logo_data()) > 8, "EDLogo requires more than 8 samples")
        )
        if(input$custom_background == TRUE){
            logo = Logolas::logomaker(foreground_rates(),bg = background_rates(), type = "EDLogo")
        }else{
            logo = Logolas::logomaker(foreground_rates(), type = "EDLogo")
        }
    } 
    if (input$logo_type == "ggseqlogo"){
        logo = ggseqlogo(foreground_rates(),seq_type='aa')
    }
    if (input$logo_type == "Logo"){
        if(input$custom_background == TRUE){
            logo = Logolas::logomaker(foreground_rates(),bg = background_rates(), type = "Logo")
        }else{
            logo = Logolas::logomaker(foreground_rates(), type = "Logo")
        }
    }
    logo
})
output$test_logo<- renderPlot({
    gglogo()
})

callModule(downloadFigure,"logo",gglogo)
