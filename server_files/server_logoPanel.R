seq_processing <- function(temp_data,seq_len){
    #removes sequences which are either too long or too short
    #browser()
    #temp_data$new_sequence<-gsub("-","",as.character(temp_data$Sequence))
    #browser()
    temp_data$length<-nchar(temp_data$Sequence)
    temp_data<-temp_data[temp_data$length==input$sequence_length,]
    temp_data    
}
logo_data<-reactive({
    seq_processing(filtered_data(),input$sequence_length)
})
background_data <- reactive({
    seq_processing(get_avgs(),input$sequence_length)
})
background_rates<- reactive({
#    background_data()$Sequence
    string_set <- AAStringSet(background_data()$Sequence)
    PSSM <- consensusMatrix(string_set, baseOnly=TRUE, as.prob = TRUE)
})
foreground_rates<- reactive({
    #    background_data()$Sequence
    string_set <- AAStringSet(logo_data()$Sequence)
    PSSM <- consensusMatrix(string_set, baseOnly=TRUE, as.prob = TRUE)
})
add_zero_rows_to_PSSM<- function(PSSM1,PSSM2){
    while (length(rownames(PSSM1)) < length(rownames(PSSM2))){
        missing_row_index <- rownames(PSSM2) %in% rownames(PSSM1)
        missing_row_name <- rownames(PSSM2)[!missing_row_index][1]
        PSSM1 <-rbind(PSSM1, rep(0, dim(PSSM1)[2]))
        rownames(PSSM1)[rownames(PSSM1) == ""]<- missing_row_name
        PSSM1<-as.data.frame(PSSM1)
        PSSM1<-PSSM1[order(row.names(PSSM1)), ]
        PSSM1<-as.matrix(PSSM1)
    }
    PSSM1
}

gglogo<-reactive({
    if (input$logo_type == "EDLOGO"){
        validate(
            need(nrow(logo_data()) > 8, "EDLogo requires more than 8 samples")
        )
        if(input$custom_background == TRUE){
            fr <-add_zero_rows_to_PSSM(foreground_rates(), background_rates())
            br <-add_zero_rows_to_PSSM(background_rates(), foreground_rates())
            logo = Logolas::logomaker(fr,bg = br, type = "EDLogo")
        }else{
            logo = Logolas::logomaker(foreground_rates(), type = "EDLogo")
        }
    } 
    if (input$logo_type == "ggseqlogo"){
        logo = ggseqlogo(foreground_rates(),seq_type='aa')
    } 
    logo
})
output$test_logo<- renderPlot({
    gglogo()
})

callModule(downloadFigure,"logo",gglogo)