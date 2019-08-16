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
    PSSM <- consensusMatrix(string_set, as.prob = TRUE)
})
gglogo<-reactive({
    if (input$logo_type == "EDLOGO"){
        validate(
            need(nrow(logo_data()) > 8, "EDLogo requires more than 8 samples")
        )
        if(input$custom_background == TRUE){
            logo = Logolas::logomaker(logo_data()$Sequence,bg = background_rates(), type = "EDLogo")
        }else{
            logo = Logolas::logomaker(logo_data()$Sequence, type = "EDLogo")
        }
    } 
    if (input$logo_type == "ggseqlogo"){
        logo = ggseqlogo(logo_data()$Sequence,seq_type='aa')
    } 
    logo
})
output$test_logo<- renderPlot({
    gglogo()
})

callModule(downloadFigure,"logo",gglogo)