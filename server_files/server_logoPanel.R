logo_data<-reactive({
    temp_data<-filtered_data()
    #browser()
    #temp_data$new_sequence<-gsub("-","",as.character(temp_data$Sequence))
    #browser()
    
    temp_data$length<-nchar(temp_data$Sequence)
    temp_data<-temp_data[temp_data$length==input$sequence_length,]
    temp_data
})
gglogo<-reactive({
    if (input$logo_type == "EDLOGO"){
        validate(
            need(nrow(logo_data()) > 8, "EDLogo requires more than 8 samples")
        )
        logo = Logolas::logomaker(logo_data()$Sequence, type = "EDLogo")
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