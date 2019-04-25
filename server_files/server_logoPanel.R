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
    ggseqlogo(logo_data()$Sequence,seq_type='aa')
})
output$test_logo<- renderPlot({
    gglogo()
})

callModule(downloadFigure,"logo",gglogo)