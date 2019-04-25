output$select_SH2 <- renderUI({
    temps<-generalized_data()
    SH2_list<-c(levels(as.factor(temps$Probe)))
    pickerInput("chosen_SH2s","Select Probes", choices=SH2_list, options = list(`actions-box` = TRUE),multiple = T,selected = SH2_list)
})
output$template_selector<-renderUI({
    temps<-generalized_data()
    template_list<-c(levels(as.factor(temps$slide_template)))
    checkboxGroupInput(inputId = "chosen_templates",choices = template_list,label = "Select Templates", inline = T, selected = template_list[0])
})
callModule(reactiveColumnDropdown,
           "foreground",
           "Select column for Modified",
           slide_filter,
           "Signal")
callModule(reactiveColumnDropdown,
           "background",
           "Select Column for Unmodified",
           slide_filter,
           "Background")
output$split_by <- renderUI({
    req(slide_filter())
    columnNames<-colnames(slide_filter())
    pickerInput("sort_by_column_selector", "Select Column to Filter By", choices = append(columnNames,"none"), selected = "none")
})

output$split_by_selector<- renderUI({
    req(input$sort_by_column_selector)
    if (input$sort_by_column_selector != "none"){
        choice<-slide_filter()%>%select(input$sort_by_column_selector)%>%distinct()
        pickerInput("sort_by_choice_selector", "Select value", choices = choice,multiple = TRUE,options = list(`actions-box` = TRUE))
    }
})