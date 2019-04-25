callModule(reactiveColumnDropdown,
           "array_tooltip",
           "Select Label for Spots",
           get_avgs,
           "Name")

digital_array_data<-reactive({
    req(input$digital_array == TRUE)
    req(input$array_sh2_display)
    temp<-get_avgs()
    temp<-temp %>% filter(Probe == input$array_sh2_display)
    temp$spotnum<-as.numeric(as.factor(temp$Name))
    
    if(("Column" %in% colnames(temp))&&("Row" %in% colnames(temp))){
        temp$Column<-as.numeric(temp$Column)
        LETTER2num <- function(x) {utf8ToInt(x) - utf8ToInt("A") + 1L}
        temp$Row<-lapply(as.character(temp$Row), LETTER2num)
        temp$Row<-as.numeric(temp$Row)
    }else{
        temp$Row<-(temp$spotnum %% 16) +1
        temp$Column<-((temp$spotnum %% 384) %/% 16) + 1
        temp$groupnum<-(temp$spotnum %/% 384) + 1
        temp<-temp%>%filter(groupnum == input$selectgroup)
    }
    temp
})

output$array_sh2_slider <- renderUI({
    sliderTextInput(inputId = "array_sh2_display","Select Probe to Display", choices = input$chosen_SH2s, animate = TRUE)
})  
output$array_group_slider <- renderUI({
  req(get_avgs())
  temp <-get_avgs()
  if(("Column" %in% colnames(temp))&&("Row" %in% colnames(temp))){}else{
    sliderInput(inputId = "selectgroup","Select Group To Display",min = 1, max = length(levels(as.factor(get_avgs()$Name)))%/%384+1, animate = TRUE,value = 1)
}})  
output$ploty<-renderPlotly({
    plot_ly(digital_array_data(),x = ~Column, y = ~Row,
            text = ~paste(input$`array_tooltip-reactiveColDropdown`,": ", get(input$`array_tooltip-reactiveColDropdown`), input$signal_selection,":", Average_Signal),
            opacity= ~Average_Signal,
            color = ~Average_Signal,
            colors = "Blues",
            #color= "Red",
            marker = list(size = 15),
            #colorscale = list(c(0, "rgb(150, 0, 0)"), list(1, "rgb(0, 255, 0)")),
            cauto = T
            
    ) %>%
        layout(yaxis = list(autorange = "reversed"))
})
