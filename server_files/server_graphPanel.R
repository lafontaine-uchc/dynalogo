callModule(reactiveColumnDropdown,
           "xaxis",
           "Select X-axis Data",
           get_avgs,
           "Name")
graph_data<-reactive({
  temp<-data_sorted()
  if (input$'xaxis-reactiveColDropdown' == "Name"){
    temp$Name<-as.character(temp$Name)
  }
  temp
})
ggbarplot <- reactive({
    #ggplot(temp, aes(Signal) + geom_bar(aes(temp$Signal)))
    graph_column<-sym(input$graph_by)
    ylabel<-sub("_"," ",input$graph_by)
    xaxis<-sym(input$'xaxis-reactiveColDropdown')

        if (input$sorting == "ascending"){
            g <- ggplot(data = graph_data(), aes(x = reorder(!!(xaxis), (!!(graph_column))),y = (!!(graph_column)),fill = Probe,label = !!(xaxis))) +
                geom_bar(stat = "identity") +
                theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust=1,size = input$x_axis_font_size)) +
                xlab(input$'xaxis-reactiveColDropdown')+ ylab(ylabel)+ggtitle(paste(input$chosen_SH2s,collapse = ' ')) + labs(fill = input$probe_selection) #+geom_text(aes(label =temp$Name), vjust =0, hjust =0, angle = 90)
        }else if(input$sorting == "descending"){
            g <- ggplot(data = graph_data(), aes(x = reorder(!!(xaxis), -(!!(graph_column))),y = (!!(graph_column)),fill = Probe,label = !!(xaxis))) +
                geom_bar(stat = "identity") +
                theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust=1,size = input$x_axis_font_size)) +
                xlab(input$'xaxis-reactiveColDropdown')+ ylab(ylabel)+ggtitle(paste(input$chosen_SH2s,collapse = ' ')) + labs(fill = input$probe_selection)#+geom_text(aes(label =temp$Name), vjust =0, hjust =0, angle = 90)
        }else {
            g <- ggplot(data = graph_data(), aes(x =!!(xaxis),y = (!!(graph_column)),fill = Probe)) +
                geom_bar(stat = "identity") +
                theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust=1,size = input$x_axis_font_size)) +
                xlab(input$'xaxis-reactiveColDropdown') + ylab(ylabel)+ggtitle(paste(input$chosen_SH2s,collapse = ' '))+ labs(fill = input$probe_selection)#+geom_text(aes(label =temp$Name), vjust =0, hjust =0, angle = 90)
        }
        if(input$hide_x_axis == TRUE){
          g<- g + theme(axis.title.x=element_blank(),
                       axis.text.x=element_blank(),
                       axis.ticks.x=element_blank())
        }

    g
})
output$histly <- renderPlotly({
    ggplotly(ggbarplot(), tooltip = c(input$'xaxis-reactiveColDropdown', input$graph_by, "Probe"))
})
output$hist <- renderPlot({
  ggbarplot()
})
callModule(downloadFigure,"bargraph",ggbarplot)