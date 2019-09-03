distributionPlotData<-reactive({
        get_avgs()
})
histogram<-reactive({
    ggplot(data = distributionPlotData(), aes(Average_Signal)) +
        geom_histogram(bins = input$bins)
})
output$hist2 <- renderPlot({
    histogram()
})
callModule(downloadFigure,"histogram",histogram)

boxPlotData<-reactive({
    transformation_for_skew()
        
})

output$boxplotUI<- renderUI({
    req(input$boxplot_type)
    if (input$boxplot_type == "Summary"){
        plotOutput(outputId = "boxplot")
    } else{
        plotlyOutput(outputId = "plotly_boxplot")
    }
})

output$column_selection_ui <- renderUI({
    req(slide_filter())
    columnNames<-colnames(slide_filter())
    pickerInput("column_selection", "Select Column Containing Replicate Data", choices = append(columnNames,"none"), selected = "none")
})

output$choices_in_column_ui<- renderUI({
    req(input$column_selection)
    if (input$column_selection != "none"){
        choice<-slide_filter()%>%select(input$column_selection)%>%distinct()
        pickerInput("choices_in_column", "Select values for x and y axis", choices =choice,selected = choice, multiple = T)
    }
})
output$boxplot <- renderPlot({
summary_box_plot()

})
summary_box_plot<-reactive({
  if(input$intra_experiment_replicates != "None"){
    fill_choice = sym(input$intra_experiment_replicates)
    ggplot(data = boxPlotData(),aes(y = Signal, x = Experiment_Number, fill = !!(fill_choice))) +
        geom_boxplot() + 
        facet_wrap(~Probe, strip.position = "bottom", scales = "free_x")+ 
        theme(panel.spacing = unit(0, "lines"), 
              strip.background = element_blank(), 
              strip.placement = "outside")
  } else{
    ggplot(data = boxPlotData(),aes(y = Signal, x = Experiment_Number)) +
      geom_boxplot() + 
      facet_wrap(~Probe, strip.position = "bottom", scales = "free_x")+ 
      theme(panel.spacing = unit(0, "lines"), 
            strip.background = element_blank(), 
            strip.placement = "outside")
  }
})       
output$plotly_boxplot<-renderPlotly({
    plot_ly(boxPlotData(), x = ~Probe, y = ~Signal, color = ~Experiment_Number, type = "box",text = ~paste( Name, Signal)) %>%
        layout(boxmode = "group")
})

scatter_data<- reactive({
    req(length(input$choices_in_column)>1)
    temp<-as_tibble(boxPlotData())
    column_choice<-sym(input$column_selection)
    temp<-temp%>%select(c(input$column_selection,"Signal","Name"))%>% spread(!!(column_choice),Signal)
    })
scatter_data_selected<- reactive({
    temp<-scatter_data()
    temp<-temp[c(input$choices_in_column[1],input$choices_in_column[2])]
    temp
})
scatterplot<-reactive({
    req(length(input$choices_in_column)>1)
    x_choice<-sym(input$choices_in_column[1])
    y_choice<-sym(input$choices_in_column[2])
    ggplot(data = scatter_data(), aes(x = !!(x_choice), y = !!(y_choice),label = Name)) + geom_point()
})
output$scatter<-renderPlotly({

    ggplotly(scatterplot())
})
callModule(downloadFigure,"scatter",scatterplot)
output$PCC<-renderText({
    req(length(input$choices_in_column)>1)
    temp<-scatter_data_selected()
    browser()
    temp$Name<-NULL
    p<-cor(temp[1],temp[2],method = "pearson")
    paste("Correlation:", formatC(p, digits = 3, format = "f"))
})

output$downloadBoxplot<-downloadHandler(
    filename = "boxplot.png",
    
    content = function(file) {
        #write.csv(table_data(), file)
        if (input$boxplot_type == "Summary"){
            ggsave(file,summary_box_plot())
        }else{
        p<- plot_ly(boxPlotData(), x = ~Probe, y = ~Signal, color = ~Experiment_Number, type = "box",text = ~paste( Name, Signal)) %>%
            layout(boxmode = "group")
        orca(p, file = "plotly-boxplot.png")}
    }
)