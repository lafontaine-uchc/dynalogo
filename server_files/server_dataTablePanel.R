table_data<-reactive({
    temp_data<-filtered_data()
    #temp_data$new_sequence<-gsub("-","",as.character(temp_data$Sequence))
    temp_data$Sequence<-gsub("pY","y",temp_data$Sequence)
    colnames(temp_data)[(names(temp_data) == "Probe")] <- input$probe_selection
    colnames(temp_data)[(names(temp_data) == "Average_Signal")] <- input$signal_selection
    colnames(temp_data)[(names(temp_data) == "Name")] <- input$spot_id_selection
    colnames(temp_data)[(names(temp_data) == "Sequence")] <- input$sequence_selection
    #colnames(temp_data)[(names(temp_data) == "Sequence")] <- input$sequence_selection
    colnames(temp_data)[(names(temp_data) == "slide_template")] <- input$array_selection
    #colnames(temp_data)[(names(temp_data) == "Experiment Number")] <- input$experiment_selection        
    temp_data
})


display_data<-reactive({
    if (input$table_display_selector == "Graph Data"){
        graph_data()
    }else if(input$table_display_selector == "Logo Data"){
        logo_data()
    }else if (input$table_display_selector == "Summarized Data"){
        get_avgs()
    }else if (input$table_display_selector == "Imported Data"){
        generalized_data()
    }else if (input$table_display_selector == "Table Data"){
        table_data()
    }else if (input$table_display_selector == "filtered_data()"){
        filtered_data()
    }else if (input$table_display_selector == "Digital Binding"){
        digital_array_data()
    }else if (input$table_display_selector == "Boxplot"){
      boxPlotData()
    }else if (input$table_display_selector == "Scatterplot"){
      boxPlotData()
    }else if (input$table_display_selector == "Histogram"){
      get_avgs()
    }else if (input$table_display_selector == "Logo Background Data"){
        background_data()
    }
})

output$sequences_text<-renderDataTable(display_data(), filter = "top",
                                       options = list(search = list(regex = TRUE),pageLength = 50)
                                       )

output$downloadData<-downloadHandler(
    filename = function(){input$downloadFileName},
    content = function(file) {
        write_csv(as_tibble(display_data()), file)
    }
)