generalized_data <- eventReactive(input$ok, {
    if (!is.null(imported_data()) ) {
        df<-imported_data()
        colnames(df)[(names(df) == input$probe_selection)] <- "Probe"
        colnames(df)[(names(df) == input$signal_selection)] <- "Signal"
        colnames(df)[(names(df) == input$background_selection)] <- "Background"
        colnames(df)[(names(df) == input$spot_id_selection)] <- "Name"
        colnames(df)[(names(df) == input$sequence_selection)] <- "Sequence"
        colnames(df)[(names(df) == input$array_selection)] <- "slide_template"
        colnames(df)[(names(df) == input$experiment_selection)] <- "Experiment_Number"
        colnames(df)[(names(df) == input$intra_experiment_replicates)] <- "slide_position"
        colnames(df)[(names(df) == input$row_location)] <- "Row"
        colnames(df)[(names(df) == input$column_location)] <- "Column"
        df$Signal<-as.numeric(df$Signal)
        df
    }
})
SH2_filter<-reactive({
    #browser()
    m<-generalized_data()
    temp<-m %>% filter(Probe %in% input$chosen_SH2s)
})
slide_filter<-reactive({
    #browser()
    req(input$array_selection)
    if(input$array_selection == "None"){
        SH2_filter()
      }else{
        temp<-SH2_filter() %>% filter(slide_template %in% input$chosen_templates)
      }
})

signal_as_ratio_background_foreground<-reactive({
  temp<-slide_filter()  
  if (input$signal_ratio == TRUE){

    #input$'modified-reactiveColDropdown'
    foreground<-sym(input$'foreground-reactiveColDropdown')
    background<-sym(input$'background-reactiveColDropdown')
    temp<-temp%>%
      as_tibble()%>%
      #group_by(dots =c("Experiment_Number","slide_template","slide_position",input$'unique-peptide-identifier-reactiveColDropdown')) %>% 
      mutate(Signal = (!!(foreground))/(!!(background)))
  }
  temp
})
signal_as_ratio_modified_unmodified<- reactive({
  
})
signal_as_ratio<-reactive({
  signal_as_ratio_background_foreground()
})


custom_filter<-reactive({
    #browser()
    temp<-signal_as_ratio()
    if(req(input$sort_by_column_selector) != "none"){
        if(req(input$sort_by_choice_selector) != "none"){
            column_var<-sym(input$sort_by_column_selector)
            temp<-as_tibble(slide_filter()) %>% filter(!!(column_var) %in% input$sort_by_choice_selector)  
        }
    }
    temp
})


transformation_for_skew<-reactive({

    temp<-custom_filter()
    if (input$right_skewed_transform == "Square Root"){
        temp %>% mutate(Signal = Signal^(1/2))
    } else if(input$right_skewed_transform == "Cube Root"){
        temp %>% mutate(Signal = Signal^(1/3))
    } else if(input$right_skewed_transform == "Log2"){
        validate(
            need(custom_filter() > 0, "Your data contains negative values which will result in imaginary numbers when log transformed.")
        )
        temp %>% mutate(Signal = log2(Signal))
    } else if(input$right_skewed_transform == "Log10"){
        temp %>% mutate(Signal = log10(Signal))
    }else if(input$right_skewed_transform == "none"){
        temp
    }
})

output$select_group_by <- renderUI({
    temps<-generalized_data()
    SH2_list<-c(levels(as.factor(temps$Probe)))
    selectInput("column_to_group_by","Select Column to Group By", choices=colnames(temps),multiple = T,selected = c("Name", "Probe"))
})
output$columns_keep <- renderUI({
  temps<-generalized_data()
  SH2_list<-c(levels(as.factor(temps$Probe)))
  column_name_choices<-c(colnames(temps),"SEM","SD","Samples","Z_Score","Average_Signal")
  selected_columns<-c("Sequence","Average_Signal","Z_Score","SEM","SD","Row","Column","Samples")
  selectInput("columns_to_keep","Select Columns to keep", choices=column_name_choices,multiple = T,selected = selected_columns)
})

get_avgs<-reactive({
    #aggregate(Signal~Measurement_spot_number + Name+Sequence+Probe+phosphorylation_state+slide_template,transformation_for_skew(),mean)
    #grouping_column = sym(input$column_to_group_by)
    temp<-as_tibble(transformation_for_skew())
    if (length(input$column_to_group_by)>0){
        if (length(input$column_to_group_by)>1){
            grouping_column = input$column_to_group_by
            temp<-temp %>% 
                group_by(.dots = grouping_column)%>% 
                summarise(SD = sd(Signal),SEM = sd(Signal)/sqrt(length(Signal)),Average_Signal= mean(Signal), Samples = n()) %>% 
                distinct() %>%
                ungroup()
        }else{
            grouping_column = sym(input$column_to_group_by)
            temp<-temp %>% 
                group_by(!!grouping_column)%>% 
                summarise(SD = sd(Signal),SEM = sd(Signal)/sqrt(length(Signal)),Average_Signal= mean(Signal), Samples = n())%>%
                mutate(Z_Score = scale(Average_Signal))%>%
                ungroup
        }
      #browser()
      temp<-temp %>% left_join(as_tibble(transformation_for_skew()), by= grouping_column)
      if ("slide_template" %in% colnames(temp)){
            temp<-temp %>% group_by(.dots = c("Probe","slide_template"))
      } else{
          temp <- temp %>% group_by(Probe)
      }
      temp <- temp %>%
          mutate(Z_Score = scale(Average_Signal))%>%
          ungroup()%>%
          select(c(input$columns_to_keep,input$column_to_group_by))  %>%
          distinct()

    }
    middle_columns<-intersect(c("Sequence","Average_Signal","Z_Score","SEM","SD","Samples"), input$columns_to_keep)
    last_columns<-setdiff(input$columns_to_keep,middle_columns)
    #browser()
    temp<-setcolorder(temp, c(input$column_to_group_by, middle_columns, last_columns))
    temp
})
get_avgs_spot<-reactive({
    aggregate(Signal~Measurement_spot_number+Sequence+Probe+phosphorylation_state+slide_template,transformation_for_skew(),mean)
})

filtered_data<- reactive({
    threshold_column<-sym(input$threshold_by)
    temp<-as_tibble(get_avgs())
    if (input$thresholding_type == "Percentage"){
        temp %>%  group_by(Probe) %>% filter(!!(threshold_column)>(input$threshold*max((!!(threshold_column))[!is.na(!!(threshold_column))& is.finite(!!(threshold_column))])/100)) %>% ungroup()-> temp
    }else if(input$thresholding_type == "Average"){
        get_avgs() %>% group_by(Probe) %>% filter(!!(threshold_column)>mean((!!(threshold_column))[!is.na(!!(threshold_column))& is.finite(!!(threshold_column))])) %>% ungroup()-> temp
    }else if(input$thresholding_type == "Multiple of non-phospho"){
        get_avgs() %>% filter(!!(threshold_column)>input$threshold)-> temp
    }else if (input$thresholding_type == "Z_Score"){
        get_avgs() %>%group_by(Probe)%>%filter(!!(threshold_column)>(input$'zthreshold-minMaxSlider'*1)) %>% ungroup()-> temp
    }else if(input$thresholding_type == "None"){
        get_avgs()-> temp
    }
})

data_sorted<-reactive({
    validate(
        need(nrow(filtered_data()) > 0, "There are no samples left in the dataset.")
    )
    filtered_data()
})