output$thresholding_slider <- renderUI({
    if(input$thresholding_type == "Percentage"){
        sliderInput(inputId = "threshold", label ="Threshold_level(% of max)", 
                    min = 0,max = 100,value =0,
                    animate = animationOptions(interval = 1000, loop = TRUE,
                                               playButton = "Play", pauseButton = "Pause"))
    } else{
        MinMaxColumnSliderUI("zthreshold")
    }
})
callModule(MinMaxColumnSlider,
           "zthreshold",
           "Z-Score Threshold",
           get_avgs,
           "Z_Score"
)
output$type_of_threshold <- renderUI({
    if(input$threshold_by == "Z_Score"){
        choices<-c("Z_Score", "None")
        selected <- "Z_Score"
    } else{
        choices <- c("Average","Percentage", "None")
        selected <- "Percentage"
    }
    radioButtons(inputId = "thresholding_type",label = "Choose type of thresholding",
                 choices = choices,
                 selected = selected) 
})