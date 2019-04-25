MinMaxColumnSliderUI<- function(id, label = "missing label"){
  ns <- NS(id)
  
  uiOutput(ns("min_max_slider"))
}

MinMaxColumnSlider <- function(input, output, session, label, data, colname,  value = 0) {
  output$min_max_slider <- renderUI({
    ns <-session$ns
    colname <- sym(colname)
    data_column = data()%>%select(!!(colname))
    zmin<-round(min(data_column), 2)
    zmax<-round(max(data_column), 2)
    sliderInput(inputId = ns("minMaxSlider"), label = label, 
                min = zmin,max = zmax,value =value,
                animate = TRUE)
  })
}
