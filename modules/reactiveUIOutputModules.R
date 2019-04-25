reactiveColumnDropdownUI<- function(id){
  ns <- NS(id)
  uiOutput(ns("reactive_Column_Dropdown"))
}

reactiveColumnDropdown <- function(input, output, session, label = "Select Column",  df, selected = "none" ) {
  output$reactive_Column_Dropdown <- renderUI({
    ns <-session$ns
    columnNames<-colnames(df())
    pickerInput(inputId = ns("reactiveColDropdown"), label, choices = append(columnNames,"none"), selected = selected)
  })
}