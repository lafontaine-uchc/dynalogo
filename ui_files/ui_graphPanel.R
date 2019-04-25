conditionalPanel(condition = "input.graph_toggle",
                 sidebarLayout(
                     sidebarPanel(
                         verticalLayout(
                             splitLayout(
                                 h4("Graph Panel"),
                                      pickerInput("graph_by","Select Y-axis Data", choices=c("Average_Signal","Z_Score"),
                                                  options = list(`actions-box` = TRUE),multiple = F,selected = "Average_Signal"),
                                      cellArgs = list (style = "overflow:visible")
                             ),
                             splitLayout(
                             radioButtons(inputId= "sorting", label = "Graph sorting", choices = c("none","ascending","descending")),
                             verticalLayout(reactiveColumnDropdownUI("xaxis"),
                                            checkboxInput("hide_x_axis", "Hide X Axis", value = FALSE)
                                            ),
                             cellArgs = list (style = "overflow:visible")
                             ),
                             
                                              sliderInput(inputId = "x_axis_font_size", label= "x-axis font size", min = 6, max = 40,value = 8),
                                              splitLayout(
                                                checkboxInput(inputId = "interactive_barchart_choice", label = "Interactive", value = FALSE),
                                                cellArgs = list (style = "overflow:visible")
                                              ),
                                                
                             
                             conditionalPanel(condition = "input.interactive_barchart_choice == false",
                                              downloadFigureUI("bargraph",filename = "bargraph",download_label = "Export")
                             )
                         )
                     ),
                     mainPanel(
                       conditionalPanel(condition = "input.interactive_barchart_choice",
                                        plotlyOutput(outputId = "histly")),
                       conditionalPanel(condition = "input.interactive_barchart_choice == false",
                                        plotOutput(outputId = "hist"))
                                                          
                     )
                 )
                 
)

