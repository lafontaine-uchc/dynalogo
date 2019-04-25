conditionalPanel(condition = "input.distribution_display",
                 tags$hr(),
                 sidebarLayout(
                   sidebarPanel(
                       h4("Distribution Panel"),
                     verticalLayout(
                       radioButtons(inputId = "distribution_chart","Chart Type", choices = c("Boxplot","Histogram","Scatterplot"),selected = "Boxplot"),
                       conditionalPanel(condition = "input.distribution_chart == 'Histogram'",
                        sliderInput(inputId = "bins",label = "Bins",value = 30,min = 1,max =60,step = 1),
                        downloadFigureUI("histogram",filename = "histogram",download_label = "Export")
                       ),
                       conditionalPanel(condition = "input.distribution_chart == 'Boxplot'",
                                        radioButtons(inputId = "boxplot_type","Boxplot Type", choices = c("Interactive","Summary"),selected = "Boxplot"),
                                        downloadButton("downloadBoxplot", "Download Boxplot")
                       ),
                       conditionalPanel(condition = "input.distribution_chart == 'Scatterplot'",
                                        uiOutput("column_selection_ui"),
                                        uiOutput("choices_in_column_ui"),
                                        textOutput("PCC"),
                       downloadFigureUI("scatter",filename = "scatterplot",download_label = "Export")
                       )
                     )
                   ),
                   mainPanel(
                     conditionalPanel(condition = "input.distribution_chart == 'Histogram'",
                                      plotOutput(outputId = "hist2")
                     ),
                     conditionalPanel(condition = "input.distribution_chart == 'Scatterplot'",
                                      plotlyOutput(outputId = "scatter")
                     ),
                     conditionalPanel(condition = "input.distribution_chart == 'Boxplot'",
                                      uiOutput("boxplotUI")
                     )
                     #plotOutput(outputId = "hist2")                                 
                   )
                 ),

                 tags$hr() 
)
