conditionalPanel(condition = "input.logo_plot_toggle",
                 sidebarLayout(
                     sidebarPanel(
                         h4("Dynamic Logo Plot"),
                         verticalLayout(
                             numericInput(inputId = "sequence_length", labe = "Sequence Length",min = 5, max = 21, value = 13, step = 1),
                             downloadFigureUI("logo",filename = "logoplot",download_label = "Export")
                         )
                     ),
                     mainPanel(
                         imageOutput(outputId = "test_logo")
                     )
                 )
                 
                 
                 
)