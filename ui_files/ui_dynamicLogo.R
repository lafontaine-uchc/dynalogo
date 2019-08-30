conditionalPanel(condition = "input.logo_plot_toggle",
                 sidebarLayout(
                     sidebarPanel(
                         h4("Dynamic Logo Plot"),
                         verticalLayout(
                             splitLayout(
                                #numericInput(inputId = "sequence_length", label = "Sequence Length",min = 5, max = 21, value = 13, step = 1),
                                radioButtons(inputId = "logo_type", label = "Logo Type:",  choices = c("EDLOGO", "ggseqlogo","Logo","KLLogo"),selected = "ggseqlogo"),
                                checkboxInput(inputId = "custom_background", label = "Custom Background:", value = FALSE)
                                ),
                             downloadFigureUI("logo",filename = "logoplot",download_label = "Export")
                         )
                     ),
                     mainPanel(
                         imageOutput(outputId = "test_logo")
                     )
                 )
                 
                 
                 
)