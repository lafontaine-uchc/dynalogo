wellPanel(h4("Filtering & Transformation"),
          splitLayout(
              verticalLayout(
                  uiOutput("select_SH2"),
                  conditionalPanel(condition = "input.array_selection != 'None'",
                                   uiOutput("template_selector")
                  )
              ),
              # checkboxGroupInput(inputId = "chosen_templates",choices = c("A","B"),label = "Select Slide Templates", inline = T, selected = "A"),
              #Controls for Data processing
              
              verticalLayout(
                  uiOutput("split_by"),
                  uiOutput("split_by_selector")
                  
              ),
              verticalLayout(
                  checkboxInput("signal_ratio","Signal as Ratio?", value = FALSE),
                  reactiveColumnDropdownUI("foreground"),
                  reactiveColumnDropdownUI("background")
                  #reactiveColumnDropdownUI("unique-peptide-identifier")
              ),
              radioButtons(inputId = "right_skewed_transform",label = "Transform Data?",choices = c("Log2","none"),selected = "none"),
              cellArgs = list (style = "overflow:visible")
              
          )
)