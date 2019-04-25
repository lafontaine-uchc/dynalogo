conditionalPanel(condition = "input.digital_array",
                 h4("Digital Array"),
                 splitLayout(
                     uiOutput("array_sh2_slider"),
                     uiOutput("array_group_slider"),
                     reactiveColumnDropdownUI("array_tooltip"),
                     cellArgs = list (style = "overflow:visible")
                 ),
                 plotlyOutput("ploty")
)