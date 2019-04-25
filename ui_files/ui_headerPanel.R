absolutePanel(id = "headerbar", style="z-index:100000000000;",
              wellPanel(
                  splitLayout(
                      #titlePanel("Dynalogo"),
                      #column(3, p(strong("Dynalogo")),
                      column(3, h4("Dynalogo")#, actionButton("show", "Import Data")),
                      ),
                      actionButton("show2", "Import Data"),
                      checkboxInput(inputId = "distribution_display", label = "Distribution Panel", FALSE),
                      checkboxInput(inputId = "logo_plot_toggle",label= "Logo plot", TRUE),
                      checkboxInput(inputId = "graph_toggle",label= "Graph", FALSE),
                      checkboxInput(inputId = "digital_array",label= "Digital Binding",FALSE)
                  )
                  #column(12,align = "center",titlePanel("Dynalogo"))
              ),
              fixed = TRUE, left = 0, right = 0,top = 0,height = 100
)