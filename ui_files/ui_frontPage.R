tagList(
wellPanel(
  splitLayout(
    column(3, h4("Dynalogo")),
    actionButton("show", "Import Dataset"),
    downloadLink("manual_download", label = "Download Manual"),
    downloadLink("demo_data_download", label = "Download Demo Data"),
    downloadLink("version_info", "Version Info")
  )
),
p("Please import your dataset or dowlnoad the sample dataset"),
fluidRow(id ="Description",
         column(1),
         column(10, align="left",
                h2("Dynalogo: Dynamic Sequence Logos"),
                h4("Dynalogo is a web application which can be used to visualize and explore 
                  binding array (ie. peptide array) datasets. It was created to allow the dynamic
                  visualizations of sequence logos for better insight into the relationship between
                  affinity and specificity of binding domains. "),
                br(),
                h4("Once imported, the data can be visualized in several different ways:"),
                br(),
                tags$ul(
                  tags$li("Dynamic Logoplot- a reactive implementation of ggseqlogo"), 
                  tags$li("Graph – binding signal vs binding site/sequence"), 
                  tags$li("Distribution – binding signal visualized as boxplots, scatterplots or histograms"),
                  tags$li("Digital Binding Array – recreation of experimental layout of data")
                )
         )
),
fluidRow(
  id = "Description2",
  column(12, align="center",
         imageOutput("logoGif"),
         imageOutput("splashImage")
  )
)
)