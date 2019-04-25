wellPanel(h4("Data Summation/Thresholding"),
          splitLayout(
              #Select column to group by
              uiOutput("select_group_by"),
              
              #select columns to keep but not group by
              uiOutput("columns_keep"),
              cellArgs = list (style = "overflow:visible")
              #
          ),
          
          
          
          
          #tags$hr(), 
          #Threshold slider
          
          splitLayout(
              pickerInput("threshold_by","Threshold by: ", choices=c("Average_Signal","Z_Score"),
                          options = list(`actions-box` = TRUE),multiple = F,selected = "Z_Score"),
              uiOutput("type_of_threshold"),  
              uiOutput("thresholding_slider"),
              cellArgs = list (style = "overflow:visible")
          )
)