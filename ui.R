### Epilepsy tracking app in progress

library(shiny)

rows <- c("A", "B", "C", "D", "E", "F", "G", "H")


shinyUI(fluidPage(
  
  #Title Panel
  titlePanel(
    fluidRow(
      column(2,
             img(src = "SEIZR.jpg", height = 210, width = 211)),
      column(8,
             br(),
             br(),
             br(),
             br(),
             h3("Studying Epilepsy In Zebrafish using R"))
        )
    ),
  #sidebar
  sidebarLayout(
    sidebarPanel(
      
      #upload zebrabox spreadsheet before ptz
      fileInput("bef_file",
        label=h4("Select Zebrabox Spreadsheet Before PTZ"),
          multiple=FALSE),
      
      #upload zebrabox spreadsheet after ptz
      fileInput("aft_file",
                label=h4("Select Zebrabox Spreadsheet After PTZ"),
                multiple=FALSE),
      
      #Selecting minimum distance moved per well per run
      "Total distance each embryo must move during run to be included in analysis",
      numericInput("mindist", "distance (cm)",
                   value = 0.1, min = 0, max = 500, step = 0.1),

      #grouping rows together options
      selectizeInput('g1', 'Group 1', choices = rows, multiple = TRUE),
      selectizeInput('g2', 'Group 2', choices = rows, multiple = TRUE),
      selectizeInput('g3', 'Group 3', choices = rows, multiple = TRUE),
      selectizeInput('g4', 'Group 4', choices = rows, multiple = TRUE),
      checkboxInput("group",
                    label="Check After Selecting Groups",
                    value = FALSE)
    ),
    
    mainPanel(
      tabsetPanel(
        
        #Put total distance per well on first tab
        tabPanel("Total distance Per Well",
                 uiOutput("totdist"),
                 dataTableOutput(outputId="ds_table")),
        
        #Put Average and Standard Error on second tab
        tabPanel("PTZ Responses",
                 uiOutput("datasummary"),
                 dataTableOutput(outputId="data_table"),
                
                  # get rid of obnoxious table footers, not needed with my simple data tables
        tags$head(
          tags$style(type="text/css", "tfoot {display:none;}"))
                 )
        
      ) 
    )
  )
))