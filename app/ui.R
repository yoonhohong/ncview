library(DT)
library(plotly)

# Define UI for data upload app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("NCViewer"),
  h4("Graphic representation of peripheral nerve conduction"), 
  tags$hr(),
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Select a file ----
      fileInput("ncsFile", "Choose CSV File",
                multiple = TRUE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),
      tags$hr(),
      DTOutput(outputId = "ptTable")
    ), # sidebarPanel 
    
    # Main panel----
    mainPanel(
      tabsetPanel(
        tabPanel("Tile plot", 
                 fluidRow(
                   column(10, plotOutput("tileView_motor")),
                   column(8, plotOutput("tileView_sensory"))
                 )), 
        tabPanel("Radial plot", 
                 br(),
                 fluidRow(
                   column(10, plotlyOutput("paramView")),
                   column(10, plotlyOutput("nerveView"))
                 )),
        tabPanel("FU", 
                 br(), 
                 DTOutput(outputId = "dateTable"), 
                 br(),
                 plotlyOutput("fuView")), 
        tabPanel("CIDP",
                 br(),
                 plotlyOutput("cidp_tileView_motor"))
      ) # tabsetPanel mainPanel 
    ) # mainPanel
  ) # sidebarLayout
) # fluidPage
