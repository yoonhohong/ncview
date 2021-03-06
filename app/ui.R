library(DT)
library(plotly)

# Define UI for data upload app ----
ui <- fluidPage(

  # App title ----
  titlePanel("NCView"),
  h4("Graphic representation of peripheral nerve conduction study data"),
  tags$hr(),
  # Sidebar layout with input and output definitions ----
  sidebarLayout(

    # Sidebar panel for inputs ----
    sidebarPanel(

      # Input: Select a Demo Data or user can select a file ----
      selectInput("demo", "Choose a Demo Dataset",
                  choices = c("CIDP" = "data/cidp_demo.rds")),
      h6("OR"), 
      fileInput("ncsFile", "Choose a Your Dataset (.csv)",
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
                 fluidRow(
                   column(12, plotOutput("cidp_tileView_motor"))
                 ))
      ) # tabsetPanel mainPanel
    ) # mainPanel
  ) # sidebarLayout
) # fluidPage
