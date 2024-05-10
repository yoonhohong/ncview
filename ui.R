library(DT)
library(plotly)

# Define UI for data upload app ----
ui <- fluidPage(

  # App title ----
  titlePanel("NCView"),
  h4("A graphical representation and analytic system for nerve conduction study"),
  tags$hr(),
  # Sidebar layout with input and output definitions ----
  sidebarLayout(

    # Sidebar panel for inputs ----
    sidebarPanel(

      # Input: Select a Demo Data or user can select a file ----
      selectInput("demo", "Choose a demo dataset",
                  choices = c("CIDP" = "data/cidp_demo.rds", 
                              "GBS" = "data/gbs_demo.rds", 
                              "CMT" = "data/cmt_demo.rds")),
      h6("OR"), 
      fileInput("ncsFile", "Upload your dataset (.csv)",
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
                 br(),
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
        tabPanel("Diagnosis",
                 br(),
                 fluidRow(
                   column(12, plotOutput("cidp_tileView_motor"))
                 ),
                 br(),
                 strong("2021 EFNS/PNS criteria"), 
                 div("At least one of the following:"),
                 div("* DML >= 150% in two nerves"), 
                 div("* NCV <= 70% in two nerves"), 
                 div("* FL >= 120% (150% if CMAP1 < 80%) in two nerves"), 
                 div("* FA (w/ CMAP1 >= 20%) in two nerves, plus >= 1 other parameters in > 1 other nerve"), 
                 div("* CB >= 0.3 (w/ CMAP >= 20%; excluding tibial nerve) in two nerves, or 1 nerve plus >= 1 other parameters (except FA) in >= 1 other nerve"), 
                 div("* TD >= 1.3 (2 in tibial nerve) in two nerves"), 
                 div("* DUR > 100 in 1 nerve, plus >= 1 other parameters in >= 1 other nerve"))
      ) # tabsetPanel mainPanel
    ) # mainPanel
  ) # sidebarLayout
) # fluidPage
