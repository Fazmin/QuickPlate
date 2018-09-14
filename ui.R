library(shiny)
library(shinydashboard)

dashboardPage(skin = "black",
  dashboardHeader(title = "qPlate Analysis v.1.1"),
  dashboardSidebar(
    # sidebarSearchForm(textId = "searchText", buttonId = "searchButton",
    #                 label = "Search..."),
    sidebarMenu(id="tab",
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      # menuItem("Widgets", tabName = "widgets", icon = icon("th")),
      menuItem("Row/Col Based Plots", tabName = "confidence", icon = icon("dot-circle-o")),
      menuItem("Normalization", tabName = "normalize", icon = icon("th")),
      menuItem("Console Output", tabName = "console", icon = icon("laptop")),
      uiOutput("choose_dataset"),
      fileInput('file1', 'Choose file to upload',
                accept = c(
                  'text/csv',
                  'text/comma-separated-values',
                  'text/tab-separated-values',
                  'text/plain',
                  '.csv',
                  '.tsv',
                  '.txt',
                  '.log'
                )
      ),
      selectInput("pty", label = strong("Select plate type"), choices = list("96 Well Plate" = 96,"384 Well Plate"=384, "1536 Well Plate"=1536), selected =384),
      p('Load sample data set'),
      actionButton("loadsample", strong("Load sample data"), width = "203", style="margin: 0px 15px 0px 12px;"),
      br(),      
      actionButton("platedata_submitclick", strong("Process pasted data"), width = "203", style="margin: 12px 15px 0px 12px;"),
      selectInput("separator", label = strong("Input data format:"), choices = list("Comma deliminated" = 1, "Tab deliminated" = 2, "unformatted text" = 3), selected = 3),
      selectInput("platetheme", label = strong("Plate theme:"), choices = list("Raw late- light" = 1, "Raw plate - Dark" = 2, "Plate - Z-Map" = 3, "b-score : Median polish" = 4, "Hits - 1 SD" = 5, "Hits - Select theshold" = 6, "b-Score & select SD" = 7, "Other" = 8), selected = 1),
      conditionalPanel(
            condition = "input.platetheme == 6 || input.platetheme == 7",
            sliderInput("threshold", "Cutoff a theshold (SD):", 0.01, 8, 1)
            # textInput("threshold", "Cutoff a theshold", value = "Drug 2"),
      ),
      selectInput("palette", label = strong("Select Color Palette"), choices = list("RdYlGn" = "RdYlGn","YlOrBr" = "YlOrBr","RdYlBu" = "RdYlBu", "YlGnBu" = "YlGnBu","RdGy" = "RdGy","Spectral" = "Spectral"), selected = "Spectral")
    )
  ),
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    ),
    tabItems(
      # First tab content  | brush ="plot1_brush",
      tabItem(tabName = "dashboard",
        fluidRow(
          tabBox(
            title = "Data Input",
            # The id lets us use input$tabset1 on the server to find the current tab
            width=12,
            id = "Data Tabset",
            tabPanel("Pasted Data", 
              h4("Paste Data (comma/tab separated-values with headers) please setect type on sidebar"),
              tags$textarea(id="platedata_datainput", style="width:100%;height:200px;font-size:12px;"),
              br()
            ),
            tabPanel("Data to Table", 
              DT::dataTableOutput('platetable')
            )
          ),
          # box(
          #   title = "Data Input", width=8, status = "primary",
          #       h4("Paste Data (comma/tab separated-values with headers) please setect type on sidebar"),
          #         tags$textarea(id="platedata_datainput", style="width:100%;height:250px;font-size:11px;"),
          #         br()
          # ),
          
          box(title = "Plates", width=8, status = "primary", plotOutput("plotplate", height = 550,  click ="plot1_brush"
          )),
          # box(title = "Select Plates", background = "black", width=4, status = "primary", uiOutput("choose_dataset")),
          box(title = "Quick Stats", width=4, height=200, status = "primary", htmlOutput('platestats', height = 200)),
          box(title = "Hit Percent", width=4, plotOutput("percentplot", height = 350))
          
          
          # box(title = "Data Table", width=4, status = "primary", DT::dataTableOutput('platetable', height = 550)),
          # 
          # box(title = "Controls", width=6,
          #   sliderInput("slider", "Number of observations:", 1, 100, 50)
          # ),

        ),
        fluidRow(
          box(title = "Raw well values plot", width=4, plotOutput("plotdatapoints", height = 350)),
          box(title = "Sorted Data Plot", width=4, plotOutput("sorted_plot", height = 350)),
          box(title = "Data Frequencies", width=4, plotOutput("plotfeq", height = 350)),
          box(title = "Raw output - Main Page", width=12, height=370,verbatimTextOutput('rawdatamain'))
          

        )
      ),
      # Second tab content-
      # tabItem(tabName = "widgets",
      #   h2("Widgets tab content")
      # ),
      tabItem(tabName = "confidence",
         box(title = "Row Error Plot", width=12, status = "primary", plotOutput("confplot3", height = 350
          )),
         box(title = "Column Error Plot", width=12, status = "primary", plotOutput("confplot4", height = 350
          ))
         
      ),
      tabItem(tabName = "normalize",
        h2("Normalization tab content"),
        box(title = "Raw - Plot with Controls", width=6, status = "primary", plotOutput("confplot1", height = 400
          )),
        box(title = "Normalized data plot", width=6, status = "primary", plotOutput("confplot2", height = 400
          ))
      ),
      tabItem(tabName = "console",
        fluidRow(
        box(title = "Raw output - Other", width=4, height=370,verbatimTextOutput('rawdata1')),
        box(title = "Raw output - Data", width=4, height=370,verbatimTextOutput('rawdata2')),
        box(title = "Raw output - Console", width=4, height=370,verbatimTextOutput('rawdata3')),
        box(title = "Raw output - Test data", width=12, height=370,verbatimTextOutput('rawdata4'))
        )

      )

      
    )
  )
)