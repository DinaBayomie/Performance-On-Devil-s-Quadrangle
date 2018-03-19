shinyUI(pageWithSidebar(
  headerPanel("performance Measures"),
  
  sidebarPanel(
    #Selector for file upload
    fileInput(
      'datafile',
      'Choose CSV file'#,
      # accept = c('text/csv', 'text/comma-separated-values,text/plain')
    )#,
     # actionButton("btn_CalcET", "Calculate Effective Time")
  ),
  mainPanel(
    tabsetPanel(
      id = "tabSet",
      tabPanel(
        "Time",
        actionButton("btn_calcTimeMeasures", "Calculate Time Measures"),
        actionButton("btn_showAct", "Show Activities Effective Time"),
        actionButton("btn_CalcEf", "Calculate Efficiency"),
        tabsetPanel(
          id = "timeSet",
          tabPanel("Effective Working Hours",
                   verbatimTextOutput("BPsummary"),
                   plotOutput("ETWTPlot")),
          tabPanel("Activities",
                   verbatimTextOutput("ActivitiesSummary")),
          tabPanel(
            title = "Efficiency",
            # plotOutput("EF3TypesPlot"),
            
            verbatimTextOutput("BPSummary"),
            plotlyOutput("EF3TypesPloty"),
            plotlyOutput("EFAlloneTypesPloty")
            
            # ,
            # 
            # dataTableOutput("EffDataFrame")
          )
        )
      )
      ,
      tabPanel(
        "Cost",
        actionButton("btn_CalcRWH", "Calculate Resource Working Hours"),
        actionButton("btn_calcCost", "Calculate Detailed HR Cost"),
        actionButton("btn_calcDCost", "Calculate Direct Cost"),
        fileInput(
          'salaryfile',
          'Choose Salary CSV file',
          accept = c('text/csv', 'text/comma-separated-values,text/plain')
        ),
        plotlyOutput("ResourceCases"),
        tags$div(tags$br(),tags$br()),
        dataTableOutput("ResourceDataFrame")
      )
      ,
      
      tabPanel(
        "Flexibility ",
        tabsetPanel(
          id = "FlexibilitySet",
          tabPanel(
            "Workload",
            # actionButton("btn_calcWL", "Calculate Working Load "),
            # actionButton("btn_showInvRate", "Show Inventory Ins. rate overtime"),
            # actionButton("btn_showCumInvRate", "Show Inventory Cum. rate overtime"),
            actionButton(
              "btn_showCumInvRateInt",
              "Calculate Workload Flexibility"
            ),
            # actionButton("btn_showThroughputRate", "Show throughput rate overtime"),
            verbatimTextOutput("inventoryStatus"),
            plotOutput(
              "flexMesIntr",
              height = 300,
              # Equivalent to: click = clickOpts(id = "plot_click")
              #click = "plot1_click",
              brush = brushOpts(id = "plot1_brush")
            ),
            verbatimTextOutput("BfSummary"),
            # 
            plotOutput("flexibilityMeasures"),
            plotOutput("flexibilityMeasures2")
          ),
          tabPanel(
            "Behavior"
            ,
            actionButton("btn_BFlex", "Calculate Behavioral Flexibility"),
            verbatimTextOutput("BflexSummary"),
            # plotOutput("BflexGraph"),
            plotlyOutput("BflexGraph"),
            dataTableOutput("BehavioralFlex")
          )
        )
      )
      ,
      tabPanel("Quality",
               tabsetPanel(
                 id = "qualitySet",
                 tabPanel(
                   "Traces",
                   actionButton("btn_HB", "Show Traces"),
                   actionButton("btn_CHB", "calculate success Percentage"),
                   actionButton("btn_BPE", "calculate Business Error Percentage"),
                   verbatimTextOutput('QDescription'),
                   DT::dataTableOutput("tracess")
                 ),
                 tabPanel(
                   "Activities",
                   actionButton("btn_Act", "Show Activities"),
                   actionButton("btn_ACHB", "calculate success Percentage"),
                   actionButton("btn_ABPE", "calculate Business Error Percentage"),
                   verbatimTextOutput('QADescription'),
                   uiOutput("moreControls")
                   # selectInput("ActivitiesList"))
                   # DT::dataTableOutput("ActivitiesDT")))
                   
                   # # actionButton("btn_calcWL", "Calculate Working Load "),
                   # verbatimTextOutput("BfSummary"),
                   # dataTableOutput("BehavioralFlex")
                 )
               )),
      tabPanel("Devil's Quandrangle",
               sidebarPanel(
                 actionButton("btn_drawDevils","Draw the devil's quandrangle"),
                 textInput(inputId="oldTime",label =  "Old time measure"),
                 textInput(inputId="oldCost",label =  "Old cost measure"),
                 textInput(inputId="oldQuality",label =  "Old quality measure"),
                 textInput(inputId="oldFlex",label =  "Old flexibility measure"),
                 # textInput(inputId="WeightFTE",label =  "Weight of flow time efficiency"),
                 # textInput(inputId="WeightEFR",label =  "Weight of efficiency flow rate"),
                 textInput(inputId="EstimatedCost",label =  "Estimated Cost"),
                 textInput(inputId="weightHP",label =  "Weight of successful traces"),
                 textInput(inputId="weightBE",label =  "Weight of business error"),
                 textInput(inputId="weightBF",label =  "Weight of behavior flexibility"), 
                 textInput(inputId="weightWF",label =  "Weight of workload flexibility")
              ),
               mainPanel(
            # textOutput("DevilQAloneLabel"),
            #       plotOutput("DevilQAlone"),
            textOutput("DevilQLabel"),
                 plotOutput("DevilQ")
               )
               
               )
    )
    
  )
))
