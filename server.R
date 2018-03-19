options(shiny.maxRequestSize = 200000 * 1024 ^ 2)

require(XML)
require(methods)
require(XML)
require(xml2)
require(methods)
require(plyr)
require(datasets)
require(ggplot2)
require(reshape2)
require(plotly)
require(DT)
require(scales)
require(data.table)
require(ineq)
require(shinyjs)

require(foreach)
require(doSNOW)
require (doParallel)
require(compiler)

shinyServer(function(input, output, session) {
  
  totalCost <-0
  happyPathPercentage <- 0
  errorPathPercentage <- 0
  behavioralFlexValue <- 0
  workloadFlexValue <- 0
  timedimensionValue <- 0
  #This function is repsonsible for loading in the selected file
  traces <- reactive({
    infile <- input$datafile
    if (is.null(infile)) {
      # User has not uploaded a file yet
      return(NULL)
    }
    log <- read.csv(file = infile$datapath,
                    header = TRUE,
                    sep = ",")
    # print("in reactive log")
    split(log , f = log$case)
  })
  dataframe.TracesCases <- reactive({
    infile <- input$datafile
    if (is.null(infile)) {
      # User has not uploaded a file yet
      return(NULL)
    }
     print("in reactive")
    BuildTraceCasesMatrix(traces())
  })
  dataframe.CAFTDRT <- reactive({
    print("loading the file")
    infile <- input$datafile
    if (is.null(infile)) {
      # User has not uploaded a file yet
      return(NULL)
    }
    # print("in reactive")
    print("Start building the CAFTRDelta matrix")
    BuildCAFTRDeltaMatrixCom<-cmpfun(BuildCAFTRDeltaMatrix)
    
    BuildCAFTRDeltaMatrixCom(traces(), unit = "hours")
  })
  
  dataframe.Efficiency <- reactive({
    print("start building the efficiency data frame")
    View(dataframe.CAFTDRT())
    casesET <-
      CalculateTperCase(dataframe.CAFTDRT = dataframe.CAFTDRT())
    casesWT <-
      CalculateTperCase(dataframe.CAFTDRT = dataframe.CAFTDRT(), type = "Waiting")
    case.dataframe <- getDataframeOfCasesETime(casesET)
    # #View(dataframe.TracesCases())
    caseCT <- vector.FlowTime()#CalculateCycleTime(traces())
    View(case.dataframe)
    View(casesWT)
    View(dataframe.TracesCases())
    tracesMins.dataframe <-
      GetMinETforAllTraces(tracescases = dataframe.TracesCases(), case.Dataframe = case.dataframe)
    
    View(tracesMins.dataframe)
    minETvalue <- GetMinETforBP(tracesMins.dataframe)
    View(minETvalue)
    movingTimeDF <- BuildmovingtimeDF(traces(), unit = "hours")
    
    
    print("Calling the efficiency building function")
    efficiency.dataframe <-
      BuildingefficiencyCaseFrame(
        caseWTList = casesWT,
        CaseMTDF = movingTimeDF,
        caseCTList = caseCT,
        case.Dataframe = case.dataframe,
        minETvalue = minETvalue,
        tracescases = tracesMins.dataframe
      )
  })
  
  dataframe.Flexibility <- reactive({
    # print("start building the flex data frame")
    BuildFlowDFCmp <- cmpfun(BuildFlowDF)
    UpdateFlowDFCmp <- cmpfun(UpdateFlowDF)
    SetInventoryStatusCmp <- cmpfun(SetInventoryStatus)
    
    flexibility.Dataframe <- BuildFlowDFCmp(traces())
    #View(flexibility.Dataframe)
    flexibility.Dataframe <- UpdateFlowDFCmp(flexibility.Dataframe)
    avgInventoryLevel = mean(flexibility.Dataframe$cuminventoryRateIns)
    # avgInventoryLevel = mean(flexibility.Dataframe$inventory)
    # print(paste("avgInventoryLevel : " ,avgInventoryLevel))
    flexibility.Dataframe <-
      SetInventoryStatusCmp(flexibility.Dataframe, avgInventoryLevel)
    
  })
  dataframe.BehavioralFlexibility <- reactive({
    totalNCases <- length(traces())
    BuildBehavioralFlexibilityDF(dataframe.TracesCases(), totalNCases)
  })
  vector.FlowTime <-
    reactive({
      CalculateCycleTime(traces(), unit = "hours")
    })
  ## handle time####
  
  observeEvent(input$btn_calcTimeMeasures, {
    # dataframe.CAFTDRT <- BuildCAFTRDeltaMatrix(traces())
    # print("in observe")
    
    View(dataframe.CAFTDRT())
    # print("get dataframe")
    actSummaries <-
      CalculateTperActivity(dataframe.CAFTDRT = dataframe.CAFTDRT())
    casesET <-
      CalculateTperCase(dataframe.CAFTDRT = dataframe.CAFTDRT())
    casesWT <-
      CalculateTperCase(dataframe.CAFTDRT = dataframe.CAFTDRT(), type = "Waiting")
    casesETWT.dataframe <-
      getDataframeOfCasesETWTime(caseETList = casesET, caseWTList = casesWT)
    casesETWT.dataframe.melt <-
      melt(casesETWT.dataframe ,
           id.vars = 'Case',
           variable.name = 'Times')
    output$BPsummary <-
      renderPrint({
        CalculateTofBPbasedonActivitiesSummary(actSummaries)
      })
    output$ETWTPlot <- renderPlot({
      ggplot(casesETWT.dataframe.melt, aes(Case, value)) + geom_point(aes(colour =
                                                                            Times))
      # qplot(casesETWT.dataframe$Case,
      #       as.numeric(casesETWT.dataframe$WaitingTime),
      #       xlab = "Cases",
      #       ylab = "Effective time")
    })
    
  })
  
  observeEvent(input$btn_showAct, {
    actSummaries <-
      CalculateTperActivity(dataframe.CAFTDRT = dataframe.CAFTDRT())
    output$ActivitiesSummary <-
      renderPrint({
        actSummaries
      })
  })
  
  observeEvent(input$btn_CalcEf, {
    # casesET <-
    #   CalculateTperCase(dataframe.CAFTDRT = dataframe.CAFTDRT())
    # casesWT <-
    #   CalculateTperCase(dataframe.CAFTDRT = dataframe.CAFTDRT(), type = "Waiting")
    # case.dataframe <- getDataframeOfCasesETime(casesET)
    # # #View(dataframe.TracesCases())
    # caseCT <- vector.FlowTime()#CalculateCycleTime(traces())
    #
    #
    # tracesMins.dataframe <-
    #   GetMinETforAllTraces(tracescases = dataframe.TracesCases(), case.Dataframe = case.dataframe)
    # minETvalue <- GetMinETforBP(tracesMins.dataframe)
    #
    #
    #
    # efficiency.dataframe <-
    #   BuildingefficiencyCaseFrame(
    #     caseWTList = casesWT,
    #     caseCTList = caseCT,
    #     case.Dataframe = case.dataframe,
    #     minETvalue = minETvalue,
    #     tracescases = tracesMins.dataframe
    #   )
    #
    
    print("Handle the efficiency button")
    timeUnit = "hours"
    View(dataframe.CAFTDRT())
    efficiency.dataframe <- dataframe.Efficiency()
    timedimensionValue <<- mean(efficiency.dataframe$EfficiencyLeadTime,na.rm = TRUE)
    # View(efficiency.dataframe)
    efficiency.dataframe$CumulativeTime <-efficiency.dataframe$EffectiveTime
    efficiency.dataframe$FlowTime <- efficiency.dataframe$LeadTime
    # movingTimeDF <- BuildmovingtimeDF(traces(), unit = "mins")
    #View(movingTimeDF)
    # #View(efficiency.dataframe)
    updateTabsetPanel(session, "tabSet", selected = "Effective Time")
    # drops <- c("EffectiveTime", "WaitingTime", "LeadTime")
    drops <-
      c(
        "EffectiveTime",
        "LeadTime",
        "EfficiencyMinET",
        "EfficiencyTrace"
      )
    
    #View(efficiency.dataframe)
    # efficiency.dataframe<- UpdateWaitingTime(efficiency.dataframe,movingTimeDF)
    #View(efficiency.dataframe)
    efficiency.values.df <-
      efficiency.dataframe[,!(names(efficiency.dataframe) %in% drops)]
    View(efficiency.dataframe)
    # efficiency.values.df$WaitingTime <- movingTimeDF$Delta
    # efficiency.dataframe.melt <- melt(efficiency.values.df,
    #                                   id.vars = 'Case',
    #                                   variable.name = 'Efficiency')
    # #View(efficiency.dataframe.melt)
    # output$EF3TypesPlot <- renderPlot({
    #   ggplot(efficiency.dataframe.melt, aes(Case, value)) + geom_point(aes(colour =
    #                                                                          Efficiency))
    #   # qplot(casesETWT.dataframe$Case,
    #   #       as.numeric(casesETWT.dataframe$WaitingTime),
    #   #       xlab = "Cases",
    #   #       ylab = "Effective time")
    # })
    
    output$EF3TypesPloty <-
      renderPlotly({
        plot_ly(
          efficiency.values.df,
          x = ~ as.character(Case),
          y = ~ as.numeric(FlowTime),
          size = ~ FlowTime,
          type = "scatter",
          mode = "markers",
          name = "Flow time",
          marker = list(
            size = 5,
            color= "green",
            line = list(color = 'black', width = 1)
          )
        ) %>% layout(margin = list(b = 160),
          xaxis = list(title = "Cases", tickangle = 45),
          yaxis = list(title = "Duration")
        )%>% add_trace(
          x = ~as.character(Case), 
          y = ~as.numeric(CumulativeTime),
          name = "Cumulative time",
          type = 'scatter',
          size = ~ CumulativeTime,
          mode = "markers",
          marker = list(
            size = 5,
            color= "blue",
            line = list(color = 'black', width = 1)
            )
          )%>% add_trace(
            x = ~as.character(Case), 
            y = ~as.numeric(WaitingTime),
            name = "Waiting time",
            type = 'scatter',
            size = ~ WaitingTime,
            mode = "markers",
            marker = list(
              size = 5,
              color= "red",
              line = list(color = 'black', width = 1)
            )
          )
        
      })
   
    
     output$EFAlloneTypesPloty <-
      renderPlotly({
        plot_ly(
          efficiency.values.df,
          x = ~ as.character(Case),
          y = ~ as.numeric(EfficiencyLeadTime),
          size = ~ EfficiencyLeadTime,
          type = "scatter",
          mode = "markers",
          name = "Efficiency",
          marker = list(
            size = 5,
            color= "green",
            line = list(color = 'black', width = 1)
          )
        ) %>% layout(margin = list(b = 160),
                     xaxis = list(title = "Cases", tickangle = 45),
                     yaxis = list(title = "Efficiency",tickformat = "%")
        )
        
      })
    # %>% add_trace(
    #   x = ~as.character(Case), 
    #   y = ~as.numeric(EfficiencyLeadTime),
    #   name = "Efficiency w.r.t flow time",
    #   type = 'scatter',
    #   size = ~ EfficiencyLeadTime,
    #   mode = "markers",
    #   marker = list(
    #     size = 5,
    #     color= "yellow",
    #     line = list(color = 'black', width = 1)
    #   )
    # )
      # renderPlot({
      # ggplot(resource.dataframe, aes(x=as.character(Resource),y=nCases)) + geom_point()+theme(axis.text.x = element_text(angle = 90, hjust = 1))
      # renderPlotly({
      #   plot_ly(
      #     efficiency.dataframe,
      #     x = ~ as.character(Case),
      #     y = ~ FlowTime,
      #     name = "Flow time",
      #     type = "scatter",
      #     mode = "markers",
      #     marker = list(
      #       size = 10,
      #       color= "purple",
      #       line = list(color = 'black', width = 2)
      #     )
      #   )
        # %>% layout(
        #   xaxis = list(title = "Cases", tickangle = 90),
        #   yaxis = list(title = "Time"+timeUnit)
        # )
        # %>% add_trace(x = ~as.character(Case), y = ~CumulativeTime, name = "Cumulative time",type = 'scatter',
        #             mode = "markers", marker = list(
        #               size = 10,
        #               color= "blue",
        #               line = list(color = 'black', width = 2)
        #             )
        #   )
        # %>% add_trace(x = ~as.character(Case), y = ~WaitingTime, name = "Waiting time",type = 'scatter',
        #             mode = "markers", marker = list(
        #               size = 10,
        #               color= "red",
        #               line = list(color = 'black', width = 2)
        #             )
        # )%>% add_trace(x = ~as.character(Case), y = ~EfficiencyLeadTime, name = "Efficiency",type = 'scatter',
        #               mode = "markers", marker = list(
        #                 size = 10,
        #                 color= "yellow",
        #                 line = list(color = 'black', width = 2)
        #               )
        #   )
      # })
    
    updateTabsetPanel(session, "timeSet", selected = "Efficiency")
    
    output$BPSummary <- renderPrint({
      summary(efficiency.values.df[,!(names(efficiency.values.df) %in% c("Case"))])
    })
    
    
  })
  
  ## handle Cost####
  
  salary.dataframe <- reactive({
    infile <- input$salaryfile
    if (is.null(infile)) {
      # User has not uploaded a file yet
      return(NULL)
    }
    read.csv(file = infile$datapath,
             header = TRUE,
             sep = ",")
  })
  
  observeEvent(input$btn_CalcRWH, {
    # resource.dataframe <-
    #   CalculateWHperResourceInCase(dataframe.CAFTDRT())
    resource.dataframe <-
      CalculateWTperResourceInBP(dataframe.CAFTDRT())
    #View(resource.dataframe)
    if (is.null(salary.dataframe())) {
      # resource.dataframe <-
      #   CalculateWTperResourceInBP(dataframe.CAFTDRT())
      #
      #avg<-mean(as.numeric(ctMatrix[,2]))
      output$ResourceDataFrame <-
        renderDataTable({
          resource.dataframe
        })
    }
    else{
      resource.dataframe.update <-
        CalculateResourceCost(resource.dataframe = resource.dataframe,
                              salary.dataframe = salary.dataframe())
      #avg<-mean(as.numeric(ctMatrix[,2]))
     
      output$ResourceDataFrame <-
        renderDataTable({
          resource.dataframe.update
        })
    }
    output$ResourceCases <-
      # renderPlot({
      # ggplot(resource.dataframe, aes(x=as.character(Resource),y=nCases)) + geom_point()+theme(axis.text.x = element_text(angle = 90, hjust = 1))
      renderPlotly({
        plot_ly(
          resource.dataframe,
          x = ~ as.character(Resource),
          y = ~ nCases,
          color = ~ nCases,
          size = ~ nCases,
          type = "scatter",
          mode = "markers",
          marker = list(
            size = 10,
            line = list(color = 'black', width = 2)
          )
        ) %>% layout(margin = list(b = 160),
          xaxis = list(title = "Resources", tickangle = 45),
          yaxis = list(title = "Number of cases")
        )
      })
    updateTabsetPanel(session, "tabSet", selected = "Cost Measurements")
  })
  
  observeEvent(input$btn_calcCost,  {
    infile <- input$salaryfile
    if (is.null(infile)) {
      # User has not uploaded a file yet
      # showNotification("Can't calculate the actual HR cost", type = "error")
      resource.dataframe <-
        CalculateWHperResourceInCase(dataframe.CAFTDRT())
      caseCostHR<-groupUpHRCaseCost(resource.dataframe.update)
      View(caseCostHR)
      output$ResourceDataFrame <-
        renderDataTable({
          resource.dataframe
        })
    }
    else{
      salary.dataframe <- read.csv(file = infile$datapath,
                                   header = TRUE,
                                   sep = ",")
      
      resource.dataframe <-
        CalculateWHperResourceInCase(dataframe.CAFTDRT())
      
      resource.dataframe.update <-
        CalculateResourceCost(resource.dataframe = resource.dataframe,
                              salary.dataframe = salary.dataframe)
      newCaseDF<- groupUpHRCaseCost(resource.dataframe.update)
      View(newCaseDF)
      output$ResourceCases <-
        # renderPlot({
        # ggplot(resource.dataframe, aes(x=as.character(Resource),y=nCases)) + geom_point()+theme(axis.text.x = element_text(angle = 90, hjust = 1))
        renderPlotly({
          plot_ly(
            newCaseDF,
            x = ~ as.character(Case),
            y = ~ Cost,
            color = ~ Cost,
            size = ~ Cost,
            type = "scatter",
            mode = "markers",
            marker = list(
              size = 10,
              line = list(color = 'black', width = 2)
            )
          ) %>% layout(margin = list(b = 160),
                       xaxis = list(title = "Cases", tickangle = 45),
                       yaxis = list(title = "Cost")
          )
        })
      output$ResourceDataFrame <-
        renderDataTable({
          newCaseDF
        })
      
    }
  })
  
  
  observeEvent(input$btn_calcDCost, {
    directCost <- CalculateDirectCost(traces())
    if (is.null(directCost)) {
      showNotification("Can't calculate the direct cost", type = "error")
      return(NULL)
    }
    else
    { 
      directCost.dataframe <-
        getDataframeOfDirectCost(directCostList = directCost)
      View(directCost.dataframe)
      totalCost<<-sum(directCost.dataframe$Cost,na.rm = TRUE)
      #avg<-mean(as.numeric(ctMatrix[,2]))
      output$ResourceCases <-
        # renderPlot({
        # ggplot(resource.dataframe, aes(x=as.character(Resource),y=nCases)) + geom_point()+theme(axis.text.x = element_text(angle = 90, hjust = 1))
        renderPlotly({
          plot_ly(
            directCost.dataframe,
            x = ~ as.character(Case),
            y = ~ Cost,
            color = ~ Cost,
            size = ~ Cost,
            type = "scatter",
            mode = "markers",
            marker = list(
              size = 10,
              line = list(color = 'black', width = 2)
            )
          ) %>% layout(margin = list(b = 160),
                       xaxis = list(title = "Cases", tickangle = 45),
                       yaxis = list(title = "Cost")
          )
        })
      output$ResourceDataFrame <-
        renderDataTable({
          directCost.dataframe
        })
    }
  })
  
  ## handle flexibility####
  
  ### Workload Flex$$##
  
  observeEvent(input$btn_calcWL, {
    # flexibility.Dataframe <- BuildFlowDF(traces())
    # flexibility.Dataframe <- UpdateFlowDF(flexibility.Dataframe)
    # drops <- c("dateDay","inventory","cumOpened",
    #            "cumClosed", "OutToIncumRateIns")
    #
    # flexibility.Dataframe.Update <-
    #   flexibility.Dataframe[, (names(flexibility.Dataframe) %in% drops)]
    
    
    # #View(flexibility.Dataframe.Update)
    # flexibility.Dataframe.Update$dateDay <-
    #   as.Date(flexibility.Dataframe.Update$dateDay)
    # flexibility.Dataframe.Update$dateDay <-
    #   as.Date(cut(
    #     flexibility.Dataframe.Update$dateDay,
    #     breaks = "week",
    #     start.on.monday = FALSE
    #   ))
    #View(dataframe.Flexibility())
    flexibility.Dataframe.melt <- melt(dataframe.Flexibility(),
                                       # flexibility.Dataframe[, (names(flexibility.Dataframe) %in% c("dateDay","inventory","cumOpened",
                                       #                                                                       "cumClosed", "OutToIncumRateIns"))],
                                       id.vars = 'dateDay',
                                       variable.name = 'measures')
    # #View(flexibility.Dataframe.Update)
    
    output$flexibilityMeasures <- renderPlot({
      # ggplot(flexibility.Dataframe.melt,
      #        aes(dateDay, value, group = 1, colour =
      #              measures)) + #theme(axis.text.x = element_text(angle = 90, hjust = 1))
      #   # stat_summary(fun.y = max) +
      #   # stat_summary_bin(data = flexibility.Dataframe.melt[which(flexibility.Dataframe.melt$measures ==
      #   #                                                                                        "cumOpened"), ], fun.y = max) +
      #   # stat_summary_bin(data = flexibility.Dataframe.melt[which(flexibility.Dataframe.melt$measures=="cumOpened"), ], fun.y = max)+
      #   # stat_summary_bin(data = flexibility.Dataframe.melt[which(flexibility.Dataframe.melt$measures=="cuminventoryRateIns"), ], fun.y = max)+
      #   # stat_summary_bin(data = flexibility.Dataframe.melt[which(flexibility.Dataframe.melt$measures=="OutToIncumRateIns"), ], fun.y = max) +
      #   theme(axis.text.x = element_text(angle = 90, hjust = 1))+
      #
      #   geom_point() +geom_path()+
      #   scale_y_continuous(limits = c(0,length(traces())+1))
      # +      scale_x_date(labels = date_format("%d.%m"))
      
      
      # ggplot(flexibility.Dataframe, aes(dateDay, cumOpened,group=1)) + geom_point()+ geom_path()
      # ggplot(flexibility.Dataframe, aes(dateDay, cumClosed,group=1)) + geom_point()+ geom_path()
      ggplot(
        flexibility.Dataframe.melt,
        aes(
          x = factor(dateDay),
          y = value,
          color = factor(measures),
          group = factor(measures)
        )
      ) +
        geom_point() + geom_line() + xlab('Date') + guides(color = guide_legend("Series")) +
        labs(title = "Flexibility measures") + theme(axis.text.x = element_text(angle = 90, hjust = 1))
      
    })
    
    output$BfSummary <- renderPrint({
      summary(dataframe.Flexibility()[,!(names(dataframe.Flexibility()) %in% c("dateDay"))])
    })
    
    
  })
  
  observeEvent(input$btn_showInvRate, {
    #View(dataframe.Flexibility())
    
    flexibility.Dataframe <- BuildFlowDF(traces())
    flexibility.Dataframe <- UpdateFlowDF(flexibility.Dataframe)
    modeInvRate = 0 #getmode(dataframe.Flexibility()$inventoryRateIns)
    # print(modeInvRate)
    output$flexibilityMeasures <-
      renderPlot({
        ggplot(dataframe.Flexibility(),
               aes(dateDay, inventoryRateIns, group = 1)) + geom_point() + geom_path() +
          geom_hline(yintercept = modeInvRate,
                     color = "red" ,
                     size = 1) +
          ylab('Inventory') + xlab('Date') + labs(title = "Inventory Rate overtime") +
          theme(axis.text.x = element_text(angle = 90, hjust = 1))
      })
    
    
    modeInvRate = getmode(dataframe.Flexibility()$justClosed)
    # print(modeInvRate)
    output$flexibilityMeasures2 <-
      renderPlot({
        ggplot(dataframe.Flexibility(),
               aes(dateDay, justClosed, group = 1)) + geom_point() + geom_path() +
          geom_hline(yintercept = modeInvRate,
                     color = "red" ,
                     size = 1) +
          ylab('throughput') + xlab('Date') + labs(title = "justClosed Rate overtime") +
          theme(axis.text.x = element_text(angle = 90, hjust = 1))
      })
    
  })
  
  observeEvent(input$btn_showCumInvRate, {
    #View(dataframe.Flexibility())
    
    modeInvRate = getmode(dataframe.Flexibility()$cuminventoryRateIns)
    # print(modeInvRate)
    output$flexibilityMeasures <-
      renderPlot({
        ggplot(dataframe.Flexibility(),
               aes(dateDay, cuminventoryRateIns, group = 1)) + geom_point() + geom_path() +
          ylab('Inventory') + xlab('Date') + labs(title = "Inventory Rate overtime") +
          theme(axis.text.x = element_text(angle = 90, hjust = 1))
      })
    
    
    
  })
  
  observeEvent(input$btn_showThroughputRate, {
    #View(dataframe.Flexibility())
    
    modeInvRate = getmode(dataframe.Flexibility()$justClosed)
    # print(modeInvRate)
    output$flexibilityMeasures <-
      renderPlot({
        ggplot(dataframe.Flexibility(),
               aes(dateDay, justClosed, group = 1)) + geom_point() + geom_path() +
          geom_hline(yintercept = modeInvRate,
                     color = "red" ,
                     size = 1) +
          ylab('throughput') + xlab('Date') + labs(title = "justClosed Rate overtime") +
          theme(axis.text.x = element_text(angle = 90, hjust = 1))
      })
    
    
  })
  
  
  observeEvent(input$btn_showCumInvRateInt, {
    # print("start handling workload flex event")
    #View(dataframe.Flexibility())
    # show the statistics about the emergency times
    BuildInvEmgDurCmp <- cmpfun(BuildInvEmgDur)
    
    InvEmgDur.dataframe <- BuildInvEmgDurCmp(dataframe.Flexibility())
    #View(InvEmgDur.dataframe)
    
    # InvEmgDur.dataframe<-BuildInvBuildingUpDur(flexDataframe = dataframe.Flexibility(),InvEmgDur.dataframe =InvEmgDur.dataframe )
    # #View(InvBuildingUpDur)
    # InvEmgDur.dataframe<-BuildInvRecoveryDur(flexDataframe = dataframe.Flexibility(),InvEmgDur.dataframe =InvEmgDur.dataframe )
    # #View(InvRecoveryDur)
    BuildInvDursCmp <- cmpfun(BuildInvDurs)
    InvEmgDur.dataframe <-
      BuildInvDursCmp(flexDataframe = dataframe.Flexibility(),
                   InvEmgDur.dataframe = InvEmgDur.dataframe)
    #View(InvEmgDur.dataframe)
    
    calculateWorkloadFlexCmp <- cmpfun(calculateWorkloadFlex)
    wf <- calculateWorkloadFlexCmp(InvEmgDur.dataframe, dataframe.Flexibility())
    
    workloadFlexValue <<- wf
    
    output$inventoryStatus <- renderPrint({
      flTimeDF = getDataframeOfList(vector.FlowTime())
      #View(flTimeDF)
      avgFlowTime = mean(flTimeDF$Value)#vector.FlowTime())
      #View(avgFlowTime)
      AvgInFlowTime = mean(InvEmgDur.dataframe$Duration) / avgFlowTime
      MinInFlowTime = min(InvEmgDur.dataframe$Duration) / avgFlowTime
      MaxInFlowTime = max(InvEmgDur.dataframe$Duration) / avgFlowTime
      
      strf = paste("Workload flexibility = ", as.double(wf))
      
      str0 = "Emergency status"
      str1 = paste("No of Emergency Inventory Cases",
                   nrow(InvEmgDur.dataframe),
                   sep = " ")
      str2 = paste(
        "The Average duration takes to recover the inprocess inventory is: ",
        mean(InvEmgDur.dataframe$Duration),
        " (",
        AvgInFlowTime,
        "Normal case flow time)",
        sep = ""
      )
      str3 = paste(
        "The Min time takes to recover the inprocess inventory is: ",
        min(InvEmgDur.dataframe$Duration),
        " (",
        MinInFlowTime,
        "Normal case flow time)",
        sep = ""
      )
      str4 = paste(
        "The Max duration takes to recover the inprocess inventory is: ",
        max(InvEmgDur.dataframe$Duration),
        " (",
        MaxInFlowTime,
        "Normal case flow time)",
        sep = ""
      )
      str15 = paste(
        "The average flow time in the emergency period is: ",
        mean(InvEmgDur.dataframe$Inventory) / mean(InvEmgDur.dataframe$ThroughputRate),
        sep = ""
      )
      
      str5 = "Building up Rate"
      str6 = paste(
        "The Average duration takes to Build up the Emergency status is: ",
        mean(InvEmgDur.dataframe$BuildingUpDuration),
        " (",
        mean(InvEmgDur.dataframe$BuildingUpDuration) / avgFlowTime,
        "Normal case flow time)",
        sep = ""
      )
      str7 = paste(
        "The Min time takes to Build up the Emergency status is: ",
        min(InvEmgDur.dataframe$BuildingUpDuration),
        " (",
        min(InvEmgDur.dataframe$BuildingUpDuration) / avgFlowTime,
        "Normal case flow time)",
        sep = ""
      )
      str8 = paste(
        "The Max duration takes to Build up the Emergency status is: ",
        max(InvEmgDur.dataframe$BuildingUpDuration),
        " (",
        max(InvEmgDur.dataframe$BuildingUpDuration) / avgFlowTime,
        "Normal case flow time)",
        sep = ""
      )
      # str13= paste("The average flow time in the building up period is: ",mean(InvEmgDur.dataframe$Inventory)/mean(InvBuildingUpDur$ThroughputRate),sep = "")
      
      str9 = "Recovery Rate"
      str10 = paste(
        "The Average duration takes to recover from the Emergency status is: ",
        mean(InvEmgDur.dataframe$InvRecoveryDur),
        " (",
        mean(InvEmgDur.dataframe$InvRecoveryDur) / avgFlowTime,
        "Normal case flow time)",
        sep = ""
      )
      str11 = paste(
        "The Min time takes to recover from the Emergency status is: ",
        min(InvEmgDur.dataframe$InvRecoveryDur),
        " (",
        min(InvEmgDur.dataframe$InvRecoveryDur) / avgFlowTime,
        "Normal case flow time)",
        sep = ""
      )
      str12 = paste(
        "The Max duration takes to recover from the Emergency status is: ",
        max(InvEmgDur.dataframe$InvRecoveryDur),
        " (",
        max(InvEmgDur.dataframe$InvRecoveryDur) / avgFlowTime,
        "Normal case flow time)",
        sep = ""
      )
      # str14= paste("The average flow time in the recovery  period is: ",mean(InvEmgDur.dataframe$Inventory)/mean(InvRecoveryDur$ThroughputRate),sep = "")
      
      
      # cat(paste0(paste(strf,str0,str1, str2,str3,str4,str15,str5,str6,str7,str8,str13,str9,str10,str11,str12,str14,sep = "\n"), "\n"))
      cat(paste0(
        paste(
          strf,
          str0,
          str1,
          str2,
          str3,
          str4,
          str15,
          str5,
          str6,
          str7,
          str8,
          str9,
          str10,
          str11,
          str12,
          sep = "\n"
        ),
        "\n"
      ))
      
    })
    ## render the graph
    # modeInvRate = getmode(dataframe.Flexibility()$cuminventoryRateIns)
    avgInventoryLevel = mean(dataframe.Flexibility()$cuminventoryRateIns)
    # avgInventoryLevel = mean(dataframe.Flexibility()$inventory)
    # print(avgInventoryLevel)
    # dateAsDate<-as.Date(dataframe.Flexibility()$dateDay)
     # dfFlexibility<-dataframe.Flexibility()
     # # dfFlexibility$DateAsDate<-as.Date(dfFlexibility$dateDay,"%m/%d/%Y")
    # dataframe.Flexibility()<-dfFlexibility
    # flowsz=dataframe.Flexibility()
    flowz=dataframe.Flexibility()[with(dataframe.Flexibility(), order(dataframe.Flexibility()$DateAsObject)), ]
    View(flowz)
    output$flexMesIntr <-
      renderPlot({
        ggplot(flowz,
               aes(DateAsObject, cuminventoryRateIns, group = 1)) + geom_point() + geom_path() + geom_hline(yintercept = avgInventoryLevel,
                                                                                                          color = "red" ,
                                                                                                          size = 1) +
          ylab('Inventory') + xlab('Date') + labs(title = "Inventory Rate overtime") +
          theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
          scale_x_date(labels = date_format("%m/%d/%Y"),
                       breaks = date_breaks("1 month"))
        
      })
    
  })
  
  output$BfSummary <- renderPrint({
    #nearPoints(dataframe.Flexibility(), input$plot1_click, addDist = TRUE)
    View(input$plot1_brush)
    selectedTubles = brushedPoints(dataframe.Flexibility(), input$plot1_brush)
    View(selectedTubles)
    if (nrow(selectedTubles) > 0) {
      selectedTubles <-
        selectedTubles[with(selectedTubles, order(dateDay)), ]
      firstDate = selectedTubles[1,]$dateDay
      lastDate = tail(selectedTubles, 1)$dateDay
      Duration = difftime(getDatetime(lastDate), getDatetime(firstDate), unit = "days")
      durationStr = paste(
        "It takes",
        Duration,
        " days to get back
        to the normal inprocess cases (Inventory) behavior ",
        sep = " "
      )
      #View(vector.FlowTime())
      flTimeDF = getDataframeOfList(vector.FlowTime())
      #View(flTimeDF)
      avgFlowTime = mean(flTimeDF$Value)#vector.FlowTime())
      #View(avgFlowTime)
      durationInFlowTime = Duration / avgFlowTime
      durationInFTStr = paste(
        "It takes what equivalent to",
        durationInFlowTime,
        "case flow time to get back
        to the normal inprocess cases (Inventory) behavior ",
        sep = " "
      )
      str3 = paste(durationStr, durationInFTStr, sep = "\n")
      cat(paste0(str3, "\n"))
    }
  })
  
  ### Behavioral Flex####
  observeEvent(input$btn_BFlex, {
    totalNCases <- length(traces())
    # BehavioralFlexibilityDF <-
    #   BuildBehavioralFlexibilityDF(dataframe.TracesCases(), totalNCases)
    # #View(BehavioralFlexibilityDF)
    BehavioralFlexibilityDF <- dataframe.BehavioralFlexibility()
    View(BehavioralFlexibilityDF)
    # giniCof = ineq(BehavioralFlexibilityDF$nCases)
    # 
    #Gini(BehavioralFlexibilityDF$Trace,BehavioralFlexibilityDF$nCases)
    # sdev=sd(BehavioralFlexibilityDF$nCases)
    # meanV = mean(BehavioralFlexibilityDF$nCases)
    # sdev=sdev/meanV*100
    sdev=sd(BehavioralFlexibilityDF$percentage)
    behavioralFlexValue <<- sdev
    output$BflexSummary <- renderPrint({
      str1 = paste("Total number of cases", totalNCases, sep = ":")
      str2 = paste("Total number of traces ",
                   nrow(BehavioralFlexibilityDF),
                   sep = ":")
      str3 = paste("The deviation in the execution behavior (st.deviation): ",format(sdev))
      # str5 = paste("The Gini cofficient is : ", giniCof)
      
      str4 = paste(str1, str2, str3, sep = "\n")
      cat(paste0(str4, "\n"))
      
    })
    output$BflexGraph <-
      renderPlotly({
        plot_ly(
          BehavioralFlexibilityDF,
          x = ~ row.names(BehavioralFlexibilityDF),
          y = ~ percentage,
          color = ~ percentage,
          size = ~ percentage,
          type = "scattergl",
          marker = list(
            size = 10,
            line = list(color = 'black', width = 2)
          )
        ) %>%
          layout(
            xaxis = list(title = "Traces IDs", tickangle = 45),
            yaxis = list(title = "Percentage of the trace in log",tickformat = "%")
          )#+ylab('number of cases') + xlab('Traces')
        # ggplot(BehavioralFlexibilityDF,
        #        aes(row.names(BehavioralFlexibilityDF), nCases, group = 1)) + geom_point() +
        #   ylab('Cases') + xlab('Traces') + labs(title = " Behavioral distribution ")
      })
    
    output$BehavioralFlex <-
      renderDataTable({
        BehavioralFlexibilityDF
      })
    
  })
  
  
  observeEvent(input$btn_HB, {
    #View(dataframe.BehavioralFlexibility())
    #  nTraces<-nrow(dataframe.BehavioralFlexibility())
    #  successPath<-vector()
    #  for(i in 1:nTraces){
    #
    #    successPath[i]<-'<input type="radio" name="Success" value="False"/>'
    #  }
    #  #View(successPath)
    #
    # BehavioralFlexibilityDF<-
    #   dataframe.BehavioralFlexibility()
    # BehavioralFlexibilityDF$success<-successPath
    #  #View(BehavioralFlexibilityDF)
    # # output$BfSummary <- renderPrint({
    # #   str1=paste("Total number of cases", totalNCases, sep = ":")
    # #   str2=paste("Total number of traces ", nrow(BehavioralFlexibilityDF), sep = ":")
    # #   str3=paste(str1,str2,sep = "\n")
    # #   cat(paste0( str3, "\n"))
    # #
    # # })
    output$tracess <-
      DT::renderDataTable(dataframe.BehavioralFlexibility(), server = FALSE)
    #    DT::renderDataTable(
    #      BehavioralFlexibilityDF, escape = FALSE, selection = 'none', server = FALSE,
    #   options = list(dom = 't', paging = FALSE, ordering = FALSE),
    #   callback = JS("table.rows().every(function(i, tab, row) {
    #                 var $this = $(this.node());
    #                 $this.attr('id', this.data()[0]);
    #                 $this.addClass('shiny-input-radiogroup');
    #                  });
    #                 Shiny.unbindAll(table.table().node());
    #                 Shiny.bindAll(table.table().node());")
    # )
    
  })
  
  ## handle the Quality####
  observeEvent(input$btn_CHB, {
    selected = input$tracess_rows_selected
    if (is.null(selected)) {
      showNotification("Please Select the successful paths to proceed")
      return(NULL)
    }
    totalSCases = 0
    for (i in 1:length(selected)) {
      hpTemp <- dataframe.BehavioralFlexibility()[selected[i],]
      totalSCases <- totalSCases + hpTemp$nCases
      # print(hpTemp$nCases)
    }
    
    totalNCases <- length(traces())
    
    SPer = totalSCases / totalNCases
    happyPathPercentage <<-SPer
    output$QDescription <- renderPrint({
      str1 = paste("Number of Happy Paths", length(selected), sep = ":")
      str2 = paste("Percentage of successful cases ", SPer, sep = ":")
      str3 = paste("Total number of success cases", totalSCases, sep = ":")
      str4 = paste(str1, str2, str3, sep = "\n")
      cat(paste0(str4, "\n"))
    })
  })
  
  observeEvent(input$btn_BPE, {
    selected = input$tracess_rows_selected
    if (is.null(selected)) {
      showNotification("Please Select the paths with business errors to proceed")
      return(NULL)
    }
    totalECases = 0
    for (i in 1:length(selected)) {
      hpTemp <- dataframe.BehavioralFlexibility()[selected[i],]
      totalECases <- totalECases + hpTemp$nCases
      # print(hpTemp$nCases)
    }
    
    totalNCases <- length(traces())
    
    ePer = totalECases / totalNCases
    errorPathPercentage <<- ePer
    output$QDescription <- renderPrint({
      str1 = paste("Number of  paths with business errors",
                   length(selected),
                   sep = ":")
      str2 = paste("Percentage of cases with business errors", ePer, sep = ":")
      str3 = paste("Total number of cases with business errors",
                   totalECases,
                   sep = ":")
      str4 = paste(str1, str2, str3, sep = "\n")
      cat(paste0(str4, "\n"))
    })
  })
  
  observeEvent(input$btn_Act, {
    Activities <- unique(dataframe.CAFTDRT()$Activity)
    output$moreControls <-
      renderUI(
        selectInput(
          inputId = 'ActivitiesList',
          choices = Activities,
          label = "Activities",
          multiple = TRUE
        )
      )
    # output$ActivitiesDT <- DT::renderDataTable(
    #   Activities, server = FALSE)
  })
  
  observeEvent(input$btn_ACHB, {
    selected = lapply(
      input$ActivitiesList,
      FUN = function(x)
        trimws(x , which = "both")
    )
    if (is.null(selected)) {
      showNotification("Please Select the Success Activities to proceed")
      return(NULL)
    }
    # print(selected)
    totalSCases = 0
    nTraces = 0
    for (i in 1:nrow(dataframe.BehavioralFlexibility())) {
      tr <- dataframe.BehavioralFlexibility()[i,]$Trace
      trL <- strsplit(tr, split = ",")[[1]]
      trL <- lapply(
        trL,
        FUN = function(x)
          trimws(x , which = "both")
      )
      if (all(selected %in% trL)) {
        nTraces = nTraces + 1
        totalSCases <-
          totalSCases + dataframe.BehavioralFlexibility()[i,]$nCases
      }
      
    }
    # print(hpTemp$nCases)
    
    
    totalNCases <- length(traces())
    
    SPer = totalSCases / totalNCases
    happyPathPercentage <<- SPer
    output$QADescription <- renderPrint({
      str1 = paste("Number of Happy Paths", nTraces, sep = ":")
      p <- paste(SPer * 100, "%")
      str2 = paste("Percentage of successful cases ", p, sep = ":")
      str3 = paste("Total number of success cases", totalSCases, sep = ":")
      str4 = paste(str1, str2, str3, sep = "\n")
      cat(paste0(str4, "\n"))
    })
  })
  
  observeEvent(input$btn_ABPE, {
    selected = lapply(
      input$ActivitiesList,
      FUN = function(x)
        trimws(x , which = "both")
    )
    if (is.null(selected)) {
      showNotification("Please Select the Success Activities to proceed")
      return(NULL)
    }
    # print(selected)
    totalECases = 0
    nTraces = 0
    for (i in 1:nrow(dataframe.BehavioralFlexibility())) {
      tr <- dataframe.BehavioralFlexibility()[i,]$Trace
      trL <- strsplit(tr, split = ",")[[1]]
      trL <- lapply(
        trL,
        FUN = function(x)
          trimws(x , which = "both")
      )
      if (all(selected %in% trL)) {
        nTraces = nTraces + 1
        totalECases <-
          totalECases + dataframe.BehavioralFlexibility()[i,]$nCases
      }
      
    }
    # print(hpTemp$nCases)
    
    
    totalNCases <- length(traces())
    
    EPer = totalECases / totalNCases
    errorPathPercentage <<- EPer
    output$QADescription <- renderPrint({
      p <- paste(EPer * 100, "%")
      str1 = paste("Number of  paths with business errors", nTraces, sep = ":")
      str2 = paste("Percentage of cases with business errors", p, sep = ":")
      str3 = paste("Total number of cases with business errors",
                   totalECases,
                   sep = ":")
      str4 = paste(str1, str2, str3, sep = "\n")
      cat(paste0(str4, "\n"))
    })
  })
  
  
  ## handle Devil ####
  observeEvent(input$btn_drawDevils, {
    print(totalCost)
    oldTime = as.numeric(input$oldTime)
    oldCost = as.numeric(input$oldCost)
    oldFlex = -1 * as.numeric(input$oldFlex)
    oldQuality = -1 * as.numeric(input$oldQuality)
    
    estimatedCost = as.numeric(input$EstimatedCost)
    calculatedCost = totalCost* as.numeric(input$weightHP)#560629698#totalCost # based on the direct cost in the log
    newC = calculatedCost / estimatedCost
    
    # FTs=sum(getDataframeOfCasesCTime(vector.FlowTime())$CycleTime)
    # EFR=1942*input$WeightEFR
    # (FTs/length(traces()))*input$WeightEFR
    
    # FTE=1*input$WeightFTE
    # FTE=(mean(dataframe.Efficiency()$EfficiencyLeadTime) )*input$WeightFTE
    HPratio = happyPathPercentage* as.numeric(input$weightHP)#0.016 * as.numeric(input$weightHP)#getDatetime(tail(t, 1)$timestamp
    BEratio = errorPathPercentage * as.numeric(input$weightBE) #0.0275 * as.numeric(input$weightBE)
    
    BF =  0.001 * as.numeric(input$weightBF)# behavioralFlexValue * as.numeric(input$weightBF)
    WF = 0.22167 *  as.numeric(input$weightWF)# workloadFlexValue*as.numeric(input$weightWF)
    
    newF = -1 * (BF + WF)
    newT = 0.12 #timedimensionValue #1#EFR+FTE
    newQ = -1 * (HPratio + BEratio)
    
    
    dfV <-
      data.frame(
        oldX = c(0, oldCost, 0, oldFlex),
        oldY = c(oldTime, 0, oldQuality, 0),
        newX = c(0, newC, 0, newF),
        newY = c(newT, 0, newQ, 0)
      )
    # output$DevilQAloneLabel <- renderText({
    #   "Current measures"
    # })
    # output$DevilQAlone <- renderPlot(
    #   ggplot(dfV) + geom_polygon(aes(
    #     x = dfV[, 3], y = dfV[, 4]
    #   )) + geom_hline(yintercept = 0) + geom_vline(xintercept = 0)
    #   + geom_label(x = 0, y = 1, label = "Time") + geom_label(x = 0, y =
    #                                                             -1, label = "Quality") + geom_label(x = 1, y = 0, label = "Cost") + geom_label(x =
    #                                                                                                                                              -1, y = 0, label = "Flexibility") + scale_x_continuous(limits = c(-1, 1), breaks = NULL) +
    #     scale_y_continuous(limits = c(-1, 1), breaks = NULL)
    #   + theme(
    #     axis.line = element_blank(),
    #     axis.text.x = element_blank(),
    #     axis.title.x = element_blank(),
    #     axis.title.y = element_blank(),
    #     legend.position = "none",
    #     panel.background = element_blank(),
    #     panel.border = element_blank()
    #     ,
    #     panel.grid.major = element_blank(),
    #     panel.grid.minor = element_blank(),
    #     plot.background = element_blank()
    #   )
    #   
    #   
    # )
    output$DevilQLabel <- renderText({
      "Current and old measures"
    })
    output$DevilQ <- renderPlot(
      ggplot(dfV) + geom_polygon(aes(
        x = dfV[, 1],
        y = dfV[, 2]
      ),fill=NA,colour="black",linetype = "dashed") + geom_polygon(aes(
        x = dfV[, 3], y = dfV[, 4]#,
        # linetype = "solid"
        # ,fill=NA
      ),fill=NA,colour="black",linetype = "solid") + geom_hline(yintercept = 0) + geom_vline(xintercept = 0)
      + geom_label(x = 0, y = 1, label = "Time") + geom_label(x = 0, y =
                                                                -1, label = "Quality") + geom_label(x = 1, y = 0, label = "Cost") + geom_label(x =
                                                                                                                                                 -1, y = 0, label = "Flexibility") + scale_x_continuous(limits = c(-1, 1), breaks = NULL) +
        scale_y_continuous(limits = c(-1, 1), breaks = NULL)
      + theme(
        axis.line = element_blank(),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.position = "none",
        panel.background = element_blank(),
        panel.border = element_blank()
        ,
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.background = element_blank()
      )+legend()
      
      
    )
  })
  
  
  
  
})




### Utilities and main functions #####

###Calculating time####

#Build the case-Activity-from:to transition- Delta Time- Resource- Delta type
###that represents the base of the all the other calculation and matrices\dataframe
BuildCAFTRDeltaMatrix <- function (log, unit = "hours") {
  # print("start building CAFTDRT Matrix")

  getDatetime <- function(strDate) {
    # datetime<-tryCatch({
    #   print("try")
    #   return(strptime(strDate, "%Y/%m/%d %H:%M:%S"))},
    #   warning = function(war){
    # print("war")
    # print(strDate)
    #     return(strptime(strDate, "%m/%d/%Y %H:%M:%S"))},
    #   error = function(err){
    #     print("err")
    #     print(err)
    #     return(strptime(strDate, "%m/%d/%Y %H:%M:%S"))})
    #
    datetime = strptime(strDate, "%m/%d/%Y %H:%M")
    if (is.na(datetime)) {
      datetime = strptime(strDate, "%Y/%m/%d %H:%M")
      if (is.na(datetime)) {
        datetime = strptime(strDate, "%Y/%m/%d %H:%M:%S")
        if (is.na(datetime)) {
          datetime = strptime(strDate, "%m/%d/%Y %H:%M:%S")
          if (is.na(datetime)) {
            datetime = strptime(strDate, "%d/%m/%Y %H:%M:%S")
            if (is.na(datetime)) {
              datetime = strptime(strDate, "%d/%m/%Y %H:%M")
            }
          }
        }
      }
      
    }
    
    return(datetime)
  }
  #Build the case-Activity-from:to transition- Delta Time- Resource- Delta type by consider the transition so its calculate its for each activity
  BuildCAFTRDeltaRow <- function (t, unit = "hours") {
    noEvents <- as.numeric(nrow(t))
    CAFTRDT <-
      data.frame(
        Case = character(),
        Activity = character(),
        From = character(),
        To = character(),
        Delta = numeric(),
        Resource = character(),
        DeltaType = character(),
        stringsAsFactors = FALSE
      )
    colnames(CAFTRDT) <-
      c("Case",
        "Activity",
        "From",
        "To",
        "Delta",
        "Resource",
        "DeltaType")
    
    i = 1
    caseId = t[1, 1]
    
    
    # loop on the activities within the trace to calculate its delta based on the different transtions of the activity
    activities <- t$activity
    if (length(activities) > 1) {
      for (act in activities) {
        events <- t[which(t$activity == act), ]
        noE <- nrow(events) - 1
        if (noE == 0) {
          next()
        }
        if (noE  >  1) {
          for (index in 1:noE)
          {
            activity <- as.character(events[index, ]$activity)
            
            # calculate execution time w.r.t next event start time
            print(events[index, ])
            sTime <- events[index, ]$timestamp
            # print("sTime")
            # print(sTime)
            eTime <- events[index + 1, ]$timestamp
            if (is.na(eTime))
              next()
            # eDateTime <- strptime(eTime, "%Y/%m/%d %H:%M:%OS")
            # sDateTime <- strptime(sTime, "%Y/%m/%d %H:%M:%OS")
            #
            eDateTime <-  getDateCmp(eTime)
            # as.POSIXct(eTime,format="%m/%d/%Y %H:%M")
            sDateTime <- getDateCmp(sTime)
            # as.POSIXct(sTime,format="%m/%d/%Y %H:%M")
            
            if (is.na(sDateTime)  || is.na(eDateTime))
            {
              # Vie(wt)
              print(activity)
              print(paste("start time", sTime, sep = ":"))
              print(paste("End time", eTime, sep = ":"))
              print(paste("start Dtime", sDateTime, sep = ":"))
              print(paste("End Dtime", eDateTime, sep = ":"))
            }
            diff = difftime(eDateTime, sDateTime, units = unit)
            numDiff <- as.numeric(diff, units = unit)
            # determine the from and To
            
            from <- as.character(events[index, ]$transition)
            to <- as.character(events[index + 1, ]$transition)
            
            deltaType <- "Waiting"
            if (from == "started" || to == "completed") {
              deltaType <- "Effective"
            }
            
            resource <- events[index, ]$resource
            if (is.null(resource)) {
              resource <- NA
            }
            
            
            rowTemp <-
              data.frame(
                Case = as.character(caseId),
                Activity = as.character(activity),
                From = from,
                To = to,
                Delta = numDiff,
                Resource = resource,
                DeltaType = deltaType,
                stringsAsFactors = FALSE
              )
            
            CAFTRDT <- rbind(CAFTRDT, rowTemp)
          }
        }
        else{
          actIndex = which(t$activity == act)
          tempT = t[c(actIndex - 1, actIndex), ]
          # CAFTRDRows <- BuildCAFTRDeltaRowwoLC(tempT, unit)
          CAFTRDRows <- BuildCAFTRDeltaRowwoLCCmp(tempT, unit)
          CAFTRDT <- rbind(CAFTRDT, CAFTRDRows)
        }
      }
    }
    
    return(CAFTRDT)
  }
  
  #Build the case-Activity-from:to transition- Delta Time- Resource- Delta type where the transition
  #are not exist so the start of the event is the completion of the previous event
  BuildCAFTRDeltaRowwoLC <- function (t, unit = "hours") {
    noEvents <- as.numeric(nrow(t))
    CAFTRDT <-
      data.frame(
        Case = character(),
        Activity = character(),
        From = character(),
        To = character(),
        Delta = numeric(),
        Resource = character(),
        DeltaType = character(),
        stringsAsFactors = FALSE
      )
    colnames(CAFTRDT) <-
      c("Case",
        "Activity",
        "From",
        "To",
        "Delta",
        "Resource",
        "DeltaType")
    i = 1
    caseId = t[1, 1]
    for (index in 2:noEvents) {
      activity <- as.character(t[index, ]$activity)
      PreActivity <- as.character(t[index - 1, ]$activity)
      # calculate execution time w.r.t next event start time
      sTime <- t[index - 1, ]$timestamp
      eTime <- t[index, ]$timestamp
      if (is.na(eTime)) {
        next
      }
      eDateTime <- getDateCmp(eTime)
      # strptime(eTime, "%Y/%m/%d %H:%M:%OS")
      sDateTime <- getDateCmp(sTime)
      # strptime(sTime, "%Y/%m/%d %H:%M:%OS")
      if (is.na(sDateTime)  || is.na(eDateTime)) {
        #View(t)
        print(index)
        print(paste("start time", sTime, sep = ":"))
        print(paste("End time", eTime, sep = ":"))
        print(paste("start Dtime", sDateTime, sep = ":"))
        print(paste("End Dtime", eDateTime, sep = ":"))
      }
      diff = difftime(eDateTime, sDateTime, units = unit)
      numDiff <- as.numeric(diff, units = unit)
      # determine the from and To
      if (numDiff < 0)
      {
        numDiff = numDiff * -1
      }
      
      from <- paste("End of Previous", PreActivity, sep = ":")
      to <- paste("Event Existance")
      deltaType <- "Effective"
      
      resource <- t[index, ]$resource
      if (is.null(resource)) {
        resource <- NA
      }
      
      
      rowTemp <-
        data.frame(
          Case = as.character(caseId),
          Activity = as.character(activity),
          From = from,
          To = to,
          Delta = numDiff,
          Resource = resource,
          DeltaType = deltaType,
          stringsAsFactors = FALSE
        )
      CAFTRDT <- rbind(CAFTRDT, rowTemp)
    }
    
    return(CAFTRDT)
  }
  
  getDateCmp <- cmpfun(getDatetime)
  BuildCAFTRDeltaRowCmp <- cmpfun(BuildCAFTRDeltaRow)
  BuildCAFTRDeltaRowwoLCCmp <- cmpfun(BuildCAFTRDeltaRowwoLC)
  
  cl <- makeCluster(4, type="SOCK") # for 4 cores machine
  # registerDoSNOW (cl)
  registerDoParallel(cl,cores=4)
  CAFTRDT <-
    data.frame(
      Case = character(),
      Activity = character(),
      From = character(),
      To = character(),
      Delta = numeric(),
      Resource = character(),
      DeltaType = character(),
      stringsAsFactors = FALSE
    )
  colnames(CAFTRDT) <-
    c("Case",
      "Activity",
      "From",
      "To",
      "Delta",
      "Resource",
      "DeltaType")
  
  print("start building CAFTDRT Matrix")
  traceNumber=1
  # for (t in log) {
  #   # when there is no transiton
  #   print(traceNumber)
  #   traceNumber= traceNumber+1
  #   if (is.null(t$transition)) {
  #     CAFTRDRows <- BuildCAFTRDeltaRowwoLC(t = t, unit = unit)
  #   }
  #   else{
  #     CAFTRDRows <- BuildCAFTRDeltaRow(t = t, unit = unit)
  #   }
  #   CAFTRDT <- rbind(CAFTRDT, CAFTRDRows)
  # }
  
  CAFTRDT <- foreach(t=iter(log, by='row'), .combine=rbind) %dopar% {
    print(traceNumber)
    print(t)
    traceNumber= traceNumber+1
      CAFTRDT <- BuildCAFTRDeltaRowCmp(t = t, unit = unit)
    
  }
   print("Finish building CAFTDRT Matrix")
  #View(CAFTRDT)
  stopCluster(cl)
  return(CAFTRDT)
}


CalculateCaseMovingTime <- function(t, unit = "hours") {
  activities <- t$activity
  mt = 0
  for (act in activities) {
    events <- which(t$activity == act)
    if (1 %in% events) {
      next
    }
    sTime <- t[min(events) - 1, ]$timestamp
    eTime <- t[min(events), ]$timestamp
    # sTime <- t[nrow(events)-1,]$timestamp
    # eTime <- t[nrow(events),]$timestamp
    eDateTime <- getDatetime(eTime)
    # as.POSIXct(eTime,format="%m/%d/%Y %H:%M")
    sDateTime <- getDatetime(sTime)
    # as.POSIXct(sTime,format="%m/%d/%Y %H:%M")
    # #View(eDateTime)
    # #View(sDateTime)
    diff = difftime(eDateTime, sDateTime, units = unit)
    numDiff <- as.numeric(diff, units = unit)
    mt = mt + numDiff
  }
  return(mt)
}


BuildmovingtimeDF <- function (log, unit = "hours") {
  # print("start building CAFTDRT Matrix")
  MTdf <-
    data.frame(Case = character(),
               Delta = numeric(),
               stringsAsFactors = FALSE)
  colnames(MTdf) <-
    c("Case",
      "Delta")
  
  # print("start building CAFTDRT Matrix")
  for (t in log) {
    # when there is no transiton
    if (is.null(t$transition)) {
      next()
    }
    else{
      movingT = as.numeric(CalculateCaseMovingTime(t, unit))
      caseID = as.character(t[1, ]$case)
      
      
      temp <-
        data.frame(Case = caseID,
                   Delta = 0.5,
                   stringsAsFactors = FALSE)
      colnames(temp) <-
        c("Case",
          "Delta")
      
      MTdf <- rbind(MTdf, temp)
    }
  }
  # print("Finish building CAFTDRT Matrix")
  # #View(MTdf)
  
  return(MTdf)
}

UpdateWaitingTime <- function(efficiencyDF, movingTime){
  
  for(case in efficiencyDF$Case){
    mt = movingTime[which(movingTime$Case==case),]$Delta
    if(is.na(mt))
    {
      next
    }
    wt=efficiencyDF[which(efficiencyDF$Case==case),]$WaitingTime 
    efficiencyDF[which(efficiencyDF$Case==case),]$WaitingTime = wt + mt
  }
  return(efficiencyDF)
}


# calculate the summary of the Effective or waiting time per each activity
CalculateTperActivity <-
  function(dataframe.CAFTDRT, type = "Effective") {
    # print("start calculating the summaries")
    CAFTDRTSub <-
      dataframe.CAFTDRT[dataframe.CAFTDRT$DeltaType == type,]
    activities.summaries <-
      lapply(split(CAFTDRTSub, f = dataframe.CAFTDRT$Activity), function(events)
        summary(as.numeric(events$Delta)))
    
    # print("Finish calculating the summaries")
    return(activities.summaries)
  }

###Calculate the summary values based on the summation of the summaries of the Effective times or
##waiting time of  the activities
CalculateTofBPbasedonActivitiesSummary <- function(summaries) {
  # print("start calculating the summary")
  BP.summary <- matrix(ncol = 6)
  colnames(BP.summary) <-
    c("Min.",
      "1st Qu.",
      "Median",
      "Mean",
      "3rd Qu.",
      "Max.")
  for (s in summaries) {
    BP.summary <-
      rbind(BP.summary, c(s[["Min."]], s[["1st Qu."]], s[["Median"]], s[["Mean"]], s[["3rd Qu."]], s[["Max."]]))
  }
  dataframe.BP <-
    data.frame(
      min = sum(BP.summary[, "Min."], na.rm = TRUE),
      q1 = sum(BP.summary[, "1st Qu."], na.rm = TRUE),
      med = sum(BP.summary[, "Median"], na.rm = TRUE),
      meanV = sum(BP.summary[, "Mean"], na.rm = TRUE),
      q3 = sum(BP.summary[, "3rd Qu."], na.rm = TRUE),
      max = sum(BP.summary[, "Max."], na.rm = TRUE)
    )
  colnames(dataframe.BP) <-
    c(" Min.",
      "1st Qu.",
      "Median",
      "Mean",
      "3rd Qu.",
      "Max.")
  # print("Finish calculating the summary")
  return(dataframe.BP)
}

# calculate the Effective or waiting time per each case by summing up the activity delta time
CalculateTperCase <-
  function(dataframe.CAFTDRT, type = "Effective") {
    CAFTDRTSub <-
      dataframe.CAFTDRT[dataframe.CAFTDRT$DeltaType == type,]
    #View(CAFTDRTSub)
    cases.summaries <-
      lapply(split(CAFTDRTSub, f = CAFTDRTSub$Case), function(events) {
        sumV = sum(as.numeric(events$Delta))
        if (is.na(sumV)) {
          sumV = 0
        }
        return(sumV)
      })
    return(cases.summaries)
  }

###Calculate the summary values based on the statistics function of the summaries of the Effective times or
##waiting time of the  cases based on the caseDataFrame data#
CalculateTofBPbasedonCasesSummary <- function(caseDataFrame) {
  return(summary(caseDataFrame$EffectiveTime))
}



###Build a dataframe from the cases summation effective time list to be used
### in CalculateTofBPbasedonCasesSummary function to calculate BP summary
### Also build the based that used in function::BuildingefficiencyCaseFrame that calculate the effeiciency
getDataframeOfCasesETime <- function(caseETList) {
  case.dataframe <-
    data.frame(
      Case = character(),
      EffectiveTime = numeric(),
      stringsAsFactors = FALSE
    )
  colnames(case.dataframe) <-
    c("Case",
      "EffectiveTime")
  noOfCases <- length(caseETList)
  caseSumETMatrix <- as.matrix(caseETList)
  casesIds <- row.names(caseSumETMatrix)
  for (i in 1:noOfCases) {
    temp <-
      data.frame(
        Case = as.character(casesIds[i]),
        EffectiveTime = as.numeric(caseETList[[i]]),
        stringsAsFactors = FALSE
      )
    colnames(temp) <-
      c("Case",
        "EffectiveTime")
    case.dataframe <- rbind(case.dataframe, temp)
  }
  return(case.dataframe)
}

getDataframeOfCasesWTime <- function(caseWTList) {
  case.dataframe <-
    data.frame(
      Case = character(),
      WaitingTime = numeric(),
      stringsAsFactors = FALSE
    )
  colnames(case.dataframe) <-
    c("Key",
      "Value")
  noOfCases <- length(caseWTList)
  caseSumWTMatrix <- as.matrix(caseWTList)
  casesIds <- row.names(caseSumWTMatrix)
  for (i in 1:noOfCases) {
    # print(paste("case id",casesIds[i],"Effective",caseETList[[i]],sep = ":"))
    temp <-
      data.frame(
        Case = as.character(casesIds[i]),
        WaitingTime = as.numeric(caseWTList[[i]]),
        stringsAsFactors = FALSE
      )
    colnames(temp) <-
      c("Case",
        "WaitingTime")
    case.dataframe <- rbind(case.dataframe, temp)
  }
  # #View(case.dataframe)
  return(case.dataframe)
}

getDataframeOfCasesETWTime <- function(caseETList, caseWTList) {
  case.dataframe <-
    data.frame(
      Case = character(),
      EffectiveTime = numeric(),
      WaitingTime = numeric(),
      stringsAsFactors = FALSE
    )
  colnames(case.dataframe) <-
    c("Case",
      "EffectiveTime",
      "WaitingTime")
  noOfCases <- length(caseETList)
  caseSumETMatrix <- as.matrix(caseETList)
  casesIds <- row.names(caseSumETMatrix)
  for (i in 1:noOfCases) {
    # print(paste("case id",casesIds[i],"Effective",caseETList[[i]],sep = ":"))
    temp <-
      data.frame(
        Case = as.character(casesIds[i]),
        EffectiveTime = as.numeric(caseETList[[i]]),
        WaitingTime = caseWTList[[casesIds[i]]],
        stringsAsFactors = FALSE
      )
    colnames(temp) <-
      c("Case", "EffectiveTime",
        "WaitingTime")
    case.dataframe <- rbind(case.dataframe, temp)
  }
  # #View(case.dataframe)
  return(case.dataframe)
}

getDataframeOfList <- function(listV) {
  keys.dataframe <-
    data.frame(Key = character(),
               Value = numeric(),
               stringsAsFactors = FALSE)
  colnames(keys.dataframe) <-
    c("Key",
      "Value")
  noOfKeys <- length(listV)
  keySumlistV <- as.matrix(listV)
  Keys <- row.names(keySumlistV)
  for (i in 1:noOfKeys) {
    # print(paste("case id",casesIds[i],"Effective",caseETList[[i]],sep = ":"))
    temp <-
      data.frame(
        Key = as.character(Keys[i]),
        Value = as.numeric(listV[[i]]),
        stringsAsFactors = FALSE
      )
    colnames(temp) <-
      c("Key",
        "Value")
    keys.dataframe <- rbind(keys.dataframe, temp)
  }
  # #View(case.dataframe)
  return(keys.dataframe)
}
### Calculate the effeicieny of each case with respect to 1- lead\ cyclic time,
###2- effeciency workflow based on the Min execution of case Trace,
###3- effeicieny workflow based on the Min effective time of buisness process
### And build an effeiciency matrix that contain the case, effective time ,waiting time, lead time, 3 types of effeicieny
BuildingefficiencyCaseFrame <-
  function(caseWTList,
           CaseMTDF,
           caseCTList,
           case.Dataframe,
           minETvalue,
           tracescases) {
    noOfCases <- nrow(case.Dataframe)
    print(nrow(case.Dataframe))
    print(noOfCases)
    wTimeVector <- vector()
    lTimeVector <- vector()
    efLtVector <- vector()
    efBPminETVector <- vector()
    efTraceminETVector <- vector()
    #View(caseWTList)
    for (i in 1:noOfCases) {
      WT <- caseWTList[[case.Dataframe[i, 1]]]
      MT <- CaseMTDF[which(CaseMTDF$Case == case.Dataframe[i, 1]),]$Delta
      ET <- case.Dataframe[i, 2]
      minETTrace <-
        as.numeric(tracescases[grepl(case.Dataframe[i, 1], tracescases$cases), ]$minEffectiveT)
      
      print(paste(length(minETTrace),":",minETTrace))
      
      if(length(minETTrace)==0){
        print("in the length condition")
        minETTrace=1
      }
      # to consider the waiting time between the activities when there is Transition life cycle
      LT <- as.numeric(caseCTList[[case.Dataframe[i, 1]]])
      # LT <- sum(c(WT, ET), na.rm = TRUE)
      if(length(WT)==0){
        WT=0
      }
      if(length(MT)==0)
      {
        MT=0
      }
      WT = WT + MT
      deltaLTET = LT - ET
      print(i)
      print("WT")
      print(WT)
      print("WT<deltaLTET")
      print(WT < deltaLTET)
      if (WT < deltaLTET) {
        WT = deltaLTET
      }
      ef <- ET / LT
      if (ET > LT) {
        ef = ET / (ET+WT)
      }
      
      efMinETBp <- ET / minETvalue
      # print(minETTrace)
      efMinETtrace <- ET / minETTrace
      print(efMinETtrace)
      wTimeVector[i] <- WT
      lTimeVector[i] <- LT
      efLtVector[i] <- ef
      efBPminETVector[i] <- efMinETBp
      efTraceminETVector[i] <- efMinETtrace
    }
    case.Dataframe$WaitingTime <- wTimeVector
    case.Dataframe$LeadTime <- lTimeVector
    case.Dataframe$EfficiencyLeadTime <- efLtVector
    case.Dataframe$EfficiencyMinET <- efBPminETVector
    case.Dataframe$EfficiencyTrace <- efTraceminETVector
    #View(case.Dataframe)
    return(case.Dataframe)
  }


# calculate the avg of the cyclic time
CalculateCycleTimePerTrace <- function (t, unit = "hours") {
  caseId = t[1, 1]
  sTime <- t[1,]$timestamp
  eTime <- tail(t, 1)$timestamp
  eDateTime <- getDatetime(eTime)
  # strptime(eTime, "%m/%d/%Y %H:%M")
  sDateTime <- getDatetime(sTime)
  # strptime(sTime, "%m/%d/%Y %H:%M")  # eDateTime <- strptime(eTime, "%Y/%m/%d %H:%M:%OS")
  # sDateTime <- strptime(sTime, "%Y/%m/%d %H:%M:%OS")
  diff = difftime(eDateTime, sDateTime, units = unit) #/ 8 # working hours per day
  # print(diff)
  numDiff <- as.numeric(diff, units = unit)
  if (numDiff < 0) {
    numDiff = numDiff * -1
  }
  # print(numDiff)
  return(numDiff)
}

# calculate the avg of the cyclic time
CalculateCycleTime <- function (log, unit = "hours") {
  cList <-
    lapply(X = log, function(x)
      CalculateCycleTimePerTrace(t = x , unit = "hours"))
  # outputCT <- matrix(unlist(cList))
  # avg <- mean(outputCT)
  #
  # print(paste("The average cyclic time of the business process is ", avg, sep =
  #               " "))
  #
  return(cList)
}

getDataframeOfCasesCTime <- function(caseCTList) {
  case.dataframe <-
    data.frame(
      Case = character(),
      CycleTime = numeric(),
      stringsAsFactors = FALSE
    )
  colnames(case.dataframe) <-
    c("Case",
      "CycleTime")
  noOfCases <- length(caseCTList)
  caseSumCTMatrix <- as.matrix(caseCTList)
  casesIds <- row.names(caseSumCTMatrix)
  for (i in 1:noOfCases) {
    temp <-
      data.frame(
        Case = as.character(casesIds[i]),
        EffectiveTime = as.numeric(caseCTList[[i]]),
        stringsAsFactors = FALSE
      )
    colnames(temp) <-
      c("Case",
        "CycleTime")
    case.dataframe <- rbind(case.dataframe, temp)
  }
  return(case.dataframe)
}

## handle traces data frame

### build dataframe inwhich the cases are grouped by there traces
BuildTraceCasesMatrix <- function(Traces) {
 print("In building trace case matri")
   df <-
    data.frame(
      tracesEL = character(),
      cases = character(),
      sortedTrace = character(),
      stringsAsFactors = FALSE
    )
  colnames(df) <- c("tracesEL", "cases","sortedTrace")
  
  for (t in Traces) {
    caseId =as.character( t[1, 1])
    strTrace = character()
    # noEvents <- as.numeric(nrow(t))
    # noE <- noEvents
    # for (index in 1:noE) {
    #   activityN <- as.character(t[index, ]$activity)
    #   strTrace <- paste(strTrace, ',', activityN)
    #
    # }
    strTrace <- paste(t$activity,collapse=",")
    sortedVersion=paste(sort(t$activity),collapse=",")
    if (sortedVersion %in% df$sortedTrace) {
      # print(caseId)
      cases <- df[which(df$sortedTrace == sortedVersion), 2]
      # print(paste("casesB : ",cases))
      cases <- paste(cases, ',', caseId)
      # print(paste("cases cur : ",cases))
      df[which(df$sortedTrace == sortedVersion), 2] <- cases
      # print(paste("casesB : ",df[which(df$sortedTrace == sortedVersion), 2]))

      # df[which(df$tracesEL == strTrace), 2] <- paste(df[which(df$tracesEL == strTrace), 2], ',', caseId)
    }
    else{
      dft <-
        data.frame(
          tracesEL = strTrace,
          cases = as.character(caseId),
          sortedTrace = sortedVersion,
          stringsAsFactors = FALSE
        )
      df <- rbind(df, dft)
    }

  }
  # 
  # df <- foreach(t=iter(Traces, by='row'), .combine=rbind) %do% {
  #   caseId = as.character(t[1, 1])
  #   strTrace  <- paste(t$activity,collapse=",")
  #   sortedVersion=paste(sort(t$activity),collapse=",")
  #   # print(sortedVersion)
  #   if (sortedVersion %in% df$sortedTrace) {
  #     # print("add new case")
  # 
  #     # print(paste("caseId : ",caseId))
  #     casesB=df[which(df$sortedTrace == sortedVersion),]$cases
  #     # print(paste("casesB : ",casesB))
  #     casesB = paste(casesB,',', caseId)
  #     df[which(df$sortedTrace == sortedVersion),]$cases <- casesB
  #     # print(paste("casesA : ",df[which(df$sortedTrace == sortedVersion),]$cases))
  #   }
  #   else{
  #     df<-
  #       data.frame(
  #         tracesEL = strTrace,
  #         cases = as.character(caseId),
  #         sortedTrace = sortedVersion,
  #         stringsAsFactors = FALSE
  #       )
  #     
  #   }
  # }
  View(df)
  return(df)
}

## Get the min effective time of a trace based on the effective time of its cases
GetMinETforTrace <- function(casesStr, case.Dataframe) {
  # print(casesStr)
  cases <- strsplit(x = as.character(casesStr), split = ",")[[1]]
  cases <-
    lapply(
      cases,
      FUN = function(x)
        trimws(x , which = "both")
    )
  return(min(case.Dataframe[case.Dataframe$Case %in% cases,]$EffectiveTime, na.rm = TRUE))
}

## Get the min effective for each trace based on their cases effective time
GetMinETforAllTraces <- function(tracescases, case.Dataframe) {
  minETs <- vector()
  noOfTraces <- nrow(tracescases)
  for (index in 1:noOfTraces) {
    t <- tracescases[index, ]
    minETs[index] <- GetMinETforTrace(t$cases, case.Dataframe)
  }
  tracescases$minEffectiveT <- minETs
  return(tracescases)
}

## Get the min effective time of all the traces to represent the min Effective time of the business process
GetMinETforBP <- function(tracescases) {
  return(min(tracescases$minEffectiveT, na.rm = TRUE))
}





##### Cost Measures####
CalculateWHperResourceInCase <-
  function(dataframe.CAFTDRT, type = "Effective") {
    CAFTDRTSub <-
      dataframe.CAFTDRT[dataframe.CAFTDRT$DeltaType == type,]
    # group the CAFTDRT with the resource and the cases
    resource.dataframe <-
      CAFTDRTSub %>% group_by(Case, Resource) %>% summarise(WH = sum(Delta))
    return(resource.dataframe)
  }

CalculateResourceCost <-
  function(resource.dataframe, salary.dataframe) {
    View(resource.dataframe)
    noOfrecords <- nrow(resource.dataframe)
    costWH <- vector()
    View(salary.dataframe)
    for (i in 1:noOfrecords) {
      # the input has the hour rate 
      resourceIndex=as.character(resource.dataframe[i, ]$Resource)
      print(resourceIndex)
      salV <- salary.dataframe[salary.dataframe$resource == resourceIndex, 2]
      print(salV)
      if(is.na(salV)){
        salary <- 28
      }
      else{
        salary <-as.numeric(salV)
        # as.numeric(salary.dataframe[salary.dataframe$resource == resource.dataframe[i, ]$Resource, 2])
      }
      # print(salary)
      # salary <- salary / 40 ## to get the cost of Working hour
      costWH[i] <- as.numeric(resource.dataframe[i, ]$WH) * salary
    }
    #View(resource.dataframe)
    resource.dataframe$Cost <- costWH
    return(resource.dataframe)
  }
groupUpHRCaseCost<-function(resource.dataframe)
{ 
  # directCostList <- lapply(resource.dataframe, function(events)
  # sum(as.numeric(events$cost)))
  drops <- c("Resource")
  
  resource.dataframe <-
    resource.dataframe[,!(names(resource.dataframe) %in% drops)]
  dt <- data.table(resource.dataframe)
  # directCostList <-
    # dt[,WH= sum(WH), Cost = sum(Cost), by = Case]
  directCostList <-
    resource.dataframe %>% group_by(Case) %>% summarise(Cost = sum(Cost), WorkingHours = sum(WH))
  return(directCostList)
}

CalculateDirectCost <- function(traces) {
  directCostList = NULL
  directCostList <- lapply(traces, function(events)
    sum(as.numeric(events$cost)))
  
  return(directCostList)
}

getDataframeOfDirectCost <- function(directCostList) {
  directCost.dataframe <-
    data.frame(Case = character(),
               Cost = numeric(),
               stringsAsFactors = FALSE)
  colnames(directCost.dataframe) <-
    c("Case",
      "Cost")
  noOfCases <- length(directCostList)
  caseSumETMatrix <- as.matrix(directCostList)
  casesIds <- row.names(caseSumETMatrix)
  for (i in 1:noOfCases) {
    temp <-
      data.frame(
        Case = as.character(casesIds[i]),
        Cost = as.numeric(directCostList[[i]]),
        stringsAsFactors = FALSE
      )
    colnames(temp) <-
      c("Case",
        "Cost")
    directCost.dataframe <- rbind(directCost.dataframe, temp)
  }
  return(directCost.dataframe)
}

CalculateWTperResourceInBP <-
  function(dataframe.CAFTDRT) {
    CAFTDRTSub <-
      dataframe.CAFTDRT[dataframe.CAFTDRT$DeltaType ==  "Effective", ]
    # group the CAFTDRT with the resource and the cases
    # resource.dataframe <-
    #   CAFTDRTSub %>% group_by(Resource) %>% summarise(nCases = length(unique(Case)), WH = sum(Delta))
    
    dt <- data.table(CAFTDRTSub)
    resource.dataframe <-
      dt[, list(nCases = length(unique(Case)), WH = sum(Delta)), by = Resource]
    #View(dt)
    return(resource.dataframe)
  }




##### Flexibility####

## THis function is used for both the behavioral flexibility and the quaility [happy path \ failure path] measures
BuildBehavioralFlexibilityDF <-
  function(tracecases.dataframe, totalNCases) {
    BehavioralFlexibilityDF <-
      data.frame(
        traces = character(),
        ncases = numeric(),
        percentage = numeric(),
        stringsAsFactors = FALSE
      )
    colnames(BehavioralFlexibilityDF) <-
      c("Trace", "nCases", "percentage")
    
    noOftraces <- nrow(tracecases.dataframe)
    for (i in 1:noOftraces) {
      trace <- tracecases.dataframe[i, 1]
      nCases <-
        length(strsplit(
          x = as.character(tracecases.dataframe[i,]$cases),
          split = ","
        )[[1]])
      percentage <- nCases / totalNCases
      temp <-
        data.frame(
          traces = trace,
          ncases = nCases,
          percentage = nCases / totalNCases,
          stringsAsFactors = FALSE
        )
      colnames(temp) <- c("Trace", "nCases", "percentage")
      
      BehavioralFlexibilityDF <-
        rbind(BehavioralFlexibilityDF, temp)
    }
    return(BehavioralFlexibilityDF)
  }

BuildFlowDFwithRDate <- function(log) {
  WorkLoad <- NULL
  for (t in log) {
    ## select the inflow
    InDay <- getDatetime(t[1, ]$timestamp)
    # as.POSIXct(t[1,]$timestamp,format="%m/%d/%Y %H:%M:%S")
    # as.Date(strptime(t[1,]$timestamp, "%m/%d/%Y %H:%M:%S"))
    # as.character(as.Date(strptime(t[1,]$timestamp, "%m/%d/%Y %H:%M:%S"), format =
    # "%m/%d/%Y %H:%M:%S"))
    
    OutDay <- getDatetime(tail(t, 1)$timestamp)
    # as.POSIXct(tail(t, 1)$timestamp,format="%m/%d/%Y %H:%M:%S")
    # as.Date(strptime(tail(t, 1)$timestamp, "%m/%d/%Y %H:%M:%S"))
    #   as.character(as.Date(strptime(
    #     tail(t, 1)$timestamp, "%m/%d/%Y %H:%M:%S"
    #   ), format = "%m/%d/%Y %H:%M:%S"))
    # # print(typeof(Day))
    ## Handle instance Inflow/ just opened cases
    if (is.null(WorkLoad)) {
      WorkLoad  <-
        data.frame(
          dateDay = InDay,
          justOpened = 1,
          justClosed = 0,
          casesStr = "",
          stringsAsFactors = FALSE
        )
      colnames(WorkLoad) <-
        c("dateDay",
          "justOpened",
          "justClosed",
          "casesStr")
    }
    else{
      if (InDay %in% WorkLoad$dateDay) {
        # print("update existed list")
        justOpened <-
          WorkLoad[which(WorkLoad$dateDay == InDay), ]$justOpened
        justOpened = justOpened + 1
        WorkLoad[which(WorkLoad$dateDay == InDay), ]$justOpened <-
          justOpened
      }
      else{
        # print("Add new trace in the data frame ")
        # print(InDay)
        temp <-
          data.frame(
            dateDay = InDay,
            justOpened = 1,
            justClosed = 0,
            casesStr = "",
            stringsAsFactors = FALSE
          )
        colnames(WorkLoad) <-
          c("dateDay",
            "justOpened",
            "justClosed",
            "casesStr")
        
        WorkLoad <- rbind(WorkLoad, temp)
      }
    }
    ## Handle instance OutFlow/ just closed cases
    if (OutDay %in% WorkLoad$dateDay) {
      justClosed <-
        WorkLoad[which(WorkLoad$dateDay == OutDay), ]$justClosed
      justClosed = justClosed + 1
      WorkLoad[which(WorkLoad$dateDay == OutDay), ]$justClosed <-
        justClosed
    }
    else{
      temp <-
        data.frame(
          dateDay = OutDay,
          justOpened = 0,
          justClosed = 1,
          casesStr = "",
          stringsAsFactors = FALSE
        )
      colnames(WorkLoad) <-
        c("dateDay",
          "justOpened",
          "justClosed",
          "casesStr")
      
      WorkLoad <- rbind(WorkLoad, temp)
    }
    
    ## Handle Inventory/running cases other than in and out
    nEvents = nrow(t) - 1
    for (index in 1:nEvents) {
      rDay <- getDatetime(t[index, ]$timestamp)
      # as.POSIXct(t[index,]$timestamp,format="%m/%d/%Y %H:%M:%S")
      # as.character(as.Date(strptime(t[index,]$timestamp, "%m/%d/%Y %H:%M:%S"), format =
      #                        "%m/%d/%Y %H:%M:%S"))
      if (rDay %in% WorkLoad$dateDay) {
        caseStr <- WorkLoad[which(WorkLoad$dateDay == rDay), ]$casesStr
        casesStr = paste(caseStr, t[index, ]$case, ",")
        WorkLoad[which(WorkLoad$dateDay == rDay), ]$casesStr <-
          casesStr
      }
      else{
        temp <-
          data.frame(
            dateDay = rDay,
            justOpened = 0,
            justClosed = 0,
            casesStr = paste(as.character(t[index, ]$case), ","),
            stringsAsFactors = FALSE
          )
        colnames(WorkLoad) <-
          c("dateDay",
            "justOpened",
            "justClosed",
            "casesStr")
        
        WorkLoad <- rbind(WorkLoad, temp)
      }
    }
    
    
  }
  #View(WorkLoad)
  return(WorkLoad)
  
}


BuildFlowDF <- function(log) {
  WorkLoad <- NULL
  for (t in log) {
    ## select the inflow
    InDay <- format(getDatetime(t[1, ]$timestamp),format='%m/%d/%Y')
    
    OutDay <- format(getDatetime(tail(t, 1)$timestamp),format='%m/%d/%Y')
    ## Handle instance Inflow/ just opened cases
    if (is.null(WorkLoad)) {
      WorkLoad  <-
        data.frame(
          dateDay = InDay,
          justOpened = 1,
          justClosed = 0,
          casesStr = paste(as.character(t[1, ]$case), ","),
          DateAsObject = as.Date(InDay,"%m/%d/%Y"),#getDatetime(InDay),
          stringsAsFactors = FALSE
        )
      colnames(WorkLoad) <-
        c("dateDay",
          "justOpened",
          "justClosed",
          "casesStr",
          "DateAsObject")
    }
    else{
      if (InDay %in% WorkLoad$dateDay) {
        # print("update existed list")
        justOpened <-
          WorkLoad[which(WorkLoad$dateDay == InDay), ]$justOpened
        justOpened = justOpened + 1
        WorkLoad[which(WorkLoad$dateDay == InDay), ]$justOpened <-
          justOpened
        
        # update the cases
        caseStr <-
          WorkLoad[which(WorkLoad$dateDay == InDay), ]$casesStr
        casesStr = paste(caseStr, t[1, ]$case, ",")
        WorkLoad[which(WorkLoad$dateDay == InDay), ]$casesStr <-
          casesStr
      }
      else{
        # print("Add new trace in the data frame ")
        # print(InDay)
        temp <-
          data.frame(
            dateDay = InDay,
            justOpened = 1,
            justClosed = 0,
            casesStr = paste(as.character(t[1, ]$case), ","),
            DateAsObject =  as.Date(InDay,"%m/%d/%Y"),#getDatetime(InDay),
            stringsAsFactors = FALSE
          )
        colnames(WorkLoad) <-
          c("dateDay",
            "justOpened",
            "justClosed",
            "casesStr",
            "DateAsObject")
        
        WorkLoad <- rbind(WorkLoad, temp)
      }
    }
    ## Handle instance OutFlow/ just closed cases
    if (OutDay %in% WorkLoad$dateDay) {
      justClosed <-
        WorkLoad[which(WorkLoad$dateDay == OutDay), ]$justClosed
      justClosed = justClosed + 1
      WorkLoad[which(WorkLoad$dateDay == OutDay), ]$justClosed <-
        justClosed
      
      caseStr <-
        WorkLoad[which(WorkLoad$dateDay == OutDay), ]$casesStr
      casesStr = paste(caseStr, t[1, ]$case, ",")
      WorkLoad[which(WorkLoad$dateDay == OutDay), ]$casesStr <-
        casesStr
    }
    else{
      temp <-
        data.frame(
          dateDay = OutDay,
          justOpened = 0,
          justClosed = 1,
          casesStr = paste(as.character(t[1, ]$case), ","),
          DateAsObject = as.Date(OutDay,"%m/%d/%Y"),# getDatetime(OutDay),
          stringsAsFactors = FALSE
        )
      colnames(WorkLoad) <-
        c("dateDay",
          "justOpened",
          "justClosed",
          "casesStr",
          "DateAsObject")
      
      WorkLoad <- rbind(WorkLoad, temp)
    }
  }
  View(WorkLoad)
  # WorkLoad <- WorkLoad[order(WorkLoad$DateAsObject), ]
  return(WorkLoad)
  
}

### check this i think we need to change how we calculate the
###inventory it should always consider the opened cases  till
## its closed
UpdateFlowDF <- function(flow.dataframe) {
  # order by date
  # print("start to update the flow df")
  #View(flow.dataframe)
  # flow.dataframe <-
    # flow.dataframe[with(flow.dataframe, order(as.Date(dateDay))), ]
  View(flow.dataframe)
  flow.dataframe <- flow.dataframe[order(flow.dataframe$DateAsObject),] 
  
  inventory <- vector()
  cumOpened <- vector()
  cumClosed <- vector()
  inventoryRateIns <- vector()
  cuminventoryRateIns <- vector()
  OutToIncumRateIns <- vector()
  InToOutcumRateIns <- vector()
  nrows = nrow(flow.dataframe)
  # print("define the additional vector")
  for (index in 1:nrows) {
    cases <-
      strsplit(flow.dataframe[index,]$casesStr, split = ",")[[1]]
    cases <-
      lapply(
        cases,
        FUN = function(x)
          trimws(x , which = "both")
      )
    # print(cases)
    inventory[index] <- length(unique(cases))
    inventoryRateIns[index] <-
      as.numeric(flow.dataframe[index,]$justOpened - flow.dataframe[index,]$justClosed)
    if (index == 1) {
      cumOpened[index] <- flow.dataframe[index,]$justOpened
      cumClosed[index] <- flow.dataframe[index,]$justClosed
      
      cuminventoryRateIns[index] = cumOpened[index] - cumClosed[index]
      OutToIncumRateIns[index] = cumClosed[index] / cumOpened[index]
      
      # InToOutcumRateIns[index]=cumOpened[index]/ cumClosed[index]
    }
    else{
      cumOpened[index] <-
        cumOpened[index - 1] + flow.dataframe[index,]$justOpened
      cumClosed[index] <-
        cumClosed[index - 1] + flow.dataframe[index,]$justClosed
      
      cuminventoryRateIns[index] = cumOpened[index] - cumClosed[index]
      OutToIncumRateIns[index] = cumClosed[index] / cumOpened[index]
      # InToOutcumRateIns[index]=cumOpened[index]/ cumClosed[index]
    }
  }
  flow.dataframe$inventory <- inventory
  
  flow.dataframe$inventoryRateIns <- inventoryRateIns
  flow.dataframe$cumOpened <- cumOpened
  flow.dataframe$cumClosed <- cumClosed
  flow.dataframe$cuminventoryRateIns <- cuminventoryRateIns
  flow.dataframe$OutToIncumRateIns <- OutToIncumRateIns
  
  # flow.dataframe$InToOutcumRateIns<-InToOutcumRateIns
  
  drops <- c("casesStr")
  
  flow.dataframe <-
    flow.dataframe[,!(names(flow.dataframe) %in% drops)]
  # print("finished")
  flow.dataframe <-flow.dataframe[order(flow.dataframe$DateAsObject),] 
    # flow.dataframe[with(flow.dataframe, order(as.Date(dateDay,format='%m/%d/%Y'))), ]
  View(flow.dataframe)
  return(flow.dataframe)
  
  
}

SetInventoryStatus <- function(flow.dataframe, acceptedInvL) {
  # order by date
  # print("start to SetInventoryStatus the flow df")
  #View(flow.dataframe)
  flow.dataframe <-flow.dataframe[order(flow.dataframe$DateAsObject),] 
    # flow.dataframe[with(flow.dataframe, order(dateDay)), ]
  InventoryStatus <- vector()
  nrows = nrow(flow.dataframe)
  # print("define the additional vector")
  for (index in 1:nrows) {
    if (flow.dataframe[index,]$cuminventoryRateIns > acceptedInvL) {
      InventoryStatus[index] <- "Above"
    }
    else if (flow.dataframe[index,]$cuminventoryRateIns == acceptedInvL) {
      InventoryStatus[index] <- "Accepted"
    }
    else if (flow.dataframe[index,]$cuminventoryRateIns < acceptedInvL) {
      InventoryStatus[index] <- "Below"
    }
  }
  
  flow.dataframe$InventoryStatus <- InventoryStatus
  flow.dataframe <-flow.dataframe[order(flow.dataframe$DateAsObject),] 
    # flow.dataframe[with(flow.dataframe, order(dateDay)), ]
  # flow.dataframe$DateAsDate <- as.Date(flow.dataframe$dateDay,format="%m/%d/%Y")
  #View(flow.dataframe)
  return(flow.dataframe)
}

BuildInvEmgDur <- function(flexDataframe) {
  
  InvEmgDur.dataframe <- NULL
  # data.frame(
  #   SDate = character() ,
  #   EDate = character() ,
  #   Duration = numeric(),
  #   stringsAsFactors = FALSE
  # )
  nrows = nrow(flexDataframe)+1
  temp = NULL
  SDate = NULL
  EDate = NULL
  sIndex = NULL
  index = 1
  # for (index in 1:nrows) {
  while(index < nrows) { 
    invStatus = flexDataframe[index,]$InventoryStatus
    print("INN THE OUTER index")
    print(index)
    
    if (invStatus == "Above") {
      print(invStatus)
      if (is.null(temp)) {
        temp <- data.frame(
          SDate = character() ,
          EDate = character() ,
          Duration = numeric(),
          Inventory = numeric(),
          ThroughputRate = numeric(),
          stringsAsFactors = FALSE
        )
        SDate = flexDataframe[index,]$dateDay
        sIndex = index
      }
       tempIndex= sIndex+1
      for(k in tempIndex:nrows){
        invKStatus = flexDataframe[k,]$InventoryStatus
        print(invKStatus)
        if(invKStatus != "Above"){
          print("index")
          print(index)
          print("k")
          print(k)
          if (!is.null(temp)) {
            EDate = flexDataframe[k,]$dateDay
            Duration = difftime(getDatetime(EDate), getDatetime(SDate), unit = "days")
            EmergenceRecords <- flexDataframe[c(sIndex:k), ]
            ThroughputRate = mean(EmergenceRecords$cumClosed, na.rm = TRUE)
            Inventory = mean(EmergenceRecords$cuminventoryRateIns, na.rm = TRUE)
            
            temp <- data.frame(
              SDate = SDate ,
              EDate = EDate ,
              Duration = Duration,
              Inventory = Inventory,
              ThroughputRate = ThroughputRate,
              stringsAsFactors = FALSE
            )
            if (is.null(InvEmgDur.dataframe)) {
              InvEmgDur.dataframe <- temp
            }
            else{
              InvEmgDur.dataframe <- rbind(InvEmgDur.dataframe, temp)
            }
            temp = NULL
            SDate = NULL
            EDate = NULL
            sIndex = NULL
            index=k -1 
          }
          break
        }
    }
      
    }
    index=index +1
  }
  return(InvEmgDur.dataframe)
}

BuildInvBuildingUpDur <-
  function(flexDataframe, InvEmgDur.dataframe) {
    InvBuildingUpDur.dataframe <- NULL
    # data.frame(
    #   SDate = character() ,
    #   EDate = character() ,
    #   Duration = numeric(),
    #   Inventory=numeric(),
    #   ThroughputRate=numeric(),
    #   stringsAsFactors = FALSE
    # )
    nrows = nrow(InvEmgDur.dataframe)
    BuildingUpDuration <- vector()
    for (index in 1:nrows) {
      # get the first below before the start of the emergency
      indexSt <-
        which(flexDataframe$dateDay == InvEmgDur.dataframe[index, ]$SDate)
      belowR <- flexDataframe[indexSt - 1, ]
      
      # get the first hight invetory value in the emergency area
      emergencyRecords = flexDataframe[which(
        flexDataframe$dateDay >= InvEmgDur.dataframe[index, ]$SDate
        &
          flexDataframe$dateDay <= InvEmgDur.dataframe[index, ]$EDate
      ), ]
      #View(emergencyRecords)
      highestInv = max(emergencyRecords$cuminventoryRateIns, na.rm = TRUE)
      firstHighestInv = emergencyRecords[min(which(
        emergencyRecords$dateDay == min(emergencyRecords[which(emergencyRecords$cuminventoryRateIns ==
                                                                 highestInv), ]$dateDay)
      )), ]
      
      # calculate the duration
      SDate = belowR$dateDay
      PickDate = firstHighestInv$dateDay
      Duration = difftime(PickDate, SDate, unit = "days")
      BuildingUpDuration[index] <- Duration
      
      # # get the average throughput\closed cases within the recovery time
      # sIndex=indexSt-1
      # if(indexSt<0){indexSt=0}
      # buildingUpRecords<-flexDataframe[c(indexSt:min(which(flexDataframe$cuminventoryRateIns==highestInv))),]
      # ThroughputRate= mean(buildingUpRecords$cumClosed,na.rm = TRUE)
      # Inventory=mean(buildingUpRecords$cuminventoryRateIns,na.rm = TRUE)
      #
      # temp <- data.frame(
      #       SDate = SDate ,
      #       PickDate = PickDate ,
      #       Duration = Duration,
      #       Inventory = Inventory,
      #       ThroughputRate=ThroughputRate,
      #       stringsAsFactors = FALSE
      #     )
      # if(is.null(InvBuildingUpDur.dataframe)){InvBuildingUpDur.dataframe<-temp}
      # else{InvBuildingUpDur.dataframe <- rbind(InvBuildingUpDur.dataframe, temp)}
      #
    }
    InvEmgDur.dataframe$BuildingUpDuration <- BuildingUpDuration
    # return(InvBuildingUpDur.dataframe)
    return(InvEmgDur.dataframe)
  }
BuildInvDurs <- function(flexDataframe, InvEmgDur.dataframe) {
  View(flexDataframe)
  View(InvEmgDur.dataframe)
  InvBuildingUpDur.dataframe <- NULL
  # data.frame(
  #   SDate = character() ,
  #   EDate = character() ,
  #   Duration = numeric(),
  #   Inventory=numeric(),
  #   ThroughputRate=numeric(),
  #   stringsAsFactors = FALSE
  # )
  nrows = nrow(InvEmgDur.dataframe)
  BuildingUpDuration <- vector()
  InvRecoveryDur <- vector()
  for (index in 1:nrows) {
    # get the first below before the start of the emergency
    indexSt <-
      which(flexDataframe$dateDay == InvEmgDur.dataframe[index, ]$SDate)
    # belowR <- flexDataframe[indexSt - 1, ]
    belowR <- flexDataframe[indexSt, ]
    #get the emergency records
    emergencyRecords = flexDataframe[which(
      flexDataframe$dateDay >= InvEmgDur.dataframe[index, ]$SDate
      &
        flexDataframe$dateDay <= InvEmgDur.dataframe[index, ]$EDate
    ), ]
    highestInv = max(emergencyRecords$cuminventoryRateIns, na.rm = TRUE)
    # get the first hight invetory value in the emergency area
    firstHighestInv = emergencyRecords[min(which(
      emergencyRecords$dateDay == min(emergencyRecords[which(emergencyRecords$cuminventoryRateIns ==
                                                               highestInv), ]$dateDay)
    )), ]
    
    # calculate the duration
    SDate = belowR$dateDay
    PickDate = firstHighestInv$dateDay
    Duration = difftime(getDatetime(PickDate), getDatetime(SDate), unit = "days")
    BuildingUpDuration[index] <- Duration
    
    # get the first below after the end of the emergency
    indexEt <-
      which(flexDataframe$dateDay == InvEmgDur.dataframe[index, ]$EDate)
    belowAfterR <- flexDataframe[indexEt, ]
    # belowAfterR <- flexDataframe[indexEt + 1, ]
    # get the Last hightest invetory value in the emergency area
    lastHighestInv = emergencyRecords[max(which(
      emergencyRecords$dateDay == max(emergencyRecords[which(emergencyRecords$cuminventoryRateIns ==
                                                               highestInv), ]$dateDay)
    )), ]
    
    
    # calculate the duration
    RecoveryDate = belowAfterR$dateDay
    PickDate = lastHighestInv$dateDay
    Duration = difftime(getDatetime(RecoveryDate), getDatetime(PickDate), unit = "days")
    InvRecoveryDur[index] <- Duration
    
    # # get the average throughput\closed cases within the recovery time
    # sIndex=indexSt-1
    # if(indexSt<0){indexSt=0}
    # buildingUpRecords<-flexDataframe[c(indexSt:min(which(flexDataframe$cuminventoryRateIns==highestInv))),]
    # ThroughputRate= mean(buildingUpRecords$cumClosed,na.rm = TRUE)
    # Inventory=mean(buildingUpRecords$cuminventoryRateIns,na.rm = TRUE)
    #
    # temp <- data.frame(
    #       SDate = SDate ,
    #       PickDate = PickDate ,
    #       Duration = Duration,
    #       Inventory = Inventory,
    #       ThroughputRate=ThroughputRate,
    #       stringsAsFactors = FALSE
    #     )
    # if(is.null(InvBuildingUpDur.dataframe)){InvBuildingUpDur.dataframe<-temp}
    # else{InvBuildingUpDur.dataframe <- rbind(InvBuildingUpDur.dataframe, temp)}
    #
  }
  View(InvRecoveryDur)
  View(BuildingUpDuration)
  InvEmgDur.dataframe$BuildingUpDuration <- BuildingUpDuration
  InvEmgDur.dataframe$InvRecoveryDur <- InvRecoveryDur
  # return(InvBuildingUpDur.dataframe)
  return(InvEmgDur.dataframe)
}
BuildInvRecoveryDur <- function(flexDataframe, InvEmgDur.dataframe) {
  InvRecoveryDur.dataframe <- NULL
  # data.frame(
  #   SDate = character() ,
  #   EDate = character() ,
  #   Duration = numeric(),
  #   Inventory = numeric(),
  #   ThroughputRate= numeric(),
  #   stringsAsFactors = FALSE
  # )
  nrows = nrow(InvEmgDur.dataframe)
  InvRecoveryDur <- vector()
  
  for (index in 1:nrows) {
    # get the first below after the start of the emergency
    indexEt <-
      which(flexDataframe$dateDay == InvEmgDur.dataframe[index, ]$EDate)
    belowR <- flexDataframe[indexEt + 1, ]
    
    # get the Last hightest invetory value in the emergency area
    emergencyRecords = flexDataframe[which(
      flexDataframe$dateDay >= InvEmgDur.dataframe[index, ]$SDate &
        flexDataframe$dateDay <= InvEmgDur.dataframe[index, ]$EDate
    ), ]
    #View(emergencyRecords)
    highestInv = max(emergencyRecords$cuminventoryRateIns, na.rm = TRUE)
    lastHighestInv = emergencyRecords[max(which(
      emergencyRecords$dateDay == min(emergencyRecords[which(emergencyRecords$cuminventoryRateIns ==
                                                               highestInv), ]$dateDay)
    )), ]
    
    
    # calculate the duration
    RecoveryDate = belowR$dateDay
    PickDate = lastHighestInv$dateDay
    Duration = difftime(RecoveryDate, PickDate, unit = "days")
    InvRecoveryDur[index] <- Duration
    
    # # get the average throughput\closed cases within the recovery time
    # recoveryRecords<-flexDataframe[c(max(which(flexDataframe$cuminventoryRateIns==highestInv)):indexEt+1),]
    # ThroughputRate= mean(recoveryRecords$cumClosed,na.rm = TRUE)
    # Inventory=mean(recoveryRecords$cuminventoryRateIns,na.rm = TRUE)
    #
    # temp <- data.frame(
    #   PickDate = PickDate ,
    #   RecoveryDate = RecoveryDate ,
    #   Duration = Duration,
    #   Inventory = Inventory,
    #   ThroughputRate=ThroughputRate,
    #   stringsAsFactors = FALSE
    # )
    # if(is.null(InvRecoveryDur.dataframe)){InvRecoveryDur.dataframe<-temp}
    # else{InvRecoveryDur.dataframe <- rbind(InvRecoveryDur.dataframe, temp)}
    #
    
  }
  # return(InvRecoveryDur.dataframe)
  InvEmgDur.dataframe$InvRecoveryDur <- InvRecoveryDur
  # return(InvBuildingUpDur.dataframe)
  return(InvEmgDur.dataframe)
}

getmode <- function(v) {
  uniqv <- unique(v)
  modeValue <- uniqv[which.max(tabulate(match(v, uniqv)))]
  return(modeValue)
}

calculateWorkloadFlex <- function(InvEmgDur.dataframe, flexDataframe) {
  nrows = nrow(InvEmgDur.dataframe)
  wf = 0
  for (index in 1:nrows) {
    ED = InvEmgDur.dataframe[index, ]$Duration
    if(ED==0)
    {
      next
    }
    BD = InvEmgDur.dataframe[index, ]$BuildingUpDuration
    RD = InvEmgDur.dataframe[index, ]$InvRecoveryDur
    SD = ED - (BD + RD)
    BR = BD / (BD + RD)
    SR = 1 - (as.double(SD) / as.double(ED))
    # print(paste("index",":",index))
    # print(paste("ED",":",ED))
    # print(paste("BD",":",BD))
    # print(paste("RD",":",RD))
    # print(paste("SD",":",SD))
    # print(paste("BR",":",BR))
    # print(paste("SR",":",SR))
    
    temp = ED * BR * SR
    # print(paste("temp",":",temp))
    wf = wf + temp
    # print(paste("WF",":",wf))
    
  }
  # get the date of the last event within the log
  # lastcase = traces[[length(traces)]]
  # lastDate = getDatetime(as.character(tail(lastcase, 1)$timestamp))
  # # as.POSIXct(traces[[length(traces)]][1,]$timestamp,format="%m/%d/%Y %H:%M:%S")
  # # as.Date(strptime(traces[[length(traces)]][1,]$timestamp, "%m/%d/%Y %H:%M:%S"))
  # firstDate = getDatetime(traces[[1]][1, ]$timestamp)
  # # as.POSIXct(traces[[1]][1,]$timestamp,format="%m/%d/%Y %H:%M:%S")
  # as.Date(strptime(traces[[1]][1,]$timestamp, "%m/%d/%Y %H:%M:%S"))
  # totalTime = difftime(lastDate, firstDate, unit = "days")
  firstDate=getDatetime(flexDataframe[1,]$dateDay)
  lastDate=getDatetime(flexDataframe[nrow(flexDataframe),]$dateDay)
  totalTime = difftime(lastDate,firstDate , unit = "days")
  finalWf = wf / as.double(totalTime)
  return(finalWf)
}

getDatetime <- function(strDate) {
  # datetime<-tryCatch({
  #   print("try")
  #   return(strptime(strDate, "%Y/%m/%d %H:%M:%S"))},
  #   warning = function(war){
  # print("war")
  # print(strDate)
  #     return(strptime(strDate, "%m/%d/%Y %H:%M:%S"))},
  #   error = function(err){
  #     print("err")
  #     print(err)
  #     return(strptime(strDate, "%m/%d/%Y %H:%M:%S"))})
  #
  datetime = strptime(strDate, "%m/%d/%Y %H:%M")
  if (is.na(datetime)) {
    datetime = strptime(strDate, "%Y/%m/%d %H:%M")
    if (is.na(datetime)) {
      datetime = strptime(strDate, "%Y/%m/%d %H:%M:%S")
      if (is.na(datetime)) {
        datetime = strptime(strDate, "%m/%d/%Y %H:%M:%S")
        if (is.na(datetime)) {
          datetime = strptime(strDate, "%d/%m/%Y %H:%M:%S")
          if (is.na(datetime)) {
            datetime = strptime(strDate, "%d/%m/%Y %H:%M")
            if (is.na(datetime)) {
              datetime = strptime(strDate, "%m/%d/%Y")
            }
          }
        }
      }
    }
    
  }
  
  return(datetime)
}