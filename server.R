
function(input,output){
  
  # 頁面切換
  v <- reactiveValues(npage = 0)
  
  observeEvent(input$home, {js$reset()})
  observeEvent(input$page0next, {v$npage = 1})
  observeEvent(input$page1next, {RiskInfo()})
  observeEvent(input$page2go3, {v$npage = 3})
  observeEvent(input$page2go5, {v$npage = 5})
  observeEvent(input$page3prev, {v$npage = 2})
  observeEvent(input$page3next, {v$npage = 4})
  observeEvent(input$page4prev, {v$npage = 3})
  observeEvent(input$page4next, {
    RunONE()
    v$npage = 6
    })
  observeEvent(input$page5prev, {v$npage = 2})
  observeEvent(input$page5next, {
    RunTWO()
    v$npage = 6
  })
  
  output$pagename <- reactive({
    npage = v$npage
    pagename = switch(as.character(npage),
                      '0' = "FinLab Robot",
                      '1' = "會員資料",
                      '2' = "篩選方式",
                      
                      '3' = "第一階段",
                      '4' = "第二階段",
                      
                      '5' = "關鍵字搜尋",
                      
                      '6' = " ")
    pagename
  })
  
  
  # 判斷會員身分
  output$ID <- renderText({
    Name = input$name
    AccountInfo = read.csv("TData/AccountInfo.csv", stringsAsFactors = F)
    if((Name=="") | any(AccountInfo$Name==Name))
    {
      return()
    }else{
      return("請輸入有效姓名")
    }
  })
  
  # 風險等級提醒
  RiskInfo <- reactive({
    Name = input$name
    AccountInfo = read.csv("TData/AccountInfo.csv", stringsAsFactors = F)
    if(any(AccountInfo$Name==Name))
    {
      Risklevel = AccountInfo$RiskLevel[AccountInfo$Name==Name]
      LevelName = AccountInfo$LevelName[AccountInfo$Name==Name]
      showModal(tags$div(id="Info1", modalDialog(title = tags$b(paste(Name, "您好!", sep = "，")),
                                                 tags$p(paste("您的風險屬性為：",LevelName), style = "color:red; font-size:15px;"),
                                                 br(),
                                                 tags$p(paste("只能投資風險等級小於等於",Risklevel,"的基金。"), style = "font-size:15px;"),
                                                 footer = modalButton("ok"))))
      v$npage = 2
    }else{
      showModal(tags$div(id="Info2", modalDialog(title = tags$b("查無會員資料", style = "color: red;"),
                                                 tags$p("請重新輸入會員姓名"),
                                                 footer = modalButton("關閉"))))
    }
  })
  
  # 關鍵字輸入說明
  output$keyintro <- renderText({
    andor = input$andor
    if(andor=="or")
    {
      return("關鍵字若為一個以上，請以空格隔開，例如：股票 全球。(代表 股票 or 全球)")
    }
    if(andor=="and"){
      return("關鍵字若為一個以上，請以空格隔開，例如：股票 全球。(代表 股票 and 全球)")
    }
  })
  
  # 第一階段 (預設與否)
  output$ui_1 <- renderUI({
    
    if(input$default_1 == F)
    {
      
      tagList(p(class="word", "基金於投資人所選擇的條件之排名總平均須符合投資人所設定的百分比條件。"),
              br(),
              dateInput("date", label = p(icon("calendar")," 基金需於多久之前成立："), value = "2008-01-01"),
              br(),
              p(class="subword", "請勾選欲考慮排名的條件："),
              checkboxInput("scale", label = "基金規模", value = F),
              checkboxInput("sd", label = "年化標準差", value = F),
              checkboxInput("return", label = "定期定額年化報酬率", value = F),
              checkboxInput("sharpe", label = "Sharpe ratio", value = F),
              br(),
              numericInput("num", label = p("選擇排名總平均前 x % 之基金："), value = 20, min = 20),
              br())
      
    }else{
      
      tagList(p(class="word", "基金於投資人所選擇的條件之排名總平均須符合投資人所設定的百分比條件。"),
              br(),
              dateInput("date", label = p(icon("calendar")," 基金需於多久之前成立："), value = "2008-01-01"),
              br(),
              p(class="subword", "請勾選欲考慮排名的條件："),
              checkboxInput("scale", label = "基金規模", value = T),
              checkboxInput("sd", label = "年化標準差", value = T),
              checkboxInput("return", label = "定期定額年化報酬率", value = T),
              checkboxInput("sharpe", label = "Sharpe ratio", value = T),
              br(),
              numericInput("num", label = p("選擇排名總平均前 x % 之基金："), value = 25, min = 20),
              br())
      
    }
    
  })
  
  # 第二階段 (預設與否)
  output$ui_2 <- renderUI({
    
    if(input$default_2 == F)
    {
      
      tagList(p(class="word", "通過第一階段之基金，須同時具備投資人所勾選的以下條件才可納入資產池作為投資標的。"),
              br(),
              p(class="subword", "請勾選條件："),
              checkboxInput("alpha", label = "經理人績效是否優於大盤", value = F),
              checkboxInput("pret", label = "自今年以來報酬率為正數", value = F),
              br())
      
    }else{
      
      tagList(p(class="word", "通過第一階段之基金，須同時具備投資人所勾選的以下條件才可納入資產池作為投資標的。"),
              br(),
              p(class="subword", "請勾選條件："),
              checkboxInput("alpha", label = "經理人績效是否優於大盤", value = T),
              checkboxInput("pret", label = "自今年以來報酬率為正數", value = T),
              br())
      
    }
    
  })
  
  # ---------------------- Run ---------------------- #
  
  # 關鍵字搜尋 - 國內
  temDPoolTable <- eventReactive(input$send, {
    library(DT)
    source('D05S01_GenPool02_D.R')
    temDPool = GenPool02_D(input$name, input$keyword, input$andor)
    DT::datatable(temDPool, class = 'cell-border stripe',  rownames = c(1:nrow(temDPool)),
                  caption = htmltools::tags$caption(class="contpic", style = 'caption-side: top', '境內基金'),
                  options =list(dom = 'tpl',
                                pageLength = 5,
                                columnDefs = list(list(className = 'dt-center', targets = "_all")),
                                initComplete = JS(
                                  "function(settings, json) {",
                                  "$(this.api().table().header()).css({'background-color': '#39ac73', 'color': '#fff'});",
                                  "}")))
  })
  # 關鍵字搜尋 - 國外
  temOPoolTable <- eventReactive(input$send, {
    library(DT)
    source('D05S01_GenPool02_O.R')
    temOPool = GenPool02_O(input$name, input$keyword, input$andor)
    DT::datatable(temOPool, class = 'cell-border stripe',  rownames = c(1:nrow(temOPool)),
                  caption = htmltools::tags$caption(class="contpic", style = 'caption-side: top', '境外基金'),
                  options =list(dom = 'tpl',
                                pageLength = 5,
                                columnDefs = list(list(className = 'dt-center', targets = "_all")),
                                initComplete = JS(
                                  "function(settings, json) {",
                                  "$(this.api().table().header()).css({'background-color': '#009999', 'color': '#fff'});",
                                  "}"))) 
  })
  
  # 資產池 & 策略
  RunONE <- reactive({
    withProgress(message = 'calculating...', value = 0, {
      source('D05S01_GenPool01_D.R')
      source('D05S01_GenPool01_O.R')
      GenPool01_D(input$name, input$date, input$scale, input$sd, input$return, input$sharpe, input$num, input$alpha, input$pret)
      GenPool01_O(input$name, input$date, input$scale, input$sd, input$return, input$sharpe, input$num, input$alpha, input$pret)
      source('D06S03_Strategy_T.R')
      source('D07S04_PlotData_T.R')
      incProgress(1/10, detail = paste("finished", "100%"))
      
      # 資產池表格
      output$PoolTable <- DT::renderDataTable({
        source('S02_PoolTable_T.R')
        PoolTable
      })
    })
  })
  RunTWO <- reactive({
    withProgress(message = 'calculating...', value = 0, {
      source('D06S03_Strategy_T.R')
      source('D07S04_PlotData_T.R')
      incProgress(1/10, detail = paste("finished", "100%"))
      
      # 資產池表格
      output$PoolTable <- DT::renderDataTable({
        source('S02_PoolTable_T.R')
        PoolTable
      })
    })
  })

  # ---------------------- Output ---------------------- #
  
  # 關鍵字搜尋結果
  output$temDPoolTable <- DT::renderDataTable({
    temDPoolTable()
  })
  
  output$temOPoolTable <- DT::renderDataTable({
    temOPoolTable()
  })

  # 權重圓餅圖
  output$WEqual <- renderPlotly({
    source('S05_GenTablePlot_T.R')
    GenPie("Equal.Weight")
  })
  output$WVol <- renderPlotly({
    source('S05_GenTablePlot_T.R')
    GenPie("Volatility.Weighted")
  })
  output$WMomentum <- renderPlotly({
    source('S05_GenTablePlot_T.R')
    GenPie("Momentum")
  })
  output$WCombo <- renderPlotly({
    source('S05_GenTablePlot_T.R')
    GenPie("Combo")
  })
  output$WAAA <- renderPlotly({
    source('S05_GenTablePlot_T.R')
    GenPie("AAA")
  })
  
  # 權重表格
  observeEvent(input$lookE,{
    source('S05_GenTablePlot_T.R')
    output$WETable <- DT::renderDataTable(GenWTable("Equal.Weight"))
  })
  observeEvent(input$lookV,{
    source('S05_GenTablePlot_T.R')
    output$WVTable <- DT::renderDataTable(GenWTable("Volatility.Weighted"))
  })
  observeEvent(input$lookM,{
    source('S05_GenTablePlot_T.R')
    output$WMTable <- DT::renderDataTable(GenWTable("Momentum"))
  })
  observeEvent(input$lookC,{
    source('S05_GenTablePlot_T.R')
    output$WCTable <- DT::renderDataTable(GenWTable("Combo"))
  })
  observeEvent(input$lookA,{
    source('S05_GenTablePlot_T.R')
    output$WATable <- DT::renderDataTable(GenWTable("AAA"))
  })
  
  # 回測說明
  output$note <- renderText({
    load('TData/PlotData.RData')
    note = paste("回測期間：", PlotData$Date, "。",
                 "benchmark：回測期間報酬表現前25%的基金以等比方式組成的投資組合。", "")
    note
  })
  
  # 本金走勢圖
  output$Trend <- renderDygraph({
    library(dygraphs)
    load('TData/PlotData.RData')

    equity = PlotData$Trend
    
    Colors = c('rgb(204,0,204)', 'rgb(204,0,0)', 'rgb(204,153,0)', 'rgb(0,153,51)', 'rgb(0,102,153)', 'rgb(128,128,128)')

    dygraph(equity, main = "Equity of Strategies") %>%
      dySeries("benchmark", strokeWidth = 2, strokePattern = "dashed") %>%
      dyLegend(show = 'always', width = 800) %>%
      dyRangeSelector(height = 20, strokeColor = "") %>%
      dyHighlight(highlightCircleSize = 5,
                  highlightSeriesBackgroundAlpha = 0.2,
                  hideOnMouseOut = T) %>%
      dyOptions(colors = Colors)
  })

  # 近四季報酬長條圖
  output$Bar <- renderPlotly({
    library(plotly)
    load('TData/PlotData.RData')

    QRet = PlotData$Bar

    Colors = c('rgba(204,0,204,0.7)', 'rgba(204,0,0,0.7)', 'rgba(204,153,0,0.7)', 'rgba(0,153,51,0.7)', 'rgba(0,102,153,0.7)', 'rgba(128,128,128,0.7)')
    Colorsl = c('rgba(204,0,204,0.3)', 'rgba(204,0,0,0.3)', 'rgba(204,153,0,0.3)', 'rgba(0,153,51,0.3)', 'rgba(0,102,153,0.3)', 'rgba(128,128,128,0.3)')

    p <- plot_ly(QRet, x = ~YQ, y = ~Equal.Weight, type = 'bar', width = 1000,
                 name = 'Equal.Weight', marker = list(color = Colors[1],
                                                      line = list(color = Colorsl[1], width = 2)),
                 hoverinfo = "y+text") %>%
      add_trace(y = ~Volatility.Weighted, name = 'Volatility.Weighted', marker = list(color = Colors[2],
                                                                                      line = list(color = Colorsl[2], width = 2))) %>%
      add_trace(y = ~Momentum, name = 'Momentum', marker = list(color = Colors[3],
                                                                line = list(color = Colorsl[3], width = 2))) %>%
      add_trace(y = ~Combo, name = 'Combo', marker = list(color = Colors[4],
                                                          line = list(color = Colorsl[4], width = 2))) %>%
      add_trace(y = ~AAA, name = 'AAA', marker = list(color = Colors[5],
                                                      line = list(color = Colorsl[5], width = 2))) %>%
      add_trace(y = ~benchmark, name = 'benchmark', marker = list(color = Colors[6],
                                                             line = list(color = Colorsl[6], width = 2))) %>%
      layout(title = "Quarterly Returns of Strategies", xaxis = list(title = ""), yaxis = list(title = "Return"),
             paper_bgcolor = 'rgba(255,255,255,0)', plot_bgcolor = 'rgba(255,255,255,0)') %>%
      config(displayModeBar = F)
  })

  # 報酬與風險 氣泡圖 or 績效表
  output$Bubble <- renderPlotly({
    library(plotly)
    load('TData/PlotData.RData')

    BubbleInfo = PlotData$Bubble
    Colors = c("#990033", "#993366", "#cc3399", "#6666ff", "#336699", "#003366")

    p <- plot_ly(BubbleInfo, x = ~Volatility, y = ~Cagr, size = ~Sharpe, color = ~Strategy, type = 'scatter', width = 1000,
                 colors = Colors, hoverinfo = 'text+name',
                 text = ~paste('Cagr:', Cagr, '<br>Volatility:', Volatility, '<br>Sharpe:', Sharpe, '<br>MaxDD:', MaxDD),
                 mode = 'markers',
                 marker = list(opacity = 0.8, sizemode = 'diameter')) %>%
      layout(title = "Return and Risk",
             xaxis = list(range = c(min(BubbleInfo$Volatility[1:6])-1, max(BubbleInfo$Volatility[1:6])*1.2)),
             yaxis = list(range = c(min(BubbleInfo$Cagr[1:6])-1, max(BubbleInfo$Cagr[1:6])*1.5)),
             paper_bgcolor = 'rgba(255,255,255,0)', plot_bgcolor = 'rgba(255,255,255,0)') %>%
      config(displayModeBar = F)
  })
  
  output$BubbleT <- DT::renderDataTable({
    library(DT)
    load('TData/PlotData.RData')
    TableInfo = PlotData$Bubble[1:6,]
    DT::datatable(TableInfo, class = 'cell-border stripe',  rownames = NULL,
                  caption = htmltools::tags$caption(style = 'caption-side: bottom',
                                                    'MaxDD：最大回撤；Volatility：年化標準差；Cagr：年化報酬率；Sharpe：夏普比率'),
                  options =list(dom = 't',
                                columnDefs = list(list(className = 'dt-center', targets = 0:4)),
                                initComplete = JS(
                                  "function(settings, json) {",
                                  "$(this.api().table().header()).css({'background-color': '#999966', 'color': '#fff'});",
                                  "}")))
  })
  
  output$ui_3 <- renderUI({
    if(input$PicTab=="氣泡圖")
    {
      ui_3 = tagList(plotlyOutput("Bubble"),
                     p(class="contspan", "說明："),
                     p(class="cont", "圓圈大小：Sharpe大小；顏色：MaxDD絕對值，由大到小依圖例順序排列。"))
    }
    if(input$PicTab=="績效表")
    {
      ui_3 = DT::dataTableOutput("BubbleT")
    }
    ui_3
  })
  
  # 每月報酬狀況熱圖
  output$Heatmap <- DT::renderDataTable({
    library(DT)
    load('TData/PlotData.RData')
    
    strategy = input$strategy
    RTable = PlotData$Heatmap[[strategy]]
    
    DT::datatable(RTable, class = 'cell-border stripe',
                          caption = htmltools::tags$caption(style = 'caption-side: bottom', paste(strategy,'的月報酬狀況')),
                          options = list(dom = 't', pageLength = nrow(RTable),
                                         columnDefs = list(list(className = 'dt-center', targets = 0:12)),
                                         initComplete = JS(
                                            "function(settings, json) {",
                                            "$(this.api().table().header()).css({'background-color': '#005580', 'color': '#fff'});",
                                            "}"))) %>%
      formatStyle(colnames(RTable)[1:12], backgroundColor = styleInterval(0, c("rgba(230,0,0,0.8)","rgba(128,255,223,0.8)"))) %>%
      formatStyle(" ", backgroundColor = '#005580', color = '#fff') %>%
      formatStyle("Year", backgroundColor = styleInterval(0, c("rgba(255,166,77,0.8)","rgba(77,255,210,0.8)")))
  })
  
}