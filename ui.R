
if(!require("shiny")) install.packages("shiny")
if(!require("shinyjs")) install.packages("shinyjs")
if(!require("V8")) install.packages("V8")
if(!require("shinyBS")) install.packages("shinyBS")
if(!require("DT")) install.packages("DT")
if(!require("plotly")) install.packages("plotly")
if(!require("RColorBrewer")) install.packages("RColorBrewer")
if(!require("dygraphs")) install.packages("dygraphs")
library(shiny)
library(shinyjs)
library(V8)
library(shinyBS)
library(DT)
library(plotly)
library(RColorBrewer)
library(dygraphs)


jsResetCode <- "shinyjs.reset = function() {history.go(0)}"

fillPage(
  
  useShinyjs(),
  extendShinyjs(text = jsResetCode),
  
  tags$style(type = "text/css",
             "#background { width:100%; height:100%; overflow:scroll; background-image:url(back.jpg); background-repeat: no-repeat; }",
             "#head { width:100%; height:8%; position: fixed; z-index:1; background-color:rgba(0,153,153,0.8);
                      box-shadow: 0 4px 8px 0 rgba(0,0,0,0.2), 0 6px 20px 0 rgba(0,0,0,0.19); }",
             "#title { height:10%; margin-top:7%; padding-left:15%; color:#008080; font-size:50px; font-weight:900;
                       font-family:verdana; text-shadow:2px 2px rgba(0,128,128,0.5); }",
             ".subbackground { width:70%; margin-left:15%; margin-bottom:10%; margin-top:1%; padding:15px;
                               background-color:rgba(255,255,255,0.7);border:10px double #ff9900; border-radius:10px;
                               box-shadow: 5px 5px 8px 0 rgba(0,0,0,0.2);5px 5px 8px 0 rgba(0,0,0,0.19); }",
             
             "#robotdiv { text-align: center; }",
             "#startdiv { text-align: center; }",
             "#choicediv { width:70%; margin-left:15%; margin-top:5%; padding:50px; }",
             ".keydiv { width:30%; float:left; }",
             "#outdiv { width:90%; height:70%; margin-left:5%; margin-top:1%; z-index:-1; }",
             ".itmbackground { width:100%; paddding-bottom:5%; background-color:rgba(255,255,255,0.7); }",
             ".pic { width:90%; margin-left:2%; margin-right:2%; }",
             ".clear { clear:both; }"),
  # 文字
  tags$style(type = "text/css",
             "#ID {color:red;}",
             ".word { color:#004d4d; font-size:20px; font-weight:900; }",
             ".subword { fon-size:15px; font-weight:900; }",
             "#choiceword { color:#004d4d; font-size:30px; font-weight:900; }",
             "#keyintro { color:#669999; font-size:12px; }",
             ".default { color: #990000; font-weight:900; }",
             ".conttitle { font-size:20px; font-weight:900; color:#004d4d; padding-top:10px; padding-left:10px; }",
             ".cont { color:#5c8a8a; font-size:17px; font-weight:900; padding-left:10px; }",
             ".contspan { color:red; font-size:17px; font-weight:900; padding-left:10px; }",
             ".contpic { color:#527a7a; font-size:17px; font-weight:900; padding-left:10px; }",
             "#note { padding-left:60px; }"),
  # 按鈕
  tags$style(type = "text/css",
             "#home { height:100%; margin-right:20px; float:right; background-color:rgba(0,153,153,0); border-style:none; }",
             "#page0next { color:#669999; font-size:150%; font-weight:900; padding:10px 15px 10px 15px; border:5px double #669999;
                           box-shadow: 5px 5px 8px 0 rgba(0,0,0,0.2);5px 5px 8px 0 rgba(0,0,0,0.19); }",
             "#page1next { color:#004d4d; font-size:150%; font-weight:700; float:right; border:5px double #669999;
                           box-shadow: 5px 5px 8px 0 rgba(0,0,0,0.2);5px 5px 8px 0 rgba(0,0,0,0.19); }",
             "#page2go3 { color:#b38600; font-size:250%; font-weight:900; margin-top:3%; margin-left:15%; padding:10px 15px 10px 15px; border:10px double #cc9900; border-radius:10px;
                          box-shadow: 5px 5px 8px 0 rgba(0,0,0,0.2);5px 5px 8px 0 rgba(0,0,0,0.19); }",
             "#page2go5 { color:#b366ff; font-size:250%; font-weight:900; margin-top:3%; margin-left:15%; padding:10px 33px 10px 33px; border:10px double #cc99ff; border-radius:10px;
                          box-shadow: 5px 5px 8px 0 rgba(0,0,0,0.2);5px 5px 8px 0 rgba(0,0,0,0.19); }",
             ".look { color:#5c8a8a; font-size:17px; font-weight:900; float:right; margin-right:5%; margin-bottom:50px; border:3px solid #5c8a8a; border-radius:10px; }"),
  
  # 提醒視窗&進度軸
  tags$style("#Info1 .modal-dialog { position:fixed; top:30%; left:30%; }",
             ".shiny-notification { text-align: center; padding-top:30px; font-size:25px;
                                    position:fixed; top:40% ;left:30%; width:40%; height:15%;
                                    border:5px solid #476b6b; background-color:rgba(102,153,153,0.7);
                                    box-shadow: 5px 5px 8px 0 rgba(0,0,0,0.2);5px 5px 8px 0 rgba(0,0,0,0.19); }"),
  
  # 篩選結果標籤
  tags$style(HTML("
                  .tabbable > .nav > li > a {font-size:20px; font-weight:500}
                  .tabbable > .nav > li > a[data-value='Equal.Weight'] {background-color:rgba(204,0,204,0.7); color:white}
                  .tabbable > .nav > li > a[data-value='Volatility.Weighted'] {background-color:rgba(204,0,0,0.7); color:white}
                  .tabbable > .nav > li > a[data-value='Momentum'] {background-color:rgba(204,153,0,0.7); color:white}
                  .tabbable > .nav > li > a[data-value='Combo'] {background-color:rgba(0,153,51,0.7); color:white}
                  .tabbable > .nav > li > a[data-value='AAA'] {background-color:rgba(0,102,153,0.7); color:white}

                  .tabbable > .nav > li > a[data-value='本金走勢'] {background-color:rgba(230,255,249,0.7); color:#5c8a8a}
                  .tabbable > .nav > li > a[data-value='每月報酬狀況'] {background-color:rgba(204,255,243,0.7); color:#5c8a8a}
                  .tabbable > .nav > li > a[data-value='近四季報酬率'] {background-color:rgba(179,255,237,0.7); color:#5c8a8a}
                  .tabbable > .nav > li > a[data-value='報酬與風險'] {background-color:rgba(153,255,231,0.7); color:#5c8a8a}
                  .tabbable > .nav > li[class=active] > a {background-color:#00cc99; font-weight:900; color:white}

                  .col-sm-2 > .nav > li > a {font-size:20px; font-weight:500}
                  .col-sm-2 > .nav > li > a[data-value='資產池'] {background-color:#ebfafa; color:#5c8a8a}
                  .col-sm-2 > .nav > li > a[data-value='策略介紹 & 配置'] {background-color:#d6f5f5; color:#5c8a8a}
                  .col-sm-2 > .nav > li > a[data-value='策略績效比較'] {background-color:#c2f0f0; color:#5c8a8a}
                  .col-sm-2 > .nav > li[class=active] > a {background-color:#33cccc; font-weight:900; color:white}
                  
                  .tabbable > .nav > li[class=active] > a[data-value='Equal.Weight'] {background-color:rgb(204,0,204); font-weight:900}
                  .tabbable > .nav > li[class=active] > a[data-value='Volatility.Weighted'] {background-color:rgb(204,0,0); font-weight:900}
                  .tabbable > .nav > li[class=active] > a[data-value='Momentum'] {background-color:rgb(204,153,0); font-weight:900}
                  .tabbable > .nav > li[class=active] > a[data-value='Combo'] {background-color:rgba(0,153,51); font-weight:900}
                  .tabbable > .nav > li[class=active] > a[data-value='AAA'] {background-color:rgb(0,102,153); font-weight:900}")),
  
  div(id = "background",
  div(id = "head", actionButton("home", label = icon("home", "fa-3x"))),
  div(id = "title", textOutput("pagename")),
  
  # 首頁
  conditionalPanel("output.pagename == 'FinLab Robot'",
                   br(),
                   div(id="robotdiv", img(src = "chicken.png", height = 350, width = 750)),
                   br(),
                   div(id="startdiv", actionButton("page0next", label = "開始建立基金組合"))),
  # 會員資料
  conditionalPanel("output.pagename == '會員資料'",
                   div(class="subbackground",
                       p(class = "word", "請輸入姓名："),
                       textInput("name", label = NULL),
                       textOutput("ID"),
                       actionButton("page1next", label = "下一步"))),
  # 篩選方式
  conditionalPanel("output.pagename == '篩選方式'",
                   div(id="choicediv",
                       p(id="choiceword", "請選擇篩選方式："),
                       actionButton("page2go3", label = "特定流程篩選"),
                       actionButton("page2go5", label = "關鍵字搜尋"))),
  # 第一階段
  conditionalPanel("output.pagename == '第一階段'",
                   div(class="subbackground", uiOutput("ui_1"),
                       checkboxInput("default_1", label = p(class="default", "預設 (基金於以上四項條件排名之總平均為前25%)"), value = F, width = '100%'),
                       actionButton("page3prev", label = "上一步"),
                       actionButton("page3next", label = "下一步"))),
  # 第二階段
  conditionalPanel("output.pagename == '第二階段'",
                   div(class="subbackground", uiOutput("ui_2"),
                       checkboxInput("default_2", label = p(class="default", "預設 (同時具備以上兩項條件)"), value = F),
                       actionButton("page4prev", label = "上一步"),
                       actionButton("page4next", label = "完成"))),
  # 關鍵字
  conditionalPanel("output.pagename == '關鍵字搜尋'",
                   div(class="subbackground",
                       p(class="word", "挑選出現關鍵字的基金納入資產池作為投資標的。"),
                       br(),
                       p(class="subword", "選擇邏輯運算子："),
                       radioButtons("andor", label = NULL, choices = list("or" = "or", "and" = "and"), selected = "or", inline = TRUE),
                       br(),
                       p(class="subword", "請輸入關鍵字："),
                       div(class="keydiv", textInput("keyword", label = NULL)),
                       div(class="keydiv", actionButton("send", label = "確定")),
                       div(class="clear"),
                       textOutput("keyintro"),
                       DT::dataTableOutput("temDPoolTable"),
                       DT::dataTableOutput("temOPoolTable"),
                       br(),
                       actionButton("page5prev", label = "上一步"),
                       actionButton("page5next", label = "完成"))),
  # 篩選結果
  conditionalPanel("output.pagename == ' '",
                   div(id="outdiv", navlistPanel(widths = c(2,10), well = F, "",
                                                 tabPanel("資產池", div(class="itmbackground",
                                                                        p(class="contpic", "根據您設定的篩選條件，所產生的資產池如下："),
                                                                        DT::dataTableOutput("PoolTable"))),

                                                 tabPanel("策略介紹 & 配置",
                                                          tabsetPanel(tabPanel("Equal.Weight", div(class="itmbackground",
                                                                                                   p(class="conttitle", "等比權重 (Equal Weight Strategy)"),
                                                                                                   p(class="cont", "將資產池內的標的給予相等權重的投資組合。每季進行一次投資組合再平衡，將標的權重恢復為均權狀態。"),
                                                                                                   p(class="contspan", "優點："),
                                                                                                   p(class="cont", "不會過度投資於某一基金，因此能達到風險分散之目的。"),
                                                                                                   br(),
                                                                                                   p(class="contpic", "根據您的資產池，此策略計算後的投資組合成分為："),
                                                                                                   br(),
                                                                                                   div(class="pic", plotlyOutput("WEqual")),
                                                                                                   actionButton(class="look", "lookE", label = "查看各基金比重"),
                                                                                                   div(class="clear"),
                                                                                                   bsModal("EW", "Equal Weight Strategy's Weight", "lookE", size = "large", DT::dataTableOutput("WETable")))),
                                                                      tabPanel("Volatility.Weighted", div(class="itmbackground",
                                                                                                          p(class="conttitle", "波動加權 (Inverse of Volatility Strategy)"),
                                                                                                          p(class="cont", "依資產池標的之歷史波動度大小配置權重的投資組合。過去波動度越低者給予較高的比重，每季亦依照波動度進行再平衡。"),
                                                                                                          p(class="contspan", "優點："),
                                                                                                          p(class="cont", "波動度小，表現較為穩定。"),
                                                                                                          br(),
                                                                                                          p(class="contpic", "根據您的資產池，此策略計算後的投資組合成分為："),
                                                                                                          br(),
                                                                                                          div(class="pic", plotlyOutput("WVol")),
                                                                                                          actionButton(class="look", "lookV", label = "查看各基金比重"),
                                                                                                          div(class="clear"),
                                                                                                          bsModal("VW", "Inverse of Volatility Strategy's Weight", "lookV", size = "large", DT::dataTableOutput("WVTable")))),
                                                                      tabPanel("Momentum", div(class="itmbackground",
                                                                                               p(class="conttitle", "動能均權 (Momentum Strategy)"),
                                                                                               p(class="cont", "從資產池中挑選具有較強動能的基金，以等比權重的方式組成的投資組合。每季依動能重新挑選基金進行再平衡。"),
                                                                                               p(class="contspan", "優點："),
                                                                                               p(class="cont", "可透過資產報酬的動能現象獲利。"),
                                                                                               br(),
                                                                                               p(class="contpic", "根據您的資產池，此策略計算後的投資組合成分為："),
                                                                                               br(),
                                                                                               div(class="pic", plotlyOutput("WMomentum")),
                                                                                               actionButton(class="look", "lookM", label = "查看各基金比重"),
                                                                                               div(class="clear"),
                                                                                               bsModal("MW", "Momentum Strategy's Weight", "lookM", size = "large", DT::dataTableOutput("WMTable")))),
                                                                      tabPanel("Combo", div(class="itmbackground",
                                                                                            p(class="conttitle", "混合型 (Combination of Inverse of Volatility and Momentum Strategy)"),
                                                                                            p(class="cont", "為動能均權策略及波動加權策略之結合。其根據標的的波動度對動能投資組合之權重部位進行配置，每季進行投資組合再平衡。"),
                                                                                            p(class="contspan", "優點："),
                                                                                            p(class="cont", "同時考慮報酬的動能效果與波動的時變性及群聚現象。在追求高動能下，亦控制其風險。"),
                                                                                            br(),
                                                                                            p(class="contpic", "根據您的資產池，此策略計算後的投資組合成分為："),
                                                                                            br(),
                                                                                            div(class="pic", plotlyOutput("WCombo")),
                                                                                            actionButton(class="look", "lookC", label = "查看各基金比重"),
                                                                                            div(class="clear"),
                                                                                            bsModal("CW", "Combination of Inverse of Volatility and Momentum Strategy's Weight", "lookC", size = "large", DT::dataTableOutput("WCTable")))),
                                                                      tabPanel("AAA", div(class="itmbackground",
                                                                                          p(class="conttitle", "適應性資產配置 (Adaptive of Asset Allocation Strategy)"),
                                                                                          p(class="cont", "利用動能挑選標的，並以能極小化投資組合風險之權重組成的投資組合。每季進行投資組合再平衡。"),
                                                                                          p(class="contspan", "優點："),
                                                                                          p(class="cont", "具有高動能、低風險的特性。"),
                                                                                          br(),
                                                                                          p(class="contpic", "根據您的資產池，此策略計算後的投資組合成分為："),
                                                                                          br(),
                                                                                          div(class="pic", plotlyOutput("WAAA")),
                                                                                          actionButton(class="look", "lookA", label = "查看各基金比重"),
                                                                                          div(class="clear"),
                                                                                          bsModal("AW", "Adaptive of Asset Allocation Strategy's Weight", "lookA", size = "large", DT::dataTableOutput("WATable")))))),
                                                 
                                                 tabPanel("策略績效比較",
                                                          tabsetPanel(tabPanel("本金走勢", div(class="itmbackground",
                                                                                               br(),
                                                                                               div(class="pic", dygraphOutput("Trend")),
                                                                                               textOutput("note"))),
                                                                      tabPanel("每月報酬狀況", div(class="itmbackground",
                                                                                                   br(),
                                                                                                   radioButtons("strategy", label = "策略名稱：",
                                                                                                                choices = list("Equal.Weight" = "Equal.Weight", "Volatility.Weighted" = "Volatility.Weighted",
                                                                                                                               "Momentum" = "Momentum", "Combo" = "Combo",
                                                                                                                               "AAA" = "AAA", "benchmark" = "benchmark"), inline = TRUE),
                                                                                                   br(),
                                                                                                   div(class="pic", DT::dataTableOutput("Heatmap")))),
                                                                      tabPanel("近四季報酬率", div(class="itmbackground",
                                                                                                   br(),
                                                                                                   div(class="pic", plotlyOutput("Bar")))),
                                                                      tabPanel("報酬與風險", div(class="itmbackground",
                                                                                                 br(),
                                                                                                 selectInput("PicTab", label = NULL, choices = list("氣泡圖" = "氣泡圖", "績效表" = "績效表"), selected = "氣泡圖"),
                                                                                                 uiOutput("ui_3"),
                                                                                                 br())))))))
  
  
  
))
