
rm(list=ls())

if(!require("TTR")) install.packages("TTR")
if(!require("xts")) install.packages("xts")
if(!require("lubridate")) install.packages("lubridate")
library(TTR)
library(xts)
library(lubridate)
library(SIT)

source("Fun/ChangeCol.R")
load("TData/models.RData")

Database_D <- readRDS("DData/demostic_U02database")
Database_O <- readRDS("OData/offshore_U02database")

strategy_Name = rownames(models$nextday.Weight)

# Date
start = min(which(models$benchmark$equity!=1, arr.ind = T)[,1])
startDate = index(models$benchmark$equity)[start]
endDate = tail(index(models$benchmark$equity), 1)

DateInfo = paste(startDate, "~", endDate, "")

# -------------------------------------- Data Processing ------------------------------------ #

# Table & Pie & Trend & Heatmap
temV_Name = c('ISIN','Name','Native','New_area','New_type','Areatype')

Data = rbind(Database_D[,temV_Name], Database_O[,temV_Name])

tem = models$nextday.Weight

w <- list()
PieInfo <- list()
TableInfo <- list()
equity <- NULL
Ret <- NULL
for(i in strategy_Name)
{
  if(i!="benchmark")
  {
    temticker = colnames(tem)[tem[i,]!=0]
    temloc = match(temticker, Data$ISIN)
    w[[i]] = data.frame(ISIN = temticker, Name = Data$Name[temloc],
                        Weight = as.numeric(tem[i,temticker]), Native = Data$Native[temloc],
                        New_area = Data$New_area[temloc], New_type = Data$New_type[temloc],
                        Areatype = Data$Areatype[temloc])
    temW = w[[i]]
    temPie = c(tapply(temW$Weight, temW$New_area, sum))
    Pie1 = data.frame(labels = names(temPie), values = temPie)
    temPie = c(tapply(temW$Weight, temW$New_type, sum))
    Pie2 = data.frame(labels = names(temPie), values = temPie)
    temPie = c(tapply(temW$Weight, temW$Native, sum))
    Pie3 = data.frame(labels = names(temPie), values = temPie)
    
    PieInfo[[i]] = list(Pie1=Pie1, Pie2=Pie2, Pie3=Pie3)  #*
    
    temT = as.matrix(ChangeCol(as.character(temW$Name), round(temW$Weight*100,3), r = ceiling(nrow(temW)/2)))
    colnames(temT) = rep(c("Name","Weight(%)"), ncol(temT)/2)
    temT = ifna(temT, "")
    
    TableInfo[[i]] = temT  #*
    
    rm(temticker, temloc, temW, temPie, Pie1, Pie2, Pie3,temT)
  }
  
  equity = cbind(equity, models[[i]]$equity)
  
  Ret = cbind(Ret, models[[i]]$ret)
  
}
colnames(equity) <- colnames(Ret) <- strategy_Name

equity = equity[(start-1):nrow(equity),]       #*
Ret = Ret[start:nrow(Ret),]

Ret = data.frame(Date = index(Ret), Ret)

rm(temV_Name, tem)

# Heatmap
RTable <- list()
for(i in strategy_Name)
{
  tem = plotbt.monthly.table(models[[i]]$equity)
  tem = tem[-nrow(tem),]
  tem = tem[,-ncol(tem)]
  mode(tem) = "numeric"
  tem = tem[-(apply(is.na(tem),1,sum)==12),]
  tem[which(is.na(tem))] = 0
  RTable[[i]] = tem               #*
  
  rm(tem)
}

# Bubble
temV_Name = c("MaxDD","Volatility","Cagr","Sharpe")
models$nextday.Weight = NULL

tem = data.frame(plotbt.strategy.sidebyside(models, return.table=T))
tem = as.matrix(tem[temV_Name,])
tem = matrix(as.numeric(tem), nrow = nrow(tem),
             dimnames = list(row.names(tem), colnames(tem)))

BubbleInfo = data.frame(Strategy = colnames(tem), t(tem))
BubbleInfo = BubbleInfo[order(BubbleInfo$MaxDD),]
BubbleInfo$Strategy = factor(BubbleInfo$Strategy, levels = as.character(BubbleInfo$Strategy))

tem = BubbleInfo
tem[,c("Volatility","Cagr")] = -5
tem$Sharpe = min(BubbleInfo$Sharpe) - 0.01
tem$MaxDD = 0

BubbleInfo = rbind(BubbleInfo, tem)  #*

rm(temV_Name, tem)

# Bar
tem = Ret
tem$YQ = paste(lubridate::year(tem$Date), "Q", lubridate::quarter(tem$Date), sep = "")
Q = tail(unique(tem$YQ), 4)

QRet <- NULL
for(i in Q)
{
  temV = apply(tem[tem$YQ==i,strategy_Name], 2, function(x){sum(x, na.rm = T)})
  
  QRet = rbind(QRet, t(temV))
  
  rm(temV)
}
QRet = data.frame(QRet)
QRet$YQ = Q             #*

rm(tem)

# ------------------------------------------ Output ----------------------------------------- #

PlotData = list(Pie = PieInfo, Table = TableInfo, Trend = equity,
                Heatmap = RTable, Bar = QRet, Bubble = BubbleInfo, Date = DateInfo)

save(PlotData, file="TData/PlotData.RData")
