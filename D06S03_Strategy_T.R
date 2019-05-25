
rm(list=ls())

if(!require("xts")) install.packages("xts")
if(!require("quantmod")) install.packages("quantmod")
library(xts)
library(quantmod)
library(SIT)

# Load Data
load('DData/demostic_NAV.RData')

load('OData/offshore_NAV.RData')

Pool_D <- as.character(as.matrix(read.csv('DData/demostic_Poollist.csv', stringsAsFactors = F)))
Pool_O <- as.character(as.matrix(read.csv('OData/offshore_Poollist.csv', stringsAsFactors = F)))
Fee_D <- read.csv('DData/demostic_Fee.csv')
Fee_O <- read.csv('OData/offshore_Fee.csv')

Data = c(demostic_NAV[Pool_D], offshore_NAV[Pool_O])
Fee = rbind(Fee_D, Fee_O)
Tickers = c(Pool_D, Pool_O)

if(anyNA(Tickers)==T)
{
  Data[is.na(names(Data))] = NULL
  Fee = Fee[-unique(which(is.na(Fee),arr.ind = T)[,1]),]
  Tickers = Tickers[-which(is.na(Tickers))]
}

# --- parameter
NoS = c(1, length(Tickers))    # 用資產池第NoS[1]~第NoS[2]檔基金跑策略 
period <- 'quarters'           # frequency of rebalance
SD = "2004-12-01"
#ED = "2018-01-01"

Top = 0.11                     # 挑資產池中動能前Top*100%的基金
top = 0.25                     # 挑資產池中回測期間平均報酬前Top*100%的基金: benchmark
n_times = 0.5                  # n_times單位 (ex:月=22) 的天數
n.mom = n_times*22	           # length of momentum look back
n.vol = n_times*22 	           # length of volatility look back

# -------------------------------------- Data Processing ------------------------------------- #

Dat = list()
for (i in NoS[1]:NoS[2]) {
  Dat[[Tickers[i]]] = Data[[i]][index(Data[[i]])>=SD]
}

data <- as.environment(Dat)
bt.prep(data, align="keep.all")

UFee = Fee$Fee_handling[match(data$symbolnames, Fee$ISIN)]
cost = matrix(rep(UFee, nrow(data$weight)), ncol = length(UFee), byrow = T)

commission = list(cps = 0, fixed = 0.0, percentage = 0.006)
# ---------------------------------------- Strategies ---------------------------------------- #

prices = data$prices

n.top = round(ncol(prices)*Top)	    	# number of momentum positions

# find period ends
period.ends = endpoints(prices, period)
period.ends = period.ends[period.ends > 0]

models = list()
nextday.Weight = NULL



#***********************************************************************************************
# Equal Weight (Equal.Weight)
#***********************************************************************************************
data$weight[] = NA

n = ncol(prices)
data$weight[period.ends,] = ntop(prices[period.ends,], n)   

models$Equal.Weight = bt.run.share(data, clean.signal = F, commission = commission)

nextday.Weight = rbind(nextday.Weight, tail(data$weight, 1))


#***********************************************************************************************
# Inverse of Volatility (Volatility.Weighted)
#***********************************************************************************************
data$weight[] = NA
data$weight[period.ends,] = 0

ret.log = bt.apply.matrix(prices, ROC, type='continuous')
for( i in period.ends[period.ends >= n.vol] ) {
  
  hist = ret.log[(i - n.vol + 1):i, ]
  vol = apply(coredata(hist), 2, sd)
  
  # require all assets to have full price history
  include.index = (count(hist)==n.vol)
  
  index = (vol > 0 ) & include.index
  n = sum(index)
  
  if(n > 0) {
    vol = vol[index]
    adj.vol = 1/ vol
    data$weight[i,index] = adj.vol / sum(adj.vol, na.rm=T)
  }
  rm(hist,vol)
}

models$Volatility.Weighted = bt.run.share(data, clean.signal = F, commission = commission)

nextday.Weight = rbind(nextday.Weight, tail(data$weight, 1))


#***********************************************************************************************
# Momentum (Momentum)
#***********************************************************************************************
data$weight[] = NA

momentum = prices / mlag(prices, n.mom)
data$weight[period.ends,] = ntop(momentum[period.ends,], n.top)

models$Momentum = bt.run.share(data, clean.signal = F, commission = commission)

nextday.Weight = rbind(nextday.Weight, tail(data$weight, 1))


#***********************************************************************************************
# Combination of Inverse of Volatility and Momentum  (Combo)
#***********************************************************************************************
data$weight[] = NA

weight = ntop(momentum[period.ends,], n.top) * adj.vol
data$weight[period.ends,] = weight / rowSums(weight, na.rm=T)

models$Combo = bt.run.share(data, clean.signal = F, commission = commission)

nextday.Weight = rbind(nextday.Weight, tail(data$weight, 1))


#***********************************************************************************************
# Adaptive of Asset Allocation  (AAA)
#***********************************************************************************************
data$weight[] = NA

weight = NA * prices
weight[period.ends,] = ntop(momentum[period.ends,], n.top)

for( i in period.ends[period.ends >= n.vol] ) {
  hist = ret.log[ (i - n.vol + 1):i, ]
  
  # require all assets to have full price history
  include.index = (count(hist)==n.vol)      
  
  # also only consider assets in the Momentum Portfolio
  index = ( weight[i,] > 0 ) & include.index
  n = sum(index)
  
  if(n > 0) {					
    hist = hist[ , index]
    
    # create historical input assumptions
    ia = create.historical.ia(hist, 252)
    s0 = apply(coredata(hist), 2, sd)       
    ia$cov = cor(coredata(hist), use='complete.obs', method='pearson') * (s0 %*% t(s0))
    
    # create constraints: 0<=x<=1, sum(x) = 1
    constraints = new.constraints(n, lb = 0, ub = 1)
    constraints = add.constraints(rep(1, n), 1, type = '=', constraints)       
    
    # compute minimum variance weights				            
    weight[i,] = 0        
    weight[i,index] = min.risk.portfolio(ia, constraints)
  }
}

data$weight[period.ends,] = weight[period.ends,]

models$AAA = bt.run.share(data, clean.signal = F, commission = commission)

nextday.Weight = rbind(nextday.Weight, tail(data$weight, 1))


#***********************************************************************************************
# Benchmark (Top_25)
#***********************************************************************************************
data$weight[] = NA
data$weight[period.ends,] = 0

#choose the top 25% names
quarter_return <- c()
for(i in 1:ncol(data$prices)){
  a <- quarterlyReturn(data$prices[,i])
  quarter_return <- c(quarter_return, mean(a, na.rm = T))
}
names(quarter_return) <- data$symbolnames
quarter_rank <- rank(quarter_return*-1)
symbol_name <- names(quarter_rank[quarter_rank<=round(length(quarter_return)*top)])

data$weight[period.ends,symbol_name] = 1/length(symbol_name)

models$benchmark = bt.run.share(data, clean.signal = F, commission = commission)

nextday.Weight = rbind(nextday.Weight, tail(data$weight, 1))


# ------------------------------------------ Output ------------------------------------------ #

nextday.Weight = as.data.frame(nextday.Weight, row.names = names(models))

models = rev(models)
models$nextday.Weight = nextday.Weight

save(models, file="TData/models.RData")



