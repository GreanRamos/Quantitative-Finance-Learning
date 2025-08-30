library(quantmod)
library(PerformanceAnalytics)
library(MASS) #class
library(fBasics) #class
library(Ecdat) #class
library(quadprog) #class
library(corrplot)
#library(fPortfolio)
library(ggplot2)
library(tidyverse)
library(timetk)
library(plotly)
library(tidyquant) # To download the data
library(plotly) # To create interactive charts
library(timetk) # To manipulate the data series

################## Define Initial Conditions for Analysis  ##################  

personal <- '/Users/grean/Desktop/Portfolio/5 GITHUB Uploads/August 2025/Portfolio.csv'

firstDate <- "2020-1-1" #defines starting date for time series.

riskfreeRate <- 0.04196 #decimal format using 3 month treasury bill rate

mean_SHARPEtime <- 504 #unit in days. Determine timeframe for most recent data to calculate expected returns for SHARPE (monthly, yearly, etc)




################## Import Position Data  ################## 
positions <- read.csv(personal, header = FALSE)
positions <- subset(positions, select= -c(V1)) #remove unneccesary first column containing titles

stocks <- positions[1,] #vector of ticker symbols

portfolioPrices <- NULL #empty list

#For-loop to create a dataframe showing all the stock adjusted prices side-by-side
for(ticker in stocks) {
  portfolioPrices <- cbind(portfolioPrices,
                           getSymbols(ticker, from=firstDate, auto.assign=FALSE)[,6])
  
}
# cbind() combines columns of multiple dataframes

colSums(is.na(portfolioPrices)) #find if there is any missing data

cleanPrices <- na.omit(portfolioPrices) #new dataframe removing all days with NA (aka remove weekends)

colSums(is.na(cleanPrices)) #ensure no NA values are left




################## Calculate Overall Portfolio Value  ################## 
portfolioValue <- cleanPrices %*% as.numeric(positions[2,]) #overall portfolio value per day via sum(daily price*position quantity)

cleanPrices <- cbind(cleanPrices, portfolioValue) #add portfolio value to the pricing table

dev.new()
plot(cleanPrices$portfolioValue)





################## Calculate Asset Weights with Current Prices  ################## 

#Weights are determined by initial capital / total portfolio value as stated in the csv

todayValue <- as.numeric(cleanPrices[nrow(cleanPrices),ncol(cleanPrices)])

todayAsset <- tail(cleanPrices, 1)
todayAsset <- todayAsset[,0:length(stocks)]

todayAsset <- as.numeric(positions[2,]) * todayAsset #intermediate calculation for each stock value in the portfolio by transposing matrix
#assetWeights <- todayAsset / todayValue #divide each respective portfolio value by the overall portfolio value

assetWeights <- todayAsset / sum(todayAsset)

assetWeights <- t(assetWeights) 
assetWeights <- as.matrix(assetWeights, headers = FALSE)

dev.new()
pie(assetWeights[,1:ncol(assetWeights)], labels = paste(paste(rownames(assetWeights), assetWeights[,1:ncol(assetWeights)]),"%",sep=""), main = 'Asset Weights w/ Current Prices')




################## Calculate Asset Weights for ONLY Invested Capital  ################## 

baselineWeights <- as.numeric(positions[3,]) / sum(as.numeric(positions[3,]))
pie.baselineWeights <- baselineWeights
names(pie.baselineWeights) <- stocks

dev.new()
pie(pie.baselineWeights, main = 'Asset Allocation of Original Capital Invested')
print(rbind(positions[1,], baselineWeights))





################## Portfolio Overall Returns in Percentages ################## 

portfolioPercentReturn <- diff(log(cleanPrices$portfolioValue)) #Create column of daily portfolio differences
cleanPrices <- cbind(cleanPrices, portfolioPercentReturn) #add returns to original dataframe
cleanPrices <- na.omit(cleanPrices) #remove initial NA value

dev.new()
hist(cleanPrices$portfolioValue.1) #histogram for portfolio 






################## Individual Asset Daily Average Risk-Return Tradeoff ################## 

stockRTNS <- cleanPrices[,-seq(ncol(cleanPrices)-1, ncol(cleanPrices))] #calculate daily log return and remove portfolio value columns
stockRTNS <- na.omit(diff(log(stockRTNS))) #calculate daily log returns for each stock

stockPerformance <- cbind(colMeans(stockRTNS), apply(stockRTNS, 2, sd, na.rm = TRUE)) #Calculate each asset mean and st dev
colnames(stockPerformance) = c("Daily_Mean", "Daily_Standard_Deviation")  
stockPerformance <- as.data.frame(stockPerformance) #Convert to data frame element

dev.new()
ggplot(stockPerformance, aes(x = stockPerformance$Daily_Standard_Deviation, y = stockPerformance$Daily_Mean, color = as.factor(stocks))) +
  geom_point(size = 3) + 
  geom_text(label=rownames(stockPerformance), nudge_x = 0.0002, nudge_y = 0.0002, check_overlap = T) +
  # Miscellaneous Formatting
  theme_bw() + ggtitle("Daily Risk-Return Tradeoff") +
  xlab("Volatility") + ylab("Daily Expected Returns")





################## Calculate Current Portfolio Return on Investment  ################## 

overallROI <- todayValue - sum(as.numeric(positions[3,])) #current portfolio value - sum(initial capital per asset)

strROI <- paste("Portfolio Overall Return on Investment (in Dollars):", overallROI)
print(strROI)




################## Calculate SHARPE Ratio of Portfolio  ################## 

# *Annualized Figure* for the SHARPE Metric

#All time SHARPE
portSHARPE_full <- ((mean(cleanPrices$portfolioValue.1)*253) - riskfreeRate) / (sd(cleanPrices$portfolioValue.1)*sqrt(253))

#SHARPE Using "mean_SHARPEtime"
portSHARPE_custom <- ((mean(tail(cleanPrices$portfolioValue.1, mean_SHARPEtime))*253) - riskfreeRate) / (sd(tail(cleanPrices$portfolioValue.1, mean_SHARPEtime))*sqrt(253))

strSRfull <- paste("Portfolio SHARPE using ALL data:", portSHARPE_full)
strSRcustom <- paste("Portfolio SHARPE using custom timeframe:", portSHARPE_custom)

print(strSRfull)
print(strSRcustom)



################## High Level Compare to Benchmark ################## 

benchmark <- getSymbols('^GSPC', from = rownames(as.data.frame(portfolioPercentReturn))[1], auto.assign = F)[,6] #Import S&P daily adjusted prices
benchRTN <- na.omit(diff(log(benchmark))) #ROC finds the daily change/returns

# 1 - Beta: measure of volatility against a benchmark (*Daily Figure*)

gspcBETA <- CAPM.beta(cleanPrices$portfolioValue.1, benchRTN, riskfreeRate/253) #Daily data

# 2 - Alpha: measure of portfolio performance against a benchmark, in this case S&P500 (*Daily Figure*)

gspcALPHA <- CAPM.jensenAlpha(cleanPrices$portfolioValue.1, benchRTN, riskfreeRate/253) #Daily data

strBETA <- paste("Portfolio BETA using S&P500:", gspcBETA)
strALPHA <- paste("Portfolio ALPHA using S&P500:", gspcALPHA)

print(strBETA)
print(strALPHA)

#Raw Metrics
summary(cleanPrices$portfolioValue.1) #Portfolio
summary(benchRTN) #Benchmark

#Benchmark vs portfolio Risk-Return Tradeoff Graph

comparePORTFOLIO <- matrix(
  c(mean(cleanPrices$portfolioValue.1), sd(cleanPrices$portfolioValue.1), mean(benchRTN) , sd(benchRTN)), 
  nrow = 2,   
  ncol = 2,         
  byrow = TRUE          
)
colnames(comparePORTFOLIO) = c("Daily_Mean", "Daily_Standard_Deviation")
rownames(comparePORTFOLIO) = c("Portfolio", "Benchmark")

comparePORTFOLIO <- data.frame(comparePORTFOLIO)

dev.new()
ggplot(comparePORTFOLIO, aes(x = comparePORTFOLIO$Daily_Standard_Deviation, y = comparePORTFOLIO$Daily_Mean)) +
  geom_point(size = 3) + 
  geom_text(label=rownames(comparePORTFOLIO), nudge_x = 0.000035, nudge_y = 0.000035, check_overlap = T) +
  # Miscellaneous Formatting
  theme_bw() + ggtitle("Daily Risk-Return Tradeoff") +
  xlab("Volatility") + ylab("Daily Expected Returns")

print(comparePORTFOLIO)

#Line Plots
dev.new()
par(mfrow= c(1,2))
plot(cleanPrices$portfolioValue.1, type = 'l')
plot(benchRTN, type = 'l')

#Histograms
dev.new()
par(mfrow= c(1,2))
hist(cleanPrices$portfolioValue.1)
hist(benchRTN)






#################### Optimization - Efficient Frontier #################### 

## Simulation
num_port <- 5000

all_wts <- matrix(nrow = num_port,
                  ncol = length(stocks))

port_returns <- vector('numeric', length = num_port) #empty variable for simulated portfolio returns

port_risk <- vector('numeric', length = num_port) #empty variable for simulated portfolio variance

sharpe_ratio <- vector('numeric', length = num_port) #empty variable for simulated portfolio sharpe ratio

for (i in seq_along(port_returns)) {
  
  wts <- runif(length(stocks)) #random weights uniformly distributed
  wts <- wts/sum(wts)
  
  # Storing weight in the matrix
  all_wts[i,] <- wts
  
  
  # Daily Portfolio returns using "mean_SHARPEtime" for more recent data
  port_ret <- sum(wts * colMeans(tail(stockRTNS, mean_SHARPEtime)))
  #port_ret <- ((port_ret + 1)^252) - 1
  
  # Storing Portfolio Returns values
  port_returns[i] <- port_ret
  
  # Creating and storing portfolio risk using "mean_SHARPEtime" for more recent data
  port_sd <- sqrt(t(wts) %*% (cov(tail(stockRTNS, mean_SHARPEtime))  %*% wts))
  port_risk[i] <- port_sd
  
  # Creating and storing Portfolio Sharpe Ratios
  
  sr <- ((port_ret*253) - riskfreeRate) / (port_sd*sqrt(253)) #Annualized Metric
  sharpe_ratio[i] <- sr
  
}

# Storing the values in the table
portfolio_values <- tibble(Return = port_returns,
                           Risk = port_risk,
                           annualSharpeRatio = sharpe_ratio)


# Converting matrix to a tibble and changing column names
all_wts <- tk_tbl(all_wts)

colnames(all_wts) <- colnames(stockRTNS)

# Combing all the values together
portfolio_values <- tk_tbl(cbind(all_wts, portfolio_values))

min_var <- portfolio_values[which.min(portfolio_values$Risk),] #Min Variance
max_sr <- portfolio_values[which.max(portfolio_values$annualSharpeRatio),] #Max SHARPE Ratio

# Min variance portfolio
p <- min_var %>%
  gather(colnames(min_var)[1]:colnames(min_var)[ncol(stocks)], key = Asset,
         value = Weights) %>%
  mutate(Asset = as.factor(Asset)) %>%
  ggplot(aes(x = fct_reorder(Asset,Weights), y = Weights, fill = Asset)) +
  geom_bar(stat = 'identity') +
  theme_minimal() +
  labs(x = 'Assets', y = 'Weights', title = "Minimum Variance Portfolio Weights") +
  scale_y_continuous(labels = scales::percent) 

ggplotly(p)

# Tangency Portfolio
p <- max_sr %>%
  gather(colnames(min_var)[1]:colnames(min_var)[ncol(stocks)], key = Asset,
         value = Weights) %>%
  mutate(Asset = as.factor(Asset)) %>%
  ggplot(aes(x = fct_reorder(Asset,Weights), y = Weights, fill = Asset)) +
  geom_bar(stat = 'identity') +
  theme_minimal() +
  labs(x = 'Assets', y = 'Weights', title = "Tangency Portfolio Weights") +
  scale_y_continuous(labels = scales::percent) 

ggplotly(p)

#Full Efficient Frontier
p <- portfolio_values %>%
  ggplot(aes(x = Risk, y = Return, color = annualSharpeRatio)) +
  geom_point() +
  theme_classic() +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(labels = scales::percent) +
  labs(x = 'Daily Risk',
       y = 'Daily Returns',
       title = "Portfolio Optimization & Efficient Frontier") +
  geom_point(aes(x = Risk,
                 y = Return), data = min_var, color = 'red') +
  geom_point(aes(x = Risk,
                 y = Return), data = max_sr, color = 'red')


ggplotly(p)

### Work To Go - Add Action Items for Implementation of Weights ### 

# Actions to implement Minimum Variance Portfolio

varBASELINE_CASH <- min_var[,0:length(stocks)] * todayValue #New weights * current portfolio value
varTRADES <- varBASELINE_CASH - as.numeric(todayAsset) #Trades that must be performed for rebalance

print(varTRADES) #Show trades in cash value that must be performed to rebalance portfolio

# Actions to implement Tangency Portfolio
srBASELINE_CASH <- max_sr[,0:length(stocks)] * todayValue #New weights * current portfolio value
srTRADES <- srBASELINE_CASH - as.numeric(todayAsset) #Trades that must be performed for rebalance

print(srTRADES) #Show trades in cash value that must be performed to rebalance portfolio

