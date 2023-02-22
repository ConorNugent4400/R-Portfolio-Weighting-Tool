install.packages("quantmod")
install.packages("PerformanceAnalytics")
install.packages("dygraphs")
install.packages("corrplot")

library(quantmod)
library(PerformanceAnalytics)
library(dygraphs)
library(corrplot)
library(tidyr)

#Stock Portfolio Build/Test Tool

#Simple function for ease
monthly_returns <- function(ticker)
{
  # Yoink from Yahoo
  stock <- getSymbols(ticker, src = "yahoo", auto.assign = FALSE) 
  
  # Prevents those pesky errors from blank boxes
  stock <- na.omit(stock)
  
  # We dont really need all that info, lets restrict to adjusted end of day
  #That's decent enough for our comparison needs
  stock <- stock[, 6]
  
  # Calculate monthly Simple Returns 
  #for future predictions
  #Can go Logarithmic here, but we used adjusted figures already
  data <- periodReturn(stock, period = "monthly", type = "arithmetic")
  
  # Assign to the global environment to be accessible otherwise R be like 
  #I don't know where that is
  assign(ticker, data, envir = .GlobalEnv)
}

# Get th(x, "xts")e current folder
current_folder <- getwd()

# Set the current folder as the working directory
setwd(current_folder)
getwd()
portfolio <- readLines("portfolio.txt")
portfolio <- lapply(portfolio, monthly_returns)

obj_names <- ls()
obj_names
# Filter for objects that are of class xts
xts_objs <- Filter(function(x) is(x, "xts"), mget(obj_names))
xts_objs
# Merge the xts objects into a single xts object
merged_xts <- Reduce(merge, xts_objs)
merged_xts

col_names <- read.table(file = "portfolio.txt", header = FALSE)
col_names
colnames(merged_xts) <- col_names$V1

merged_xts
merged_xts<- na.omit(merged_xts)

merged_xts

#Lets us plot that bitch
#Produce interactive chart of stock returns
#I'm sorry little PC...
dygraph(merged_xts, main = "Portfolio Stocks") %>%
  dyAxis("y", label = "Return", valueRange = c(-1,0.5))%>%
  dyOptions(colors = RColorBrewer::brewer.pal(12, "Set3"))


portfolio=merged_xts
#What about Correlation in our Portfolio?
#Can call up matrix to see
#Interested in Stock Vs S&P
corrplot::corrplot(cor(portfolio), method = "number")
#Remember Low Inter-Stock correlation is good
#Market (S&P here) correlation depends on your beliefs


#Meaty Stuff time:
#So looking at returns is cool but we want to know
#where to dump our money
#Lets do some Portfolio Building and Assessing


#Mr Walsh cheers for the package recomendation

library(tidyquant) # To download the data
library(plotly) # To create interactive charts
library(timetk) # To manipulate the data series
library(tidyr)
library(forcats)

#Mean Return of Portfolio
mean_ret <- colMeans(portfolio)
print(round(mean_ret, 5))

#Lets start Random Weighting
wts <- runif(n = length(colnames(portfolio)))
print(wts)
print(sum(wts))

#Be good to normalise them though....
wts <- wts/sum(wts)
print(wts)
sum(wts)
port_returns <- (sum(wts * mean_ret) + 1)^12 - 1

#Analyse risk
cov_mat <- cov(portfolio) * 12
print(round(cov_mat,4))

port_risk <- sqrt(t(wts) %*% (cov_mat %*% wts))
print(port_risk)

#Sharpe Ratio
#The greater it is the better the risk adjusted preformance is
#Between 1 and 2 for the ratio indicates acceptable performance 
#compared to the performance of risk-free investment.
sharpe_ratio <- port_returns/port_risk
print(sharpe_ratio)


#Now Lets get Looping
#Minus that weird scene in the movie....

#You can change this number for more possibilities to run through
num_port <- 50000

# Creating a matrix to store the weights
all_wts <- matrix(nrow = num_port,
                  ncol = length(colnames(portfolio)))
portfolio
all_wts
# Creating an empty vector to store portfolio returns
port_returns <- vector('numeric', length = num_port)
port_returns
# Creating an empty vector to store portfolio Standard deviation
port_risk <- vector('numeric', length = num_port)
port_risk
# Creating an empty vector to store portfolio Sharpe Ratio
sharpe_ratio <- vector('numeric', length = num_port)
sharpe_ratio


##Running Loop
for (i in seq_along(port_returns)) {
  
  wts <- runif(length(colnames(portfolio)))
  wts <- wts/sum(wts)
  
  # Storing weight in the matrix
  all_wts[i,] <- wts
  
  # Portfolio returns
  port_ret <- sum(wts * mean_ret)
  port_ret <- ((port_ret + 1)^12) - 1
  
  # Storing Portfolio Returns values
  port_returns[i] <- port_ret
  
  # Creating and storing portfolio risk
  port_sd <- sqrt(t(wts) %*% (cov_mat  %*% wts))
  port_risk[i] <- port_sd
  
  # Creating and storing Portfolio Sharpe Ratios assuming 0% Risk free rate
  
  sr <- port_ret/port_sd
  sharpe_ratio[i] <- sr
  
}

# Storing the values in the table
portfolio_values <- tibble(Return = port_returns,
                           Risk = port_risk,
                           SharpeRatio = sharpe_ratio)
colnames(portfolio_values)
portfolio_values

# Converting matrix to a tibble and changing column names
all_wts <- tk_tbl(all_wts, preserve_index = "FALSE")
colnames(all_wts) <- colnames(portfolio)
all_wts




# Combing all the values together
portfolio_values <- tk_tbl(cbind(all_wts,
                                 portfolio_values))

head(portfolio_values)

#Minimum Variance Portfolio
min_var <- portfolio_values[which.min(portfolio_values$Risk),]
portfolio_values
min_var


#Optimum Portfolio
max_sr <- portfolio_values[which.max(portfolio_values$SharpeRatio),]
max_sr

gatherworkaround=subset(portfolio_values, select = -c(Return, Risk, SharpeRatio))
gatherworkaround

#Workaround times
altmin_var=subset(min_var, select = -c(Return, Risk, SharpeRatio))
min_var
altmin_var

altmax_sr=subset(max_sr, select = -c(Return, Risk, SharpeRatio))
max_sr
altmax_sr


all_wts

#Plot minimum variance
p <- min_var %>%
  gather(colnames(altmin_var), key = Asset,
         value = Weights) %>%
  mutate(Asset = as.factor(Asset)) %>%
  ggplot(aes(x = fct_reorder(Asset,Weights), y = Weights, fill = Asset)) +
  geom_bar(stat = 'identity') +
  theme_minimal() +
  labs(x = 'Assets', y = 'Weights', title = "Minimum Variance Portfolio Weights") +
  scale_y_continuous(labels = scales::percent) 

ggplotly(p)


#Plot Optimum
p2 <- max_sr %>%
  gather(colnames(altmax_sr), key = Asset,
         value = Weights) %>%
  mutate(Asset = as.factor(Asset)) %>%
  ggplot(aes(x = fct_reorder(Asset,Weights), y = Weights, fill = Asset)) +
  geom_bar(stat = 'identity') +
  theme_minimal() +
  labs(x = 'Assets', y = 'Weights', title = "Tangency Portfolio Weights") +
  scale_y_continuous(labels = scales::percent) 

ggplotly(p2)

#Plot Efficient Frontier
p3 <- portfolio_values %>%
  ggplot(aes(x = Risk, y = Return, color = SharpeRatio)) +
  geom_point() +
  theme_classic() +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(labels = scales::percent) +
  labs(x = 'Annualized Risk',
       y = 'Annualized Returns',
       title = "Portfolio Optimization & Efficient Frontier") +
  geom_point(aes(x = Risk,
                 y = Return), data = min_var, color = 'red') +
  geom_point(aes(x = Risk,
                 y = Return), data = max_sr, color = 'red') +
  annotate('text', x = 0.20, y = 0.42, label = "Tangency Portfolio") +
  annotate('text', x = 0.18, y = 0.01, label = "Minimum variance portfolio") +
  annotate(geom = 'segment', x = 0.14, xend = 0.135,  y = 0.01, 
           yend = 0.06, color = 'red', arrow = arrow(type = "open")) +
  annotate(geom = 'segment', x = 0.22, xend = 0.2275,  y = 0.405, 
           yend = 0.365, color = 'red', arrow = arrow(type = "open"))


ggplotly(p3)

