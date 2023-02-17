install.packages("quantmod")
install.packages("PerformanceAnalytics")
install.packages("dygraphs")
install.packages("corrplot")

library(quantmod)
library(PerformanceAnalytics)
library(dygraphs)
library(corrplot)

#Stock Portfolio Build/Test Tool
#N.B. S&P will act as a standardisation tool
#i.e. it is there for comparison purposes, obvs not part
#of our portfolio :x

#Important
#Never Change SP500 as being 1st
#By nature of code being scalable, our comparsion case must be in a
#constant column

#Gianluca style creating a function
monthly_returns <- function(ticker, start_year)
{
    # Yoink from Yahoo
    stock <- getSymbols(ticker, src = "yahoo", auto.assign = FALSE) 
    
    # Prevents those pesky errors from blank boxes
    stock <- na.omit(stock)
    
    # We dont really need all that info, lets restrict to adjusted end of day
    #I'm sorry Kyle, yours was more comprehensive but I'm lazy
    stock <- stock[, 6]
    
    # Set up for the horizon we are studying
    horizon <- paste0(as.character(start_year), "/", as.character(Sys.Date()))
    stock <- stock[horizon]
    
    # Calculate monthly Simple Returns 
    #for future predictions
    data <- periodReturn(stock, period = "monthly", type = "arithmetic")
    
    # Assign to the global environment to be accessible, I don't really understand but it no work without
    assign(ticker, data, envir = .GlobalEnv)
}

# Test for each stock
monthly_returns("SBUX", 2015)
monthly_returns("CCL", 2015)
monthly_returns("AAPL", 2015)

# We can also look at Indexes(dices?) (Multiple indexes?)
monthly_returns("SPY", 2015)
# Merge function makes a comparative table so to speak
#Lets us plot that bitch
portfolio <- merge.xts(SPY, SBUX, CCL, AAPL)
colnames(portfolio) <- c("SP500", "SBUX", "CCL", "AAPL")

#Produce interactive chart of stock returns
#I'm sorry little PC...
dygraph(portfolio, main = "Portfolio Stocks vs. S&P 500") %>%
  dyAxis("y", label = "Return", valueRange = c(-1,0.5)) %>%
  dyRangeSelector(dateWindow = c("2015-01-01", "2020-07-01")) %>%
  dyOptions(colors = RColorBrewer::brewer.pal(4, "Set2")) 

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


#Will be taking some inspiration from Mr Walsh's Code

library(tidyquant) # To download the data
library(plotly) # To create interactive charts
library(timetk) # To manipulate the data series
library(tidyr)
library(forcats)

#Mean Return of Portfolio
mean_ret <- colMeans(portfolio[,-1])
print(round(mean_ret, 5))

#Lets start Random Weighting
wts <- runif(n = length(colnames(portfolio[,-1])))
print(wts)
print(sum(wts))

#Be good to normalise them though....
wts <- wts/sum(wts)
print(wts)
sum(wts)
port_returns <- (sum(wts * mean_ret) + 1)^12 - 1

#Analyse risk
cov_mat <- cov(portfolio[,-1]) * 12
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
                  ncol = length(colnames(portfolio[,-1])))

# Creating an empty vector to store portfolio returns
port_returns <- vector('numeric', length = num_port)

# Creating an empty vector to store portfolio Standard deviation
port_risk <- vector('numeric', length = num_port)

# Creating an empty vector to store portfolio Sharpe Ratio
sharpe_ratio <- vector('numeric', length = num_port)



##Running Loop
for (i in seq_along(port_returns)) {
  
  wts <- runif(length(colnames(portfolio[,-1])))
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


#Ignore me just allowing for scaling


# Converting matrix to a tibble and changing column names
all_wts <- tk_tbl(all_wts, preserve_index = "FALSE")
colnames(all_wts) <- colnames(portfolio[,-1])
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

gatherworkaround=subset(portfolio_values, select = -c(Return, Risk, SharpeRatio))
gatherworkaround

#Workaround times
altmin_var=subset(min_var, select = -c(Return, Risk, SharpeRatio))
min_var
altmin_var

altmax_sr=subset(max_sr, select = -c(Return, Risk, SharpeRatio))
max_sr
altmax_sr


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
