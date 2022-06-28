# Using tidyquant (including Quantmod package) to get stock price with time series
library(tidyquant)
library("rjson")
library(dplyr)
library(ggplot2)

options("getSymbols.warning4.0"=FALSE)
options("getSymbols.yahoo.warning"=FALSE)

tickers = c("AAPL", "NFLX", "AMZN", "K", "O")

prices <- tq_get(tickers,
                 from = "2008-01-01",
                 to = "2022-06-01",
                 get = "stock.prices")

# prices <- map(tickers,function(x) Ad(get(x)))
# prices <- reduce(prices,merge)
# colnames(prices) <- tickers

head(prices)

# Give the input file name to the function.
# SP500 <- fromJSON(file = "sp500_constituents.json")
# head(SP500)

# install.packages("jsonlite", repos="https://cran.rstudio.com/")
library("jsonlite")

json_file <- 'https://datahub.io/core/s-and-p-500-companies-financials/datapackage.json'
json_data <- fromJSON(paste(readLines(json_file), collapse=""))

# get list of all resources:
print(json_data$resources$name)

# print all tabular data(if exists any)
for(i in 1:length(json_data$resources$datahub$type)){
  if(json_data$resources$datahub$type[i]=='derived/csv'){
    path_to_file = json_data$resources$path[i]
    data <- read.csv(url(path_to_file))
   # print(data)
  }
}

head(data)
head(prices)
merge_data <- merge(x = prices, y = data, by.x="symbol", by.y="Symbol")
head(merge_data)
keep <- c("symbol", "date", "volume", "adjusted", "Name", "Sector")
df1 <- merge_data[keep]
head(df1)
df2 <- data[c("Symbol", "Name", "Sector")]

daily_sector = df1 %>% group_by(symbol, Name, Sector) %>% 
  tq_transmute(select     = adjusted, 
               mutate_fun = periodReturn, 
               period     = "daily") %>% 
  ungroup()
head(daily_sector)

avg_return =daily_sector %>% 
  group_by(Name, Sector) %>%
  summarise(avg_return = round(mean(daily.returns), 4),Volatility =   sd(daily.returns)) %>%         
  arrange(desc(avg_return), desc(Volatility))
avg_return %>% head()

avg_return %>% head(20) %>% ggplot(aes(reorder(Name, -avg_return), avg_return, fill = avg_return))+
  geom_col()+
  coord_flip()+
  labs(title = "Some Tickers Average Return in SP500 From 2008 - Present", x = "Ticker", y = "Average Return")+
  theme_classic()+
  theme(legend.position="none")