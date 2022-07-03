# Using tidyquant (including Quantmod package) to get stock price with time series
library(tidyquant)
library("rjson")
library(dplyr)
library(ggplot2)
library(tidyverse)
library(readr)

# set your wd to local github repo: setwd("/<path>/GitHub/Team-14/Recession-Proof_Portfolio/Data/")
# get SP500 with sector, market cap, and PE
sp <- read_csv("S&P500_by_Sector_Cap_PE.csv")
sp1 <- read_csv("constituents-financials.csv")
sp$`Market Capitalization` <- parse_number(sp$`Market Capitalization`)



# Categorize institutions by its market cap
sp <- sp %>%
  mutate(cap_category = ifelse(sp$market_cap > 10000000000, 'Large Cap', 
                               ifelse(sp$market_cap < 2000000000, 'Small Cap', 'Mid Cap')))
sp1 <- sp1 %>%
  mutate(cap_category = ifelse(sp1$`Market Cap` > 10000000000, 'Large Cap', 
                               ifelse(sp1$`Market Cap` < 2000000000, 'Small Cap', 'Mid Cap')))

# use SP500 as benchmarks
benchmarks <- "^GSPC"
# tickers <- sp$Ticker
tickers <- as.character(sp1$Symbol)

# get return for SP500
Ra <- tickers %>%
  tq_get(get  = "stock.prices",
         from = "2007-12-01",
         to = "2009-06-30") %>%
  group_by(symbol) %>%
  tq_transmute(select     = adjusted, 
               mutate_fun = periodReturn, 
               period     = "monthly", 
               col_rename = "Ra")

baseline <- benchmarks %>%
  tq_get(get  = "stock.prices",
         from = "2007-12-01",
         to = "2009-06-30") %>%
  tq_transmute(select     = adjusted, 
               mutate_fun = periodReturn, 
               period     = "monthly", 
               col_rename = "Rb")

RaRb_single_portfolio <- left_join(Ra, 
                                   baseline,
                                   by = "date")

# write.csv(as.data.frame(Ra), "/Users/baovo/Documents/GitHub/Team-14/Recession-Proof_Portfolio/Data/BV_Ra.csv")
# write.csv(as.data.frame(sp1), "/Users/baovo/Documents/GitHub/Team-14/Recession-Proof_Portfolio/Data/BV_sp.csv")
# write.csv(as.data.frame(RaRb_single_portfolio), "/Users/baovo/Documents/GitHub/Team-14/Recession-Proof_Portfolio/Data/BV_RaRb.csv")
# write.csv(as.data.frame(avg_return), "/Users/baovo/Documents/GitHub/Team-14/Recession-Proof_Portfolio/Data/BV_avg_return.csv")
# write.csv(as.data.frame(comp_by_sec), "/Users/baovo/Documents/GitHub/Team-14/Recession-Proof_Portfolio/Data/BV_comp_by_sec.csv")

avg_return =Ra %>% 
  group_by(symbol) %>%
  summarise(avg_return = round(mean(Ra), 4),Volatility =   sd(Ra)) %>%         
  arrange(desc(avg_return), desc(Volatility))

Ra <- Ra %>% arrange(symbol, date) %>% group_by(symbol) %>% mutate(cum_ra = cumsum(Ra))   

# company by sector
comp_by_sec <- sp1 %>% group_by(Sector) %>% summarise(count_company = n())

# prices <- map(tickers,function(x) Ad(get(x)))
# prices <- reduce(prices,merge)
# colnames(prices) <- tickers

head(prices)


# -------------------------------------------
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
# -------------------------------
