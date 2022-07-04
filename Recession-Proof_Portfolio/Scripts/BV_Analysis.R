# Using tidyquant (including Quantmod package) to get stock price with time series
library(tidyquant)
library("rjson")
library(dplyr)
library(ggplot2)
library(tidyverse)
library(readr)

# set your wd to local github repo: setwd("/<path>/GitHub/Team-14/Recession-Proof_Portfolio/Data/")
# get SP500 with sector, market cap, and PE
# sp <- read_csv("S&P500_by_Sector_Cap_PE.csv")
sp1 <- read_csv("constituents-financials.csv")
# sp$`Market Capitalization` <- parse_number(sp$`Market Capitalization`)



# Categorize institutions by its market cap
# sp <- sp %>%
#  mutate(cap_category = ifelse(sp$market_cap > 10000000000, 'Large Cap', 
#                               ifelse(sp$market_cap < 2000000000, 'Small Cap', 'Mid Cap')))
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

# Calculate performance using tidyquant 
RaRb_capm <- RaRb_single_portfolio %>%
  tq_performance(Ra = Ra, 
                 Rb = Rb, 
                 performance_fun = table.CAPM)

RaRb_capm %>% select(symbol, Alpha, Beta)

#create data frame with 0 rows and 3 columns
df <- data.frame(matrix(ncol = 3, nrow = 0))

#provide column names
#colnames(df) <- c('symbol', 'coef', 'mkt')

symb <- unique(RaRb_single_portfolio$symbol)

for (value in symb) {
  curr <- RaRb_single_portfolio[RaRb_single_portfolio$symbol == value,]
  lms <- lm(Ra ~ Rb, data = curr)
  df <- rbind(df, c(value, lms$coefficient[1], lms$coefficient[2]))
}

colnames(df) <- c('symbol','model_intercept','Beta')


# create an xts dataset
All.dat<-xts(RaRb_single_portfolio[,-2],order.by=RaRb_single_portfolio$date)

# calculate the Compounded Return
Return.cumulative(All.dat$ContraRet, geometric = TRUE)

# write.csv(as.data.frame(RaRb_capm), "/Users/baovo/Documents/GitHub/Team-14/Recession-Proof_Portfolio/Data/BV_RaRb_capm.csv")
# write.csv(as.data.frame(df), "/Users/baovo/Documents/GitHub/Team-14/Recession-Proof_Portfolio/Data/BV_RaRbCoef.csv")
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

prices <- tickers %>%
  tq_get(get  = "stock.prices",
         from = "2007-12-01",
         to = "2009-06-30")
prices <- reduce(prices,merge)
colnames(prices) <- tickers

avg_return %>% head(20) %>% ggplot(aes(reorder(symbol, -avg_return), avg_return, fill = avg_return))+
  geom_col()+
  coord_flip()+
  labs(title = "Top 20 Tickers Average Return in SP500 during Great Depression (2007-2009)", x = "Ticker", y = "Average Return")+
  theme_classic()+
  theme(legend.position="none")

