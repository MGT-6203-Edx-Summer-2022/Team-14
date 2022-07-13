# Using tidyquant (including Quantmod package) to get stock price with time series
library(tidyquant)
library("rjson")
library(tidyverse)
library(dplyr)

# set your wd to local github repo: setwd("/<path>/GitHub/Team-14/Recession-Proof_Portfolio/Data/")
# get SP500 with sector, market cap, and PE
# sp <- read_csv("S&P500_by_Sector_Cap_PE.csv")
sp500_w_details <- read_csv("constituents-financials.csv")
# sp$`Market Capitalization` <- parse_number(sp$`Market Capitalization`)

# company by sector
comp_by_sec <- sp500_w_details %>% group_by(Sector) %>% summarise(count_company = n())

# Categorize institutions by its market cap (1 of the factors that can be used later on in analysis)
# sp <- sp %>%
#  mutate(cap_category = ifelse(sp$market_cap > 10000000000, 'Large Cap', 
#                               ifelse(sp$market_cap < 2000000000, 'Small Cap', 'Mid Cap')))
sp500_w_details <- sp500_w_details %>%
  mutate(cap_category = ifelse(sp500_w_details$`Market Cap` > 10000000000, 'Large Cap', 
                               ifelse(sp500_w_details$`Market Cap` < 2000000000, 'Small Cap', 'Mid Cap')))

sp500_historical <- read_csv("sp500_bear_periods.csv")
head(sp500_historical)

tickers_1971 <- filter(sp500_historical, Date == "1969-04-01")
tickers_1971 <- as.character(tickers_1971$Ticker)
# use SP500 as benchmarks
benchmarks <- "^GSPC"
# tickers <- sp$Ticker
tickers <- as.character(sp500_w_details$Symbol)

# Create a new data frame grouping the sectors together
# with their fundamentals  

df_sector <- sp500_w_details %>%
  group_by(Sector) %>%
  summarise(
    count = n(),
    avg.price = as.integer(mean(Price)),
    med.pe = median(`Price/Earnings`, na.rm = TRUE),
    avg.eps = mean(`Earnings/Share`),
    cap = median(`Market Cap`),
    ebitda = median(EBITDA),
    ps = mean(`Price/Sales`),
    pb = median(`Price/Book`, na.rm = TRUE)
  )
df

t <- df %>%
  arrange(count) %>%
  mutate(Sector = factor(Sector, levels = Sector)) %>%
  ggplot(aes(x = Sector, y = count)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  xlab("") +
  theme_bw()

t

# get return for SP500 during 1971 period
Ra_1971 <- tickers_1971 %>%
  tq_get(get  = "stock.prices",
         from = "1969-04-01",
         to = "1971-01-01") %>%
  group_by(symbol) %>%
  tq_transmute(select     = adjusted, 
               mutate_fun = periodReturn, 
               period     = "monthly", 
               col_rename = "Ra") 

# get return for baseline (^GSPC) during 1971 period
baseline_1971 <- benchmarks %>%
  tq_get(get  = "stock.prices",
         from = "1969-04-01",
         to = "1971-01-01") %>%
  tq_transmute(select     = adjusted, 
               mutate_fun = periodReturn, 
               period     = "monthly", 
               col_rename = "Rb") 

# merge Ra & Rb for 1971
RaRb_single_portfolio_1971 <- left_join(Ra_1971, 
                                        
                                        
                                        baseline_1971,
                                        by = "date")

RaRb_capm_1971 <- RaRb_single_portfolio_1971 %>%
  tq_performance(Ra = Ra, 
                 Rb = Rb, 
                 performance_fun = table.CAPM)


# get return for SP500 during dotcom bubble 2001
Ra_2001 <- tickers %>%
  tq_get(get  = "stock.prices",
         from = "2000-03-24",
         to = "2001-09-21") %>%
  group_by(symbol) %>%
  tq_transmute(select     = adjusted, 
               mutate_fun = periodReturn, 
               period     = "monthly", 
               col_rename = "Ra") 

# get return for baseline (^GSPC) dotcom bubble 2001
baseline_2001 <- benchmarks %>%
  tq_get(get  = "stock.prices",
         from = "2000-03-24",
         to = "2001-09-21") %>%
  tq_transmute(select     = adjusted, 
               mutate_fun = periodReturn, 
               period     = "monthly", 
               col_rename = "Rb") 

# get return for SP500 during Great Depression period 2007 - 2009
Ra_2007 <- tickers %>%
  tq_get(get  = "stock.prices",
         from = "2007-12-01",
         to = "2009-06-30") %>%
  group_by(symbol) %>%
  tq_transmute(select     = adjusted, 
               mutate_fun = periodReturn, 
               period     = "monthly", 
               col_rename = "Ra") 

# get return for baseline (^GSPC) 2007 - 2009
baseline_2007 <- benchmarks %>%
  tq_get(get  = "stock.prices",
         from = "2007-12-01",
         to = "2009-06-30") %>%
  tq_transmute(select     = adjusted, 
               mutate_fun = periodReturn, 
               period     = "monthly", 
               col_rename = "Rb") 

# get return for SP500 of most return bear market
Ra_2020 <- tickers %>%
  tq_get(get  = "stock.prices",
         from = "2020-01-01",
         to = "2020-09-30") %>%
  group_by(symbol) %>%
  tq_transmute(select     = adjusted, 
               mutate_fun = periodReturn, 
               period     = "monthly", 
               col_rename = "Ra")

# get return for baseline (^GSPC) of the most recent bear market
baseline_2020 <- benchmarks %>%
  tq_get(get  = "stock.prices",
         from = "2020-01-01",
         to = "2020-09-30") %>%
  tq_transmute(select     = adjusted, 
               mutate_fun = periodReturn, 
               period     = "monthly", 
               col_rename = "Rb") 

# merge Ra & Rb for 2001
RaRb_single_portfolio_2001 <- left_join(Ra_2001, 
                                   baseline_2001,
                                   by = "date")
# merge Ra & Rb for 2007
RaRb_single_portfolio_2007 <- left_join(Ra_2007, 
                                        baseline_2007,
                                        by = "date")

# merge Ra & Rb for 2020
RaRb_single_portfolio_2020 <- left_join(Ra_2020, 
                                        baseline_2020,
                                        by = "date")


RaRb_capm_2001 <- RaRb_single_portfolio_2001 %>%
  tq_performance(Ra = Ra, 
                 Rb = Rb, 
                 performance_fun = table.CAPM)

RaRb_capm_2001 %>% select(symbol, Alpha, Beta)

RaRb_capm_2007 <- RaRb_single_portfolio_2007 %>%
  tq_performance(Ra = Ra, 
                 Rb = Rb, 
                 performance_fun = table.CAPM)

RaRb_capm_2007 %>% select(symbol, Alpha, Beta)

RaRb_capm_2020 <- RaRb_single_portfolio_2020 %>%
  tq_performance(Ra = Ra, 
                 Rb = Rb, 
                 performance_fun = table.CAPM)

RaRb_capm_2020 %>% select(symbol, Alpha, Beta)

#create data frame with 0 rows and 3 columns
curr_md <- data.frame(matrix(ncol = 3, nrow = 0))

#provide column names
#colnames(df) <- c('symbol', 'coef', 'mkt')

symb_2007 <- unique(RaRb_single_portfolio_2007$symbol)

symb_2001 <- unique(RaRb_single_portfolio_2001$symbol)

symb_2020 <- unique(RaRb_single_portfolio_2020$symbol)

for (value in symb_2007) {
  curr <- RaRb_single_portfolio_2007[RaRb_single_portfolio_2007$symbol == value,]
  lms <- lm(Ra ~ Rb, data = curr)
  curr_md_2007 <- rbind(curr_md, c(value, lms$coefficient[1], lms$coefficient[2]))
}

colnames(curr_md_2007) <- c('symbol','model_intercept','Beta')

# get 30 percentile based on alpha
thirtyp_threshold_2007 <- quantile(as.numeric(curr_md_2007$model_intercept), probs = 0.7)

thirtp_df_2007 <- filter(curr_md, curr_md$model_intercept >= thirtyp_threshold_2007)

# create an xts dataset
All.dat<-xts(RaRb_single_portfolio[,-2],order.by=RaRb_single_portfolio$date)

# calculate the Compounded Return
Return.cumulative(All.dat$ContraRet, geometric = TRUE)

# write.csv(as.data.frame(thirtp_df_2007), "/Users/bao.vo/Documents/GitHub/Team-14/Recession-Proof_Portfolio/Data/List_30th_percentile.csv")
# write.csv(as.data.frame(RaRb_capm), "/Users/baovo/Documents/GitHub/Team-14/Recession-Proof_Portfolio/Data/BV_RaRb_capm.csv")
# write.csv(as.data.frame(df), "/Users/baovo/Documents/GitHub/Team-14/Recession-Proof_Portfolio/Data/BV_RaRbCoef.csv")
# write.csv(as.data.frame(Ra), "/Users/baovo/Documents/GitHub/Team-14/Recession-Proof_Portfolio/Data/BV_Ra.csv")
# write.csv(as.data.frame(sp500_w_details), "/Users/baovo/Documents/GitHub/Team-14/Recession-Proof_Portfolio/Data/BV_sp.csv")
# write.csv(as.data.frame(RaRb_single_portfolio), "/Users/baovo/Documents/GitHub/Team-14/Recession-Proof_Portfolio/Data/BV_RaRb.csv")
# write.csv(as.data.frame(avg_return), "/Users/baovo/Documents/GitHub/Team-14/Recession-Proof_Portfolio/Data/BV_avg_return.csv")
# write.csv(as.data.frame(comp_by_sec), "/Users/baovo/Documents/GitHub/Team-14/Recession-Proof_Portfolio/Data/BV_comp_by_sec.csv")

avg_return =Ra %>% 
  group_by(symbol) %>%
  summarise(avg_return = round(mean(Ra), 4),Volatility =   sd(Ra)) %>%         
  arrange(desc(avg_return), desc(Volatility))

Ra <- Ra %>% arrange(symbol, date) %>% group_by(symbol) %>% mutate(cum_ra = cumsum(Ra))   

# create chart
p <- avg_return %>% head(20) %>% ggplot(aes(reorder(symbol, -avg_return), avg_return, fill = avg_return))+
  geom_col()+
  coord_flip()+
  labs(title = "Top 20 Tickers Average Return in SP500 during Great Depression (2007-2009)", x = "Ticker", y = "Average Return")+
  theme_classic()+
  theme(legend.position="right")
p <- p + guides(fill=guide_legend(title="Avg Return"))
