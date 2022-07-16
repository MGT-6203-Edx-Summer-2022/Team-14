# Using tidyquant (including Quantmod package) to get stock price with time series
library(tidyquant)
library("rjson")
library(tidyverse)
library(dplyr)


# Required library for ships dataset
install.packages("MASS")

# Required for melt() and cast() function
install.packages("reshape2")
install.packages("reshape")

#Loading the libraries
library(MASS)
library(reshape2)
library(reshape)


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

# get the 12 bear periods data that we collected from get_SP500_historical_data.R
sp500_historical <- read_csv("sp500_bear_periods.csv")
head(sp500_historical)
sp500_historical_sector <- sp500_historical[c('Ticker', 'Sector')] %>% 
                        group_by(Ticker, Sector) %>% 
                        distinct(Ticker, Sector, .keep_all = TRUE) 

colnames(sp500_historical_sector) <- c('symbol', 'sector')

tickers_1971 <- filter(sp500_historical, Date == "1969-04-01")
tickers_1971 <- as.character(tickers_1971$Ticker)

tickers_1975 <- filter(sp500_historical, Date == "1973-10-01")
tickers_1975 <- as.character(tickers_1975$Ticker)

tickers_1980 <- filter(sp500_historical, Date == "1979-04-01")
tickers_1980 <- as.character(tickers_1980$Ticker)

tickers_1982 <- filter(sp500_historical, Date == "1981-04-01")
tickers_1982 <- as.character(tickers_1982$Ticker)

tickers_1991 <- filter(sp500_historical, Date == "1989-10-01")
tickers_1991 <- as.character(tickers_1991$Ticker)

tickers_2001 <- filter(sp500_historical, Date == "2001-01-01")
tickers_2001 <- as.character(tickers_2001$Ticker)

tickers_2007 <- filter(sp500_historical, Date == "2007-10-01")
tickers_2007<- as.character(tickers_2007$Ticker)

tickers_2020 <- filter(sp500_historical, Date == "2007-10-01")
tickers_2020<- as.character(tickers_2020$Ticker)

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

# get return for SP500 for each period (replace variable with the year end of bear market period)
# Ra_1971, Ra_1975, Ra_1980, Ra_1982, Ra_1991, Ra_2001, Ra_2007, Ra_2020
# We identify that there are the lack of data for further period in the past, so we 
# will work on the 3 period: 2001, 2007, and 2020 bear market and focus on 2007 period

Ra_1971<- tickers_1971 %>%
  tq_get(get  = "stock.prices",
         from = "1969-04-01",
         to = "1971-01-01") %>%
  group_by(symbol) %>%
  tq_transmute(select     = adjusted, 
               mutate_fun = periodReturn, 
               period     = "monthly", 
               col_rename = "Ra") 
# => the number of stock in the list of our SP500 for  period before 2001 is small

# get return for baseline (^GSPC) during each period
baseline_1971 <- benchmarks %>%
  tq_get(get  = "stock.prices",
         from = "1969-04-01",
         to = "1971-01-01") %>%
  tq_transmute(select     = adjusted, 
               mutate_fun = periodReturn, 
               period     = "monthly", 
               col_rename = "Rb") 

# merge Ra & Rb 
RaRb_single_portfolio_1971 <- left_join(Ra_1971,
                                        baseline_1971,
                                        by = "date")

RaRb_capm_1971 <- RaRb_single_portfolio_1971 %>%
  tq_performance(Ra = Ra, 
                 Rb = Rb, 
                 performance_fun = table.CAPM)

# get return for SP500 during dotcom bubble 2001
# we got total 350 out of 514 tickers input for this period
Ra_2001 <- tickers_2001 %>%
  tq_get(get  = "stock.prices",
         from = "2001-01-02",
         to = "2001-10-02") %>%
  group_by(symbol) %>%
  tq_transmute(select     = adjusted, 
               mutate_fun = periodReturn, 
               period     = "monthly", 
               col_rename = "Ra") 

# get return for baseline (^GSPC) dotcom bubble 2001
baseline_2001 <- benchmarks %>%
  tq_get(get  = "stock.prices",
         from = "2001-01-02",
         to = "2001-10-02") %>%
  tq_transmute(select     = adjusted, 
               mutate_fun = periodReturn, 
               period     = "monthly", 
               col_rename = "Rb") 

# get return for SP500 during Great Depression period 2007 - 2009
# we got 405/505
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

# get return for SP500 of most recent bear market
# 443/505 tickers
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

# remove CFC as there are some issues while retrieving the return for this ticker
RaRb_single_portfolio_2001 <- RaRb_single_portfolio_2001[!(RaRb_single_portfolio_2001$symbol=="CFC"),]

# merge Ra & Rb for 2007
RaRb_single_portfolio_2007 <- left_join(Ra_2007, 
                                        baseline_2007,
                                        by = "date")

# merge Ra & Rb for 2020
RaRb_single_portfolio_2020 <- left_join(Ra_2020, 
                                        baseline_2020,
                                        by = "date")
# RaRb_single_portfolio_2020 <- RaRb_single_portfolio_2020[!(RaRb_single_portfolio_2020$symbol=="ACE"),]

# METHOD 1
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


# METHOD 2 -------------------------------
#create data frame with 0 rows and 3 columns
curr_md <- data.frame(matrix(ncol = 3, nrow = 0))

#provide column names
#colnames(df) <- c('symbol', 'coef', 'mkt')

symb_2007 <- unique(Ra_2007$symbol)

symb_2001 <- unique(Ra_2001$symbol)

symb_2020 <- unique(Ra_2020$symbol)

for (value in symb_2007) {
  curr <- RaRb_single_portfolio_2007[RaRb_single_portfolio_2007$symbol == value,]
  lms <- lm(Ra ~ Rb, data = curr)
  curr_md_2007 <- rbind(curr_md, c(value, lms$coefficient[1], lms$coefficient[2]))
}

colnames(curr_md_2007) <- c('symbol','model_intercept','Beta')

#------------------------------------------

# get 30 percentile based on alpha
keep <- c("symbol", "Alpha", "performance")

thirtyp_threshold_2001 <- quantile(as.numeric(RaRb_capm_2001$Alpha), probs = 0.7)
RaRb_capm_2001$performance <- ifelse(RaRb_capm_2001$Alpha >= thirtyp_threshold_2001, 1, 0)
RaRb_capm_2001 <- RaRb_capm_2001[keep]

thirtyp_threshold_2007 <- quantile(as.numeric(RaRb_capm_2007$Alpha), probs = 0.7)
RaRb_capm_2007$performance <- ifelse(RaRb_capm_2007$Alpha >= thirtyp_threshold_2007, 1, 0)
RaRb_capm_2007 <- RaRb_capm_2007[keep]

thirtyp_threshold_2020 <- quantile(as.numeric(RaRb_capm_2020$Alpha), probs = 0.7)
RaRb_capm_2020$performance <- ifelse(RaRb_capm_2020$Alpha >= thirtyp_threshold_2020, 1, 0)
RaRb_capm_2020 <- RaRb_capm_2020[keep]

Ra_LMT_2001 <- tickers_2001 %>%
  tq_get(get  = "stock.prices",
         from = "2000-01-02",
         to = "2001-12-31") %>%
  group_by(symbol) %>%
  tq_transmute(select     = adjusted, 
               mutate_fun = periodReturn, 
               period     = "monthly", 
               col_rename = "Ra") 

Ra_LMT_2007 <- tickers %>%
  tq_get(get  = "stock.prices",
         from = "2006-12-01",
         to = "2007-11-01") %>%
  group_by(symbol) %>%
  tq_transmute(select     = adjusted, 
               mutate_fun = periodReturn, 
               period     = "monthly", 
               col_rename = "Ra") 

Ra_LMT_2020 <- tickers %>%
  tq_get(get  = "stock.prices",
         from = "2019-01-02",
         to = "2019-12-31") %>%
  group_by(symbol) %>%
  tq_transmute(select     = adjusted, 
               mutate_fun = periodReturn, 
               period     = "monthly", 
               col_rename = "Ra")

# Get std and average stock return
std_2001 <- data.frame(matrix(ncol = 4, nrow = 0))

for (value in symb_2001) {
  curr <- subset(RaRb_single_portfolio_2001, symbol==value)
  std <- round(sd(curr$Ra)*100,2) 
  mean <- round(mean(curr$Ra)*100,2)
  std_2001[nrow(std_2001) + 1,] <- c(value, std, mean, 2001)
}

colnames(std_2001) <- c('symbol','LMT_std', 'mean', 'period')

RaRb_capm_comb_2001 <- left_join(RaRb_capm_2001, 
                            std_2001,
                            by = "symbol") 
RaRb_capm_comb_2007 <- left_join(RaRb_capm_2007, 
                                 std_2007,
                                 by = "symbol") 
RaRb_capm_comb_2020 <- left_join(RaRb_capm_2020, 
                                 std_2020,
                                 by = "symbol") 

std_2007 <- data.frame(matrix(ncol = 4, nrow = 0))

for (value in symb_2007) {
  curr <- subset(RaRb_single_portfolio_2007, symbol==value)
  std <- round(sd(curr$Ra)*100,2) 
  mean <- round(mean(curr$Ra)*100,2)
  std_2007[nrow(std_2007) + 1,] <- c(value, std, mean, 2007)
}

colnames(std_2007) <- c('symbol','LMT_std', 'mean', 'period')


std_2020 <- data.frame(matrix(ncol = 4, nrow = 0))

for (value in symb_2020) {
  curr <- subset(RaRb_single_portfolio_2020, symbol==value)
  std <- round(sd(curr$Ra)*100,2) 
  mean <- round(mean(curr$Ra)*100,2)
  std_2020[nrow(std_2020) + 1,] <- c(value, std, mean, 2020)
}

colnames(std_2020) <- c('symbol','LMT_std', 'mean', 'period')

# get economic factors data (interest, unemp)
eco_factors <- read_csv("/Users/baovo/Documents/GitHub/Team-14/data/fred.csv")
colnames(eco_factors) <- c('date', 'FEDFUNDS', 'GDP', 'UNRATE', 'PAYEMS', 'ICSA', 'CPIAUCSL', 'USREC')
eco_factors$date <- as.Date(eco_factors$date, format = "%Y-%m-%d")

eco_factors_2001 <- filter(eco_factors, date>="2001-01-02" & date<="2001-10-02")
RaRb_capm_comb_2001$interest_mean <- round(mean(eco_factors_2001$FEDFUNDS),1)
RaRb_capm_comb_2001$unemp_mean <- round(mean(eco_factors_2001$UNRATE),1)

eco_factors_2007 <- filter(eco_factors, date>="2007-12-01" & date<="2009-06-30")
RaRb_capm_comb_2007$interest_mean_2007 <- round(mean(eco_factors_2007$FEDFUNDS),1)
RaRb_capm_comb_2007$unemp_mean <- round(mean(eco_factors_2007$UNRATE),1)

eco_factors_2020 <- filter(eco_factors, date>="2020-01-01" & date<="2020-09-30")
RaRb_capm_comb_2020$interest_mean_2020 <- round(mean(eco_factors_2020$FEDFUNDS),1)
RaRb_capm_comb_2020$unemp_mean <- round(mean(eco_factors_2020$UNRATE),1)

tickers_w_symbol <- read_csv("tickers_w_sectors.csv") 
colnames(tickers_w_symbol) <- c("symbol", "sector")

RaRb_capm_comb_final_2001 <- left_join(RaRb_capm_comb_2001, 
                                       tickers_w_symbol,
                                       by = "symbol") 
RaRb_capm_comb_final_2007 <- left_join(RaRb_capm_comb_2007, 
                                       tickers_w_symbol,
                                       by = "symbol") 
RaRb_capm_comb_final_2020 <- left_join(RaRb_capm_comb_2020, 
                                       tickers_w_symbol,
                                       by = "symbol") 

RaRb_capm_comb_final_2001 <- RaRb_capm_comb_final_2001 %>% drop_na()
RaRb_capm_comb_final_2007 <- RaRb_capm_comb_final_2007 %>% drop_na()
RaRb_capm_comb_final_2020 <- RaRb_capm_comb_final_2020 %>% drop_na()

RaRb_capm_comb_final_2001$sector <- as.factor(RaRb_capm_comb_final_2001$sector)
RaRb_capm_comb_final_2007$sector <- as.factor(RaRb_capm_comb_final_2007$sector)
RaRb_capm_comb_final_2020$sector <- as.factor(RaRb_capm_comb_final_2020$sector)

RaRb_capm_comb_final_2001$LMT_std <- as.double(RaRb_capm_comb_final_2001$LMT_std)
RaRb_capm_comb_final_2001$mean <- as.double(RaRb_capm_comb_final_2001$mean)
RaRb_capm_comb_final_2001$performance <- as.integer(RaRb_capm_comb_final_2001$performance)
RaRb_capm_comb_final_2007$LMT_std <- as.double(RaRb_capm_comb_final_2007$LMT_std)
RaRb_capm_comb_final_2007$mean <- as.double(RaRb_capm_comb_final_2007$mean)
RaRb_capm_comb_final_2007$performance <- as.integer(RaRb_capm_comb_final_2007$performance)
RaRb_capm_comb_final_2020$LMT_std <- as.double(RaRb_capm_comb_final_2020$LMT_std)
RaRb_capm_comb_final_2020$mean <- as.double(RaRb_capm_comb_final_2020$mean)
RaRb_capm_comb_final_2020$performance <- as.integer(RaRb_capm_comb_final_2020$performance)
colnames(RaRb_capm_comb_final_2020) <- c("symbol", "Alpha", "performance", "LMT_std", "mean", "period", "interest_mean", "unemp_mean", "sector")

combine <- bind_rows(RaRb_capm_comb_final_2001, RaRb_capm_comb_final_2007, RaRb_capm_comb_final_2020)
combine_converted <- combine %>% 
  mutate(con_dis = ifelse(sector == "Consumer Discretionary", 1, 0)) %>%
  mutate(fin = ifelse(sector == "Finance", 1, 0)) %>%
  mutate(inds = ifelse(sector == "Industrials", 1, 0)) %>%
  mutate(real_est = ifelse(sector == "Real Estate", 1, 0)) %>%
  mutate(tech = ifelse(sector == "Technology", 1, 0)) %>%
  mutate(tele = ifelse(sector == "Telecommunications", 1, 0)) 
  
logistic<- glm(performance ~ LMT_std + mean + interest_mean + unemp_mean + sector, data=combine, family=binomial(link='logit'))

logistic_conv <- glm(performance ~ LMT_std + mean + interest_mean + unemp_mean + con_dis + fin + inds
                     + real_est + tech + tele, data=combine_converted, family=binomial(link='logit'))

logistic_conv_revise <- glm(performance ~ LMT_std + mean + interest_mean + unemp_mean + fin + inds
                     + real_est, data=combine_converted, family=binomial(link='logit'))

logistic_conv_revise1 <- glm(performance ~ LMT_std + mean + interest_mean + unemp_mean + fin
                            + real_est, data=combine_converted, family=binomial(link='logit'))

summary(logistic)
summary(logistic_conv)
summary(logistic_conv_revise)

colnames(logistic_2001) <- c('symbol', 'alpha', 'LMT_std', 'mean', ' interest_mean', 'unemp', 'sector')
# create an xts dataset
All.dat<-xts(RaRb_single_portfolio[,-2],order.by=RaRb_single_portfolio$date)

# calculate the Compounded Return
Return.cumulative(All.dat$ContraRet, geometric = TRUE)

#write.csv(as.data.frame(RaRb_capm_comb_final_2001), "/Users/baovo/Documents/GitHub/Team-14/Recession-Proof_Portfolio/Data/FINAL_TABLE_DATA_2001.csv")
#write.csv(as.data.frame(RaRb_capm_comb_final_2007), "/Users/baovo/Documents/GitHub/Team-14/Recession-Proof_Portfolio/Data/FINAL_TABLE_DATA_2007.csv")
#write.csv(as.data.frame(RaRb_capm_comb_final_2020), "/Users/baovo/Documents/GitHub/Team-14/Recession-Proof_Portfolio/Data/FINAL_TABLE_DATA_2020.csv")
#write.csv(as.data.frame(combine), "/Users/baovo/Documents/GitHub/Team-14/Recession-Proof_Portfolio/Data/COMBINED.csv")


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
