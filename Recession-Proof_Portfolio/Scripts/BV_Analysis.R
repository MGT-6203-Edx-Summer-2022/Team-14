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

# company by sector
comp_by_sec <- sp1 %>% group_by(Sector) %>% summarise(count_company = n())

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

# Create a new data frame grouping the sectors together
# with their fundamentals  

df <- sp1 %>%
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
  geom_segment(aes(xend = Sector, yend = 0)) +
  geom_point(size = 4, color = "black", fill = alpha("salmon", 0.4), alpha = 0.7, shape = 21, stroke = 2) +
  coord_flip() +
  xlab("") +
  theme_bw()

t

## Barplot of the sectors with the highest prices.

# Order prices from lowest to highest.
df.price <- df[order(df$avg.price),]

# Make sure sector names are ordered with prices.
df.price$Sector <- factor(df.price$Sector, levels = df.price$Sector)
df.price

# Create the graph.
ggplot(data = df.price, aes(x = Sector, y = avg.price)) +
  geom_bar(stat="identity", fill = "#ff6666", color = "black") +
  scale_x_discrete(labels=c("Telecom", "Utilities", "Energy", "Staples", "Real Estate",
                            "Financials", "Materials", "Industrials", "IT",
                            "Cons. Disc", "Healthcare")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle=55, vjust=0.5))

## Barplot of highest market cap.

# Same as before.
df.cap <- df[order(df$cap),]
df.cap$Sector <- factor(df.cap$Sector, levels = df.cap$Sector)


# So few telecom companies are skewing the data in its favor
ggplot(data = df.cap, aes(x = Sector, y = cap)) +
  geom_bar(stat="identity", fill = "#ff6666", color = "black") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle=55, vjust=0.5))

# Let's fix this by dropping telecom from our factor.

df.cap.new <- subset(df.cap, Sector != "Telecommunication Services")
df.cap.new

ggplot(data = df.cap.new, aes(x = Sector, y = cap)) +
  geom_bar(stat="identity", fill = "#ff6666", color = "black") +
  scale_x_discrete(labels=c("Cons. Disc", "RE", "Materials",
                            "Utilities", "Industrials","Energy",
                            "Financials","IT", "Staples", "Healthcare")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle=55, vjust=0.5))


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

# get return for baseline (^GSPC)
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

# create chart
p <- avg_return %>% head(20) %>% ggplot(aes(reorder(symbol, -avg_return), avg_return, fill = avg_return))+
  geom_col()+
  coord_flip()+
  labs(title = "Top 20 Tickers Average Return in SP500 during Great Depression (2007-2009)", x = "Ticker", y = "Average Return")+
  theme_classic()+
  theme(legend.position="right")
p <- p + guides(fill=guide_legend(title="Avg Return"))
