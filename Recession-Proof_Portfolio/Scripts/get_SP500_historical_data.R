# Load dependencies
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, rvest)
library(dplyr)
library(readr)

wikispx <- read_html('https://en.wikipedia.org/wiki/List_of_S%26P_500_companies')
currentconstituents <- wikispx %>%
  html_node('#constituents') %>%
  html_table(header = TRUE)

# currentconstituents

spxchanges <- wikispx %>%
  html_node('#changes') %>%
  html_table(header = FALSE, fill = TRUE) %>%
  filter(row_number() > 2) %>% # First two rows are headers
  `colnames<-`(c('Date','AddTicker','AddName','RemovedTicker','RemovedName','Reason')) %>%
  mutate(Date = as.Date(Date, format = '%B %d, %Y'),
         year = year(Date),
         month = month(Date))

spxchanges

# Start at the current constituents...
currentmonth <- as.Date(format(Sys.Date(), '%Y-%m-01'))
monthseq <- seq.Date(as.Date('1969-04-01'), currentmonth, by = 'month') %>% rev()

spxstocks <- currentconstituents %>% mutate(Date = currentmonth) %>% select(Date, Ticker = Symbol, Name = Security, Sector = `GICS Sector`)
lastrunstocks <- spxstocks

# Iterate through months, working backwards
for (i in 2:length(monthseq)) {
  d <- monthseq[i]
  y <- year(d)
  m <- month(d)
  changes <- spxchanges %>% 
    filter(year == year(d), month == month(d)) 
  
  # Remove added tickers (we're working backwards in time, remember)
  tickerstokeep <- lastrunstocks %>% 
    anti_join(changes, by = c('Ticker' = 'AddTicker')) %>%
    mutate(Date = d)
  
  # Add back the removed tickers...
  tickerstoadd <- changes %>%
    filter(!RemovedTicker == '') %>%
    transmute(Date = d,
              Ticker = RemovedTicker,
              Name = RemovedName)
  
  thismonth <- tickerstokeep %>% bind_rows(tickerstoadd)
  spxstocks <- spxstocks %>% bind_rows(thismonth)  
  
  lastrunstocks <- thismonth
}

# use for period 1969-04-01 to 1971-01-01
sp500_1971 <- filter(spxstocks, Date == '1969-04-01')

# use for period 1973-10-01 to 1975-04-01
sp500_1975 <- filter(spxstocks, Date == '1973-10-01')

# use for period 1979-04-01 to 1980-07-01
sp500_1980 <- filter(spxstocks, Date == '1979-04-01')

# use for period 1981-04-01 to 1982-07-01
sp500_1982 <- filter(spxstocks, Date == '1981-04-01')

# use for period 1989-10-01 to 1991-04-01
sp500_1991 <- filter(spxstocks, Date == '1989-10-01')

# use for period 2001-01-01 to 2001-10-01
sp500_2001 <- filter(spxstocks, Date == '2001-01-01')

# use for period 2007-10-01 to 2009-07-01
sp500_2007 <- filter(spxstocks, Date == '2007-10-01')

# use for period 2020-01-01 to 2020-07-01
sp500_2020 <- filter(spxstocks, Date == '2020-01-01')

combined <- do.call("rbind", list(sp500_1971, sp500_1975, sp500_1980, sp500_1982, sp500_1991, sp500_2001, sp500_2007, sp500_2020))

# write.csv(as.data.frame(combined), "/Users/baovo/Documents/GitHub/Team-14/Recession-Proof_Portfolio/Data/sp500_bear_periods.csv")
