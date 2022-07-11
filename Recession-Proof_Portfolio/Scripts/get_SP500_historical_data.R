# Load dependencies
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, rvest)

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

spxstocks <- currentconstituents %>% mutate(Date = currentmonth) %>% select(Date, Ticker = Symbol, Name = Security)
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
current_m <- filter(spxstocks, Date >= '1969-04-01' & Date <= '1971-01-01')
current_m
