require("rvest")

getSecInd = function(x)
{
  Sys.sleep(1)
  url = paste0("https://seekingalpha.com/symbol/",x)
  a <- suppressWarnings(read_html(url))
  
}

tickers_curr <- c("AAPL", "MMM")
tickers_w_sector <- lapply(as.list(tickers_curr), function(x)
{
  tmp <- try(getSecInd(x))
})
Sector <- a %>% html_nodes("div div a") %>% .[6] %>% html_text()
Sector <- as.data.frame(Sector)
tic <- as.data.frame(x)
colnames(tic) <- "symbol"
cbind(tic, Sector)