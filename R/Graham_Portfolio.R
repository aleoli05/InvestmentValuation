#' Graham_Portfolio
#' Return Graham portfolio from Yahoo data
#' Attend the restriction give for Graham in the Intelligent Investor
#' @param Symbols Tickers to import and analyzes the financial parameters.
#' For all SP500 use 'Current_SP500_Tickers'
#' @examples
#' # example code
#' Graham_Portfolio(c("XOM","MSFT","JNJ","GE","CVX","WFC","PG","JPM","VZ","PFE","T","IBM","MRK","BAC","DIS","ORCL","PM","INTC","SLB"))

#' @export
Graham_Portfolio <- function(Symbols){
library(quantmod)
library(plyr)
library(dplyr)
library(writexl)
###############################################################################
library(rvest)
Tickers='Current_SP500_Tickers'
Tickers_1 = Tickers
options(warn=-1)
# get the URL for the wikipedia page with all SP500 symbols
url <- "https://en.wikipedia.org/wiki/List_of_S%26P_500_companies"
# use that URL to scrape the SP500 table using rvest
tickers_ <- url %>%
  # read the HTML from the webpage
  read_html() %>%
  # one way to get table
  #html_nodes(xpath='//*[@id="mw-content-text"]/div/table[1]') %>%
  # easier way to get table
  html_nodes(xpath = '//*[@id="constituents"]') %>%
  html_table()
#create a vector of tickers
sp500tickers <- tickers_[[1]]
sp500tickers = sp500tickers %>% mutate(Symbol =
                                         case_when(Symbol == "BRK.B" ~ "BRK-B",
                                                   Symbol == "BF.B" ~ "BF-B",
                                                   TRUE ~ as.character(Symbol)))
Current_SP500<-sp500tickers$Symbol
#######

x=as.numeric(any(c('Current_SP500_Tickers') %in% Tickers_1))
if (x==1) {
  y=which(Tickers_1 %in% c('Current_SP500_Tickers'))
  Tickers_2=Tickers[-y]

  z=as.numeric(any(Tickers_2 %in% Current_SP500))
  if (z==1) {
    h=which(Tickers_2 %in% Current_SP500)
    Tickers_3=Tickers_2[-h]
    Tick=c(Tickers_3,Current_SP500)
  }

  Tick=c(Tickers_2,Current_SP500)
} else {
  Tick=Tickers_1

}


#######
Tickers = Symbols
Tickers_1 = Symbols
x=as.numeric(any(c('Current_SP500_Tickers') %in% Tickers_1))
if (x==1) {
  y=which(Tickers_1 %in% c('Current_SP500_Tickers'))
  Tickers_2=Tickers[-y]

  z=as.numeric(any(Tickers_2 %in% Current_SP500))
  if (z==1) {
    h=which(Tickers_2 %in% Current_SP500)
    Tickers_3=Tickers_2[-h]
    Tick=c(Tickers_3,Current_SP500)
  }

  Tick=c(Tickers_2,Current_SP500)
} else {
  Tick=Tickers_1

}

#####
Symbols=Tick


##############################################################################


what_metrics <- yahooQF(c("Price/Sales",
                          "P/E Ratio",
                          "Price/EPS Estimate Next Year",
                          "PEG Ratio",
                          "Dividend Yield",
                          "Market Capitalization",
                          "Book Value"
                          ))

#Symbols<-c("XOM","MSFT","JNJ","GE","CVX","WFC","PG","JPM","VZ","PFE","T","IBM","MRK","BAC","DIS","ORCL","PM","INTC","SLB")


metrics <- quantmod::getQuote(paste(Symbols, sep="", collapse=";"), what=what_metrics)

################################################################################
#Graham_Portfolio
####################


Market_to_Book = metrics[,5]/(metrics[,6]*1000000000)
### P/E x Market to Book
Graham_indicator = metrics[,2]*Market_to_Book

Graham = data.frame(metrics,Market_to_Book,Graham_indicator,rownames(metrics))
Graham = arrange(as.data.frame(Graham), desc(Graham[,2]))
Graham = arrange(as.data.frame(Graham), Graham[,2])
Graham = na.omit(Graham)
View(Graham)

#Graham_filter = Graham %>% filter(between(Graham$Graham_indicator, 0, 21.5))

### Filter 1: Market Capitalization >= 2 Billions
Graham_filter0=Graham[Graham$Market.Capitalization  >= 2000000000,]

### Filter 2: P/E ratio <=15
Graham_filter1=Graham_filter0[Graham_filter0$P.E.Ratio <= 15,]

### Filter 3: Marketo to Book < 1.5
Graham_filter2=Graham_filter1[Graham_filter1$Market_to_Book <= 1.5,]

### Filter 4: 0 <= Graham Indicator <= 21.5
Graham_filter=Graham_filter2[Graham_filter2$Graham_indicator >= 0 & Graham_filter2$Graham_indicator <= 21.5,]

### Filter 5: Max Dividend
Graham_filter3 = arrange(as.data.frame(Graham_filter), desc(Graham_filter[,4]))
View(Graham_filter3)

if (nrow(Graham_filter3)<20){
  corte=nrow(Graham_filter3)
} else {corte=20}

Graham_My_Portfolio = Graham_filter3[1:corte,]

save(Graham_My_Portfolio, file='~/Graham_My_Portfolio.rda')
write_xlsx(as.data.frame(Graham_My_Portfolio), '~/Graham_My_Portfolio..xlsx')
View(Graham_My_Portfolio)
########################################

library(quantmod)

Symbols<-c("XOM","MSFT","JNJ","GE","CVX","WFC","PG","JPM","VZ","PFE","T","IBM","MRK","BAC","DIS","ORCL","PM","INTC","SLB")

StartDate <- as.Date('2015-01-01')

Stocks <-  lapply(Symbols, function(sym) {
  Cl(na.omit(getSymbols(sym, from=StartDate, auto.assign=FALSE)))
})


Stocks <- do.call(merge, Stocks)


################################################################################
# Metrics import from Yahoofinance
######################################
# NOT RUN {
yahooQuote.EOD
# }
# NOT RUN {
getQuote("AAPL")
getQuote("QQQQ;SPY;^VXN",what=yahooQF(c("Bid","Ask")))
standardQuote()
yahooQF()
# }
################################################################################
}
