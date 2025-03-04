#' Fundamentals_analysis
#' Return analysis from the Enterprise
#' @param sym Ticker
#' @param AQ = Annual (A) or Quarterly (Q)?
#' @examples
#' Fundamentals_analysis('TSLA','A')
#' @export
Fundamentals_analysis <- function(sym,AQ){
  require(writexl)
### Get Financial Statments for Analytics
#title: "FinViz Financials"
#output: html_document
#date: Sys.Date()
# variable assigment
#sym = "CMG"
#AQ = "Q" # for quarterly

## Get All Financials

#################################
# get income statment
getFins(symbol = sym, AQ = AQ, FS = 'I')
load('~/is.rda')
## PlotTrends
# IS: plot Total Revenue
table2plot(ticker = sym, WHAT = 'Total Revenue', FROM = is)
#################################


#get cash flow statment
getFins(symbol = sym, AQ = AQ, FS = 'C')
load('~/cf.rda')
# CF: plot Total Income
table2plot(ticker = sym, WHAT = 'Net Income', FROM = cf)
#################################


# get balance sheet
getFins(symbol = sym, AQ = AQ, FS = 'B')
load('~/bs.rda')
# BS: plot Total Liabilities
table2plot(ticker = sym, WHAT = 'Total Liabilities', FROM = bs)
#################################


## Balance Sheet as Percentage
# convert balance sheet to percentage
pctBS(BS=bs)

}
