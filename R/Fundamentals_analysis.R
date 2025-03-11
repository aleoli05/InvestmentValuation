#' Fundamentals_analysis
#' Return analysis from the Enterprise
#' @param sym Ticker
#' @param AQ = Annual (A) or Quarterly (Q)?
#' @param Plot_IS Chart of Income Sheet account selected
#' @param Plot_CF Chart of Cash Flow account selected
#' @param Plot_BS Char of Balance Sheet accoun selected
#' @examples
#' Fundamentals_analysis('TSLA','A',Plot_IS='Total Revenue',Plot_CF='Cash Dividends Paid', Plot_BS='Total Liabilities')
#' @export
Fundamentals_analysis <- function(sym,AQ,Plot_IS='Total Revenue',
                                  Plot_CF='Cash Dividends Paid',
                                  Plot_BS='Total Liabilities'){
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
  if (Plot_IS!=''){
    table2plot(ticker = sym, WHAT = Plot_IS, FROM = is)
  }
#################################


#get cash flow statment
  getFins(symbol = sym, AQ = AQ, FS = 'C')
  load('~/cf.rda')
  # CF: plot Total Income
  if (Plot_CF!=''){
    table2plot(ticker = sym, WHAT = Plot_CF, FROM = cf)
  }
#################################


# get balance sheet
  getFins(symbol = sym, AQ = AQ, FS = 'B')
  load('~/bs.rda')
  # BS: plot Total Liabilities
  if (Plot_BS!=''){
    table2plot(ticker = sym, WHAT = Plot_BS, FROM = bs)
  }
#################################


## Balance Sheet as Percentage
# convert balance sheet to percentage
pctBS(BS=bs)

}
