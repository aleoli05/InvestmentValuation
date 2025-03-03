#' Fundamentals_analysis
#' Return analysis from the Enterprise
#' @examples
#' Fundamentals_analysis()
#' @export
Fundamentals_analysis <- function(){
  require(writexl)
### Get Financial Statments for Analytics
#title: "FinViz Financials"
#output: html_document
#date: Sys.Date()
# variable assigment
sym = "CMG"
AQ = "Q" # for quarterly

## Get All Financials
# get income statment
is = getFins(symbol = sym, AQ = AQ, FS = 'I')
# get cash flow statment
cf = getFins(symbol = sym, AQ = AQ, FS = 'C')
# get balance sheet
bs = getFins(symbol = sym, AQ = AQ, FS = 'B')

## PlotTrends
# IS: plot Total Revenue
png(file="~/Total Revenue.png", width=1920, height=1920, res=296, family = "A")
table2plot(ticker = sym, WHAT = 'Total Revenue', FROM = is)
# CF: plot Total Income
table2plot(ticker = sym, WHAT = 'Net Income', FROM = cf)
# BS: plot Total Liabilities
table2plot(ticker = sym, WHAT = 'Total Liabilities', FROM = bs)

## Balance Sheet as Percentage
# convert balance sheet to percentage
pct_bs <- pctBS(BS=bs)
# print table
DT:: datatable(pct_bs, rownames=TRUE, extensions="Buttons", options = list(dom="Blfrtip",
               buttons = c('csv', 'excel', 'pdf')))

save(is, file='~/is.rda')
save(cf, file='~/cf.rda')
save(bs, file='~/bs.rda')

write_xlsx(as.data.frame(is), "~/Income_Statment.xlsx")
write_xlsx(as.data.frame(cf), "~/Cash_Flow_Statment.xlsx")
write_xlsx(as.data.frame(bs), "~/Balance_Sheet_Statment.xlsx")

}
