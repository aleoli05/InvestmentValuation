#'getFins - Get Financials
#'Income Statement, Cash Flow Statment, & Balance Sheet
#set options
#hcoptslang <- getOption('highcharter.lang')
#hcoptslang$thousandsSep <-','
#options(highcharter.lang = hcoptslang)

################################################################################
#  Get Financial : Income Statement, Cash Flow Statment, & Balance Sheet
################################################################################
#' @param symbol = ticker symbol
#' @param AQ = Annual (A) or Quarterly (Q)?
#' @param FS = Financial Statement (FS): Income Statement (I), Balance Sheet (B), Cash Flow (C)
#' @examples
#' getFins('TSLA', 'A', 'B')
#'@export
getFins <- function(symbol, AQ, FS){
  require('httr');require('highcharter');require('quantmod');require('scales');require('DT')
  require('writexl')
      # get URL from Network
  URL = paste0('https://finviz.com/api/statement.ashx?t=',symbol,"&so=F&s=",FS,AQ)
  headers =c(
    'Host'='finviz.com',
    'User-Agent'='Mozilla/5.0 ( Mocintosh; Intel Mac 05 X 10.15; rv:132.0) Gecko/20100101 Firefox/132.0',
    'Accept'='*/*',
    'Accept-Language'= 'en-US,en;q=0.5',
    'Accept-Encoding'= 'gzip, deflote, br, zstd',
    'Referer'= 'http://finviz.com/quote.ashx?t=NVDA&p=d',
    'Connection'= 'keep-alive',
    'Cookie'= 'chartsTheme=dark; notice-newsletter=show; pv_date=Thu Dec 05 2024 01:52:09 GMT-0800 (Pacific Standard Time); pv_count=3; _ga_ZT9VQEWD4N=GS1.1.1.1733392370.0.0.0',
    'Sec-Fetch-Dest'= 'empty',
    'Sec-Fetch-Mode'= 'cors',
    'Sec-Fetch-Site'= 'same-origin',
    'Priority'='u=4'
    )
  # submit a get request
  pg = httr::GET(url= URL, add_headers(headers))
  # extract page contents
  res= httr::content(pg)
  # return as table
  tbl=unlist(res$data)
  # extract names for rows
  rName = gsub('1','',unique(names(tbl)[seq(1,length(tbl),8)]))
  # we can use matrix & assign the number of cols
  df=matrix(tbl, ncol=8, byrow=TRUE)
  # add row names
  df=data.frame(df,row.names=rName)
  # re-assign column names
  colnames(df)=df[1,1:ncol(df)]
  # remove first row
  df=df[-1,]
  # convert to numeric
  dfn= data.frame(apply(df, 2, function(x) as.numeric(gsub("\\,","", x))), row.names = rownames(df))
  colnames(dfn)= colnames(df)
  dfn = dfn[rowSums(is.na(dfn)) != ncol(dfn),]
  dfn[is.na(dfn)] <- round(0, 2)
  # return data
  dfn
  save(symbol, file='~/symbol.rda')
  save(dfn, file='~/dfn.rda')
  dfn2=matrix(nrow=nrow(dfn), ncol=ncol(dfn)+1)
  dfn2[,1]=rownames(dfn)
  dfn2[,2:ncol(dfn2)]=as.matrix(dfn)
  nomes = c('Account',colnames(dfn))
  colnames(dfn2)=nomes
  if (FS=='I'){
  tipo = paste('_Income_Statment')
  is = dfn
  nome='is'
  save(nome, file='~/nome.rda')
  save(is, file='~/is.rda')
  }
  if (FS=='C'){
  tipo = paste('_Cash_Flow_Statment')
  cf = dfn
  nome='cf'
  save(nome, file='~/nome.rda')
  save(cf, file='~/cf.rda')
  }
  if (FS=='B'){
  tipo = paste('_Balance_Sheet_statment')
  bs = dfn
  nome='bs'
  save(nome, file='~/nome.rda')
  save(bs, file='~/bs.rda')
  }

  nome2=paste("~/",symbol,tipo,".xlsx",sep='')
  write_xlsx(as.data.frame(dfn2), nome2)
  View(dfn)
}
