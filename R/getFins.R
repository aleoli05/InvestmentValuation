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
  require('stringr')
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
    'Cookie'= 'chartsTheme=dark; icAbTest=control; _gcl_au=1.1.1157152402.1740923269;
    _ga=GA1.1.466320403.1740923269; usprivacy=1N--;
    _lr_env_src_ats=false; pbjs-unifiedid=%7B%22TDID%22%3A%220d4c97a6-85ff-447d-ac18-40be1b94146e%22%2C%22TDID_LOOKUP%22%3A%22TRUE%22%2C%22TDID_CREATED_AT%22%3A%222025-02-02T13%3A47%3A58%22%7D;
    pbjs-unifiedid_cst=TyylLI8srA%3D%3D; _unifiedId=%7B%22TDID%22%3A%220d4c97a6-85ff-447d-ac18-40be1b94146e%22%2C%22TDID_LOOKUP%22%3A%22TRUE%22%2C%22TDID_CREATED_AT%22%3A%222025-02-02T13%3A47%3A58%22%7D;
    _unifiedId_cst=VyxHLMwsHQ%3D%3D; _cc_id=a5f030bb108eabc75f572cd320c48249; cnx_userId=2-9edd034765674e72994be24a3ee71d1c; cto_bidid=ExsoaF9jenlEaEl2ZXhybVpOSWZEWU14VGVXJTJCTG9oS2lmbG55SGNBUXVvb2pDUlZWUXlXTzFYOGhYMGJpUEZ2c0FoJTJGRnQ2WHMyNmJIN3owZjF3dlZIMmxoZWs1QkxUU1JBcU9LbExwY0luUXc2QUklM0Q;
    notice-newsletter=show; icAbTest3=control; ic_tagmanager=AY; pv_date=Tue Mar 11 2025 22:20:01 GMT-0300 (HorÃ¡rio PadrÃ£o de BrasÃ­lia); IC_ViewCounter_finviz.com=2; pv_count=2; _lr_geo_location_state=RS; _lr_geo_location=BR; _awl=2.1741742418.5-087a872d88820c2366a80e445135acf8-6763652d75732d6561737431-0;
    _lr_sampling_rate=0; cto_bundle=F8npY19yS3Rkd3pXVHZITndaeWJ2S3BoSTg0YUNPTVhSWFJPZjMwd3FmMUVSMGM0YmJGMFFPVnQ3Q3BjM3ZGUElpbUlhS2FEUDlBR0tpbHJucHpnaGZyU0JhbmpUb3ZXeGhXZzRQNXkzQW5weGJDM1VWbmNIeDRTaVRCeDQxZWNPSzRqcENBWjAyd0xzM2xoWkc5Q2UlMkZwY1lzU1AwNzNiV1Fiamtnd3AxcGdscXZzVkdwSmZadGpmZzclMkJSYnE2WGVjRm5PR0JBRGo1S3lLTzhQdWVQT0NwOVolMkJBJTNEJTNE;
    cto_bundle=6YtK-19yS3Rkd3pXVHZITndaeWJ2S3BoSTgySWp1bUNjUFMlMkZLUjkxMkdFZ2x1SkI2ZWVWa0o2TDdMUlZKUERWa3Y1MzRkeTQ3VzNCRGIlMkJNZW90TUFZS0NieG92SllXdFhZcHNRNlppSEtyRXdRTWVsa3Q5SmRhUkNjcEIyQzRQYyUyRmRhdnNteVZSSlNYYXdiNlZiRnJRemZvSE8xU25yQmdtcUEyM01hY2J5aG8xaG5SMVpJc1JucnRZVjFidFdrR3ROY0ZJNFhwelMzMFJxSFZkdWFVJTJGJTJCZEQxUSUzRCUzRA; _ga_ZT9VQEWD4N=GS1.1.1741742405.3.1.1741742520.60.0.0;
    __gads=ID=625cea49f7eb8b4a:T=1740923276:RT=1741742740:S=ALNI_MalJdSwDRInmKICSkY25QCcrQVw-w; __gpi=UID=0000105b16617a40:T=1740923276:RT=1741742740:S=ALNI_MZ8PlNjMOrQRdp23HnkdmQGJA14lg; __eoi=ID=214eaa91d97fa6aa:T=1740923276:RT=1741742740:S=AA-Afjb0obaJ5xGq9gzUMuv9kgdt',
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
  rName = gsub('1','',unique(names(tbl)[seq(1,length(tbl),9)]))
  # we can use matrix & assign the number of cols
  df=matrix(tbl, ncol=9, byrow=TRUE)
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
  ###### Corrections in data
  Nomes=rownames(dfn)
  Suprime = c('1','2','3','4','5','6','7','8','9')
  for (h in 1:length(Suprime)){
    S=Suprime[h]
    for (y in 1:length(Nomes)){
      Davez = Nomes[y]
      a=stringr::str_ends(Davez, S)
      if (a==TRUE){
        Nomes[y]=str_sub(Nomes[y],end=-2)
      }
    }
  }
  rownames(dfn)=Nomes


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
