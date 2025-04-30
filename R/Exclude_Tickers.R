#' Exclude_Tickers
#' #' Exclud tickers that not have cash flow statment published in FinVis

#' @param Tickers to import and analyzes the financial parameters.
#' For all SP500 use 'Current_SP500_Tickers'
#' @param AQ Annual Statements (A) or Quarterly Statements (Q)
#' @param FS = Financial Statement (FS): Income Statement (I), Balance Sheet (B), Cash Flow (C)
#' @param Exclude_ticket Deletes any ticket from the ticket list that you want to remove for some reason
#' @examples
#' Exclude_Tickers('Current_SP500_Tickers', 'Q', 'C')

#' @export
Exclude_Tickers <- function(Tickers, AQ='A', FS='C',Exclude_ticket=''){
  library(quantmod)
  library(plyr)
  library(dplyr)
  library(writexl)
  library(stringr)
  ###############################################################################
  # Tickers to use
  ###############################################################################
  library(rvest)
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
  ###############Exclude_ticket
  Exclude=NULL
  for (i in 1:length(Exclude_ticket)) {
    Exclude[i]<-which(Tick==Exclude_ticket[i])
    #Exclude<-Tickers[!Exclude_ticket]
  }
  if(length(Exclude)!=0){
    Tick=Tick[-Exclude]
  }
  ###############
  require('httr');require('highcharter');require('quantmod');require('scales');require('DT')
  require('writexl')
  require('stringr')
  Exclusao_ausentes=c()

  options(warn=-1)


  for (i in 1:length((Tick))){
    symbol=Tick[i]
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
  if(length(tbl)==0){
    print(paste(i,': ',Tick[i],': The selected Financial Statement has not been released', sep=''))
     Exclude_Tickers_Missing=append(Exclude_Tickers_Missing,Tick[i])
  }
  }
  save(Exclude_Tickers_Missing, file='~/Exclude_Tickers_Missing.rda')
  print(Exclude_Tickers_Missing)
}
