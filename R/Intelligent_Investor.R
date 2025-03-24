#' Intelligent_Investor
#' #' Return Graham portfolio from FinVis data
#' Attend the restriction give for Graham in the Intelligent Investor
#'
#' @param Tickers to import and analyzes the financial parameters.
#' For all SP500 use 'Current_SP500_Tickers'
#' @param AQ Annual Statements (A) or Quarterly Statements (Q)
#' @param Size Minimum Total Assets in Millions
#' @param PE_Ratio Maximum Price/Earnings Ratio
#' @param PB_Ratio Maximum Price to Book Ratio
#' @param GI_min Graham indicator minimum
#' @param GI_max Graham indicator maximum
#' @param CR Minimum Current Ratio
#' @param EPS Earning per Share Minimum
#' @param Break Maximum number of stocks in the portfolio
#' @param Exclude_ticket Deletes any ticket from the ticket list that you want to remove for some reason
#' @param Plot_IS Chart of Income Sheet account selected
#' @param Plot_CF Chart of Cash Flow account selected
#' @param Plot_BS Char of Balance Sheet accoun selected
#'
#' @examples
#' Tickers = 'Current_SP500_Tickers'
#' AQ = 'A'
#' Size = 2000
#' PE_Ratio = 15
#' PB_Ratio = 1.5
#' GI_min = 0
#' GI_max = 21.5
#' EPS = 0
#' Exclude_ticket = c('AZO','GL','TPL')
#' Intelligent_Investor(Tickers,AQ='A', Size=2000, PE_Ratio=15, PB_Ratio=1.5, GI_min=0, GI_max=21.5, CR=2, EPS=0, Break=15)
#'
#' @export
Intelligent_Investor <- function(Tickers, AQ='A', Size=2000, PE_Ratio=15, PB_Ratio=1.5,
                                 GI_min=0, GI_max=21.5, CR=2, EPS=0, Break=15,Plot_IS='Total Revenue',
                                 Plot_CF='Cash Dividends Paid',
                                 Plot_BS='Total Liabilities',
                                 Exclude_ticket=''){
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
  #######################################################################
  # Import the statements for all companies required
  Graham_matrix = matrix(nrow=7,ncol=length((Tick)))
  nomes_linhas=c('Size_Total_Assets', 'Current_ratio', 'EPS_Diluted',
                 'Dividends_Yield', 'PE_Ratio', 'Price_to_Book',
                 'Graham_Indicator')
  rownames(Graham_matrix)=nomes_linhas
  colnames(Graham_matrix)=Tick

  lista_matrizes <- lapply(1:7, function(x) matrix(nrow=7,ncol=length((Tick))))

  for (k in 1:7){
    rownames(lista_matrizes[[k]])=nomes_linhas
    colnames(lista_matrizes[[k]])=Tick
  }

  Excluidos=c()
  for (i in 1:length((Tick))){
    sym=Tick[i]
    Number= which(Tick==sym)
    Comprimento= length(Tick)
    mensagem = paste(sym, ' -> Number:',Number,' of ', Comprimento, ' stocks', sep='')
    print('************************************')
    print(mensagem)
    Fundamentals_analysis(sym=sym,AQ='A', Plot_IS=Plot_IS,
                          Plot_CF=Plot_CF,
                          Plot_BS=Plot_BS)

    load ('~/bs.rda')
    load ('~/cf.rda')
    load ('~/is.rda')

    ###############################################
    ## j = Create a matrix for each year or quarter
    for (j in 1:7){
      if(ncol(bs)>=j){
      # Filter 1: Adequate_Size > 2 billions
      lista_matrizes[[j]][1,i] = bs[which(rownames(bs)=='Total Assets'),j]


      # Filter 2: Current_ratio >2
         ###### Corrections in data
          x=as.numeric(any(c('Current Ratio') %in% bs))
          if(x==1){
            lista_matrizes[[j]][2,i] = bs[which(rownames(bs)=='Current Ratio'),j]
          }else{
            lista_matrizes[[j]][2,i] = (bs[1,j]+bs[2,j])/bs[which(rownames(bs)=='Total Liabilities'),j]
            Excluidos=append(Excluidos,Tick[i])
          }

      # Filter 3: Earning stability in then years >0
      lista_matrizes[[j]][3,i]=is[which(rownames(is)=='EPS (Basic, Before Extraordinaries)'),j+1]

      # Filter 4: Dividend record in all years
      ### Dividends determination
        z=as.numeric(any(c('EPS (Diluted)') %in% bs))
            if (z==1){
            Dividends = is[which(rownames(is)=='EPS (Diluted)')]
            } else{
            Dividends = is[which(rownames(is)=='EPS (Basic, Before Extraordinaries)'),j+1]
            }
      lista_matrizes[[j]][4,i]=round(abs(cf[which(rownames(cf)=='Cash Dividends Paid'),j+1])/
                                       (is[which(rownames(is)=='Shares Outstanding'),j+1])*Dividends, 2)

      # Filter 5: P/E ratio < 15
      lista_matrizes[[j]][5,i]= is[which(rownames(is)== 'Price To Earnings Ratio'),j+1]

      # Filter 6: Price to Book < 2.5
      lista_matrizes[[j]][6,i] = bs[which(rownames(bs)== 'Price to Book Ratio'),j]

      # Filter 7: Graham Indicator = (P/E) x (Price to Book) < 21.5
      lista_matrizes[[j]][7,i] = round(is[which(rownames(is)== 'Price To Earnings Ratio'),j+1]/
                                         bs[which(rownames(bs)== 'Price to Book Ratio'),j], 2)

      # Filter 8: Cut 15 stocks


      # Filter 9: Select stocks with good S&P rating ranking
      } else{
        lista_matrizes[[j]][1,i] = 0
        lista_matrizes[[j]][2,i] = 0
        lista_matrizes[[j]][3,i] = 0
        lista_matrizes[[j]][4,i] = 0
        lista_matrizes[[j]][5,i] = 0
        lista_matrizes[[j]][6,i] = 0
        lista_matrizes[[j]][7,i] = 0
      }

      ### Need save Graham matrix for each year or quarterly
      date_matrix = paste('~/lista_matrizes.rda', sep='')
      date_matrix2 = paste('~/lista_matrizes.xlsx', sep='')
      save(lista_matrizes, file=date_matrix)
      write_xlsx(as.data.frame(lista_matrizes), date_matrix2)
    } # End for j
  } # End for i
  save(Excluidos,file='~/Excluidos.rda')

  Results = matrix(ncol=7,nrow=Break)
  colnames(Results)=colnames(bs[1:7])


  for (j in 1:7){
    Graham_filter0= data.frame(t(do.call(rbind,lista_matrizes[j])))

    ### Filter 1: Market Capitalization >= 2 Billions
    Graham_filter1=Graham_filter0[Graham_filter0$Size_Total_Assets  >= Size,]

    ### Filter 2: Current ratio > 2
    Graham_filter2=Graham_filter1[Graham_filter1$Current_ratio >= CR,]

    ### Filter 3: Earning Stability > 0
    Graham_filter3=Graham_filter2[Graham_filter2$EPS_Diluted >= EPS,]

    ### Filter 4: P/E ratio <=15
    Graham_filter4=Graham_filter3[Graham_filter3$PE_Ratio <= PE_Ratio,]

    ### Filter 5: Market to Book < 1.5
    Graham_filter5=Graham_filter4[Graham_filter4$Price_to_Book <= PB_Ratio,]

    ### Filter 6: 0 <= Graham Indicator <= 21.5
    Graham_filter6=Graham_filter5[Graham_filter5$Graham_Indicator >= GI_min & Graham_filter5$Graham_Indicator <= GI_max,]

    ### Filter 7: Max Dividend
    Graham_filter7 = dplyr::arrange(as.data.frame(Graham_filter6), desc(Graham_filter6[,4]))
    Graham_filter7 = na.omit(Graham_filter7)

    ### Filter 8: Break - Maximum number of stocks
    Graham_filter8 = Graham_filter7[1:Break,]
    View(Graham_filter8)

    Results[,j] = rownames(Graham_filter8)
    Data = colnames(bs[j])
    Data = str_replace_all(Data,"/",'_')
    nome_Graham = paste('~/Graham_portfolio_',Data,'.rda',sep='')
    assign(paste('Graham_portfolio_',Data,sep=''),value=Graham_filter8)
    save(Graham_filter8, file=nome_Graham)
    nome_G=paste('Graham_portfolio_',Data,sep='')
    #save(assign(paste('Graham_portfolio_',Data,sep=''),value=Graham_filter8), file=nome_G)

    Graham_filter82=matrix(nrow=nrow(Graham_filter8), ncol=ncol(Graham_filter8)+1)
    Graham_filter82[,1]=rownames(Graham_filter8)
    Graham_filter82[,2:ncol(Graham_filter82)]=as.matrix(Graham_filter8)
    nomes = c('Stocks',colnames(Graham_filter8))
    colnames(Graham_filter82)=nomes
    nome = paste('~/Graham_Portfolio_',Data,'.xlsx', sep='')
    write_xlsx(as.data.frame(Graham_filter82), nome)


  } # End Filters

  Graham_Portfolio=Results
  print(Graham_Portfolio)

  save(Graham_Portfolio,file='~/Graham_Portfolio.rda')

} # End function
