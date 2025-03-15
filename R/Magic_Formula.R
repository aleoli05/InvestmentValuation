#' Magic_Formula Generate Magic Formula Portfolio
#' The Joel Greenblatt´s Magic Formula for beating the stock market
#' @param Tickers to import and analyzes the financial parameters.
#' For all SP500 use 'Current_SP500_Tickers'
#' @param AQ Annual Statements (A) or Quarterly Statements (Q)
#' @param Break Portfolio asset numbers to be generated
#' @param Exclude_ticket Deletes any ticket from the ticket list that you want to remove for some reason
#' @param Plot_IS Chart of Income Sheet account selected
#' @param Plot_CF Chart of Cash Flow account selected
#' @param Plot_BS Char of Balance Sheet account selected
#' @examples
#' Tickers = 'Current_SP500_Tickers'
#' AQ = 'A'
#' GI_min = 0
#' GI_max = 21.5
#' EPS = 0
#' Exclude_ticket = c('AZO','GL','TPL')
#' Magic_Formula(Tickers='Current_SP500_Tickers',AQ='A',Break=20, Plot_IS='', Plot_BS='',Plot_CF='',Exclude_ticket = c('AZO','GL','TPL'))
#'
#' @export
Magic_Formula <- function(Tickers, AQ='A', Break=20,Plot_IS='Total Revenue',
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
  Magic_matrix = matrix(nrow=5,ncol=length((Tick)))
  nomes_linhas=c('Tangible_Assets', 'EBIT', 'EV',
                 'ROIC_MAGIC', 'EBIT_EV')
  rownames(Magic_matrix)=nomes_linhas
  colnames(Magic_matrix)=Tick

  lista_Magic <- lapply(1:7, function(x) matrix(nrow=5,ncol=length((Tick))))

  for (k in 1:7){
    rownames(lista_Magic[[k]])=nomes_linhas
    colnames(lista_Magic[[k]])=Tick
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
        # Filter 1: Tangible Assets
        lista_Magic[[j]][1,i] = bs[which(rownames(bs)=='Net Property, Plant & Equipment'),j]


        # Filter 2: EBIT
        ###### Corrections in data

        Operating_Income=as.numeric(which(grepl('Operating Income',rownames(is))))
        x=as.numeric(any(rownames(is) =='Unusual Expense'))
        if(x==1){
          lista_Magic[[j]][2,i] = is[Operating_Income,j]-
                                     is[which(rownames(is)=='Unusual Expense'),j]
        }else{
          lista_Magic[[j]][2,i] = is[Operating_Income,j]
          Excluidos=append(Excluidos,Tick[i]) # Armazena ativos sem despesas não operacionais
        }

        # Filter 3: Enterprise Value
        w=as.numeric(any(c('Total Debt') %in% is))
        if(w==1){
        lista_Magic[[j]][3,i]=bs[which(rownames(bs)=='Total Assets'),j]*
                                 bs[which(rownames(bs)=='Price to Book Ratio'),j]+
                                 bs[which(rownames(bs)=='Total Debt'),j]-
                                 cf[which(rownames(cf)=='Free Cash Flow'),j+1]
        }else{
          k=as.numeric(any(grepl('Debt',rownames(bs))))
          if(k==1){
          h = which(grepl('Debt',rownames(bs)))
          debt=0
          for (q in (1:length(h))){
            debt=debt+bs[q,j]
          }}else{
            debt=0
          }
          lista_Magic[[j]][3,i]=bs[which(rownames(bs)=='Total Assets'),j]*
                                debt-
                                cf[which(rownames(cf)=='Free Cash Flow'),j+1]

        }
        # Filter 4: ROIC_Magic = EBIT/Tangible Assets
        lista_Magic[[j]][4,i]=lista_Magic[[j]][2,i]/
                                         lista_Magic[[j]][1,i]

        # Filter 5: Company Income = EBIT/
        lista_Magic[[j]][5,i]= (lista_Magic[[j]][2,i]/
                                          lista_Magic[[j]][3,i])

      } else{
        lista_Magic[[j]][1,i] = 0
        lista_Magic[[j]][2,i] = 0
        lista_Magic[[j]][3,i] = 0
        lista_Magic[[j]][4,i] = 0
        lista_Magic[[j]][5,i] = 0
      }

      ### Need save Magic matrix for each year or quarterly
      date_matrix = paste('~/lista_Magic.rda', sep='')
      date_matrix2 = paste('~/lista_Magic.xlsx', sep='')
      save(lista_Magic, file=date_matrix)
      write_xlsx(as.data.frame(lista_Magic), date_matrix2)
    } # End for j
  } # End for i
  save(Excluidos,file='~/Excluidos.rda')

  Results = matrix(ncol=7,nrow=Break)
  colnames(Results)=colnames(bs[1:7])
################## Magic Resuls
  Magic <- lapply(1:7, function(x) matrix(ncol=4,nrow=length((Tick))))

  for (k in 1:7){
    Magic[[k]][,1]=Tick
    colnames(Magic[[k]])=c('Ticker','Ranking_EBIT/Tangible_Assets','Ranking_EBIT/EV','Ranking_Magic')
  }

  for (j in 1:7){
    Magic[[j]][,2]=order(lista_Magic[[j]][4,])

    ### Filter 1: Market Capitalization >= 2 Billions
    Magic[[j]][,3]=order(lista_Magic[[j]][5,])

    ### Filter 2: Break - Maximum number of stocks
    Magic[[j]][,4] = as.numeric(Magic[[j]][,2]) + as.numeric(Magic[[j]][,3])


    ### Filter 8: Break - Maximum number of stocks
    Magic[[j]]= arrange(as.data.frame(Magic[[j]]), desc(as.numeric(Magic[[j]][,4])))

    Results[,j] = Magic[[j]][1:Break,1]
    Data = colnames(bs[j])



    Data = str_replace_all(Data,"/",'_')
    nome_Magic = paste('~/Magic_portfolio_',Data,'.rda',sep='')
    assign(paste('Magic_portfolio_',Data,sep=''),value=Magic[[j]])
    Magics = as.matrix(Magic[[j]])
    save(Magics, file=nome_Magic)
    nome_G=paste('Magic_portfolio_',Data,sep='')
    #save(assign(paste('Magic_portfolio_',Data,sep=''),value=Magic_filter2), file=nome_G)

    Magic_filter22=matrix(nrow=nrow(Magics), ncol=ncol(Magics)+1)
    Magic_filter22[1:ncol(Magic_filter22)]=as.matrix(Magics)
    nome = paste('~/Magic_Portfolio_',Data,'.xlsx', sep='')
    write_xlsx(as.data.frame(Magic_filter22), nome)

  } # End Filters
  save(Magic, file='~/Magic.rda')
  save(Results, file='~/Results.rda')
  Magic_Portfolio=Results
  print("************************************")
  print("This is the Magic Portfolio for each period:")
  print(Magic_Portfolio)

  save(Magic_Portfolio,file='~/Magic_Portfolio.rda')

}
