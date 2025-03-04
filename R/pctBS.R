#'pctBS - Porcentage Balance Sheet
#'Convert Financial Tables to Percentages
##################################################################################
# pecentage balance sheet
#' @param BS = balance sheet from FinViz
#' @examples
#' pctBS('BS')
#' @export
pctBS = function(BS){
  require(writexl)
  load('~/symbol.rda')
  #location of totals
  i = which(rownames(BS) == "Total Assets")   # for Assets
  j = which(rownames(BS) == "Total Liabilities") # for liabilities
  k = which(rownames(BS) == "Total Equity")  # for stockholder´s equity
  # for assets
  total_assets = do.call(rbind, lapply(as.list(1:i), function(ii){
    # convert row to percentage
    this_row <- t(scales::percent(as.numeric(BS[ii,]/BS[i,]), accuracy = 0.01))
    # convert to date frame
    this_row <- data.frame(this_row, row.names = rownames(BS)[ii])
    # add the column names
    colnames(this_row)= colnames(BS)
    # eliminate NA
    this_row[is.na(this_row)] <- scales::percent(0, accuracy = 0.01)
    # return formatted table
    this_row
  }))
  # for liabilities
  start_row = i+1
  total_liab = do.call(rbind, lapply(as.list(start_row:j), function(ii){
    # convert row to percentage
    this_row <- t(scales::percent(as.numeric(BS[ii,]/BS[j,]), accuracy = 0.01))
    # convert to data frame
    this_row <- data.frame(this_row, row.names = rownames(BS)[ii])
    # add the column names
    colnames(this_row)=colnames(BS)
    # eliminate NA
    this_row[is.na(this_row)] <- scales::percent(0, accuracy = 0.01)
    # return formatted table
    this_row
  }))
  # for stockholders equity
  start_row = j+1
  total_eqt = do.call(rbind, lapply(as.list(start_row:k), function(ii){
    # convert row to percentage
    this_row <- t(scales::percent(as.numeric(BS[ii,]/BS[k,]), accuracy = 0.01))
    # convert to data frame
    this_row <- data.frame(this_row, row.names = rownames(BS)[ii])
    # add the column names
    colnames(this_row) = colnames(BS)
    # eliminate NA
    this_row[is.na(this_row)] <- scales::percent(0, accuracy = 0.01)
    # return formatted table
    this_row
  }))
  # capture missing rows
  start_row = k+1
  misc_rows = BS[start_row:nrow(BS),]
  # combine
  pct_bs = rbind(total_assets, total_liab, total_eqt, misc_rows)
  View(pct_bs)
  save(pct_bs, file='~/pct_bs.rda')
  pct_bs2=matrix(nrow=nrow(pct_bs), ncol=ncol(pct_bs)+1)
  pct_bs2[,1]=rownames(pct_bs)
  pct_bs2[,2:ncol(pct_bs2)]=as.matrix(pct_bs)
  nomes = c('Account',colnames(pct_bs))
  colnames(pct_bs2)=nomes
    nome = paste('~/Percentage_',symbol,'_Balance_Sheet_Statment.xlsx', sep='')
  write_xlsx(as.data.frame(pct_bs2), nome)
  x =   DT:: datatable(pct_bs, rownames=TRUE, extensions="Buttons", options = list(dom="Blfrtip",
                                                                                   buttons = c('csv', 'excel', 'pdf')))
  htmlwidgets::saveWidget(x, "~/pct_bs.html")

    DT:: datatable(pct_bs, rownames=TRUE, extensions="Buttons", options = list(dom="Blfrtip",
                                              buttons = c('csv', 'excel', 'pdf')))

}
