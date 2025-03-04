#' table2plot
#' Plot Quarterly Values from the Financial
################################################################################
# ticker = 'TSLA'
# WHAT = 'Cash & Short Term Investments'

#' @param ticker = ticker symbol
#' @param WHAT = One of the row names from the financial table ex. 'Cash & Short Term Investments'
#' @param FROM = data generate with getFins Command
#' @examples
#' table2plot('TSLA','Cash & Short Term Investments',bs)
#' @export
table2plot = function(ticker, WHAT, FROM){
  require(webshot)
  load('~/nome.rda')
  nome2=paste('~/',nome,'.rda', sep='')
  load(nome2)
  # locate row
  dta <- subset(FROM, row.names(FROM)==WHAT)
  # reverse order (oldest to newest)
  dta <- rev(dta)
  # transpose
  dta = as.data.frame(t(dta))
  # as data.frame
  dta$date=row.names(dta)
  # data.frame
  dta = data.frame(date=format(as.Date(dta$date,"%m/%d/%Y"), "%b %d '%y"), value=dta[,1])
  # add Period to period growth
  dta$pct_change = ROC(dta$value, type = 'discrete') # pct = percentage
  dta$pct_change[is.na(dta$pct_change)]<-0
  # add YoY Growth
  dta$yoy_change = ROC(dta$value, n=4, type = 'discrete') # yoy = Year-on-year
  dta$yoy_change[is.na(dta$yoy_change)]<-0
  # plot using highcharter
  ##############################################################
  nome3 = paste('~/',ticker,'_',nome,'_Chart.png', sep='')
  nome4 = paste('~/',ticker,'_',nome,'_Chart.html', sep='')
  hc = dta%>%
     hchart(
      'column', hcaes(x='date',y='value'), name='value',
      stacking='normal' #, tooltip = list(pointFormat = "<b>{series.name}: {point.y: , f}")
    ) %>%
    hc_colorAxis(minColor='aquamarine',maxColor='darkgreen') %>%
    hc_title(text=paste0('$',ticker), align='center') %>%
    hc_subtitle(text=paste0(WHAT)) %>%
    hc_yAxis_multiples(list(title=list(text='value'), opposite = FALSE),
                       list(showLastLabel = FALSE, opposite = TRUE, title=list(text='% Change'),
                            min=-500, max=500)) %>%
    hc_add_series(round(dta$yoy_change*100,2), yAxis=1, name="YoY % Change",
                  tooltip = list(pointFormat = "<b>{series.name}: {point.y:,.2f}%")) %>%
    hc_add_series(round(dta$pct_change*100,2), yAxis=1, name="QoQ % Change",
                  tooltip = list(pointFormat = "<b>{series.name}: {point.y:,.2f}%"))
  # QoQ = Quarter-on-quarter
  htmlwidgets::saveWidget(widget = hc, file=nome4)
  nome5=paste('C:/Users/Usuario/Documents/',ticker,'_',nome,'_Chart.html', sep='')
  nome6 = paste('C:/Users/Usuario/Documents/',ticker,'_',nome,'_Chart.png', sep='')
  webshot::webshot(url=nome5,file = nome6, delay = 0.2)
  #########################################################

  dta%>%
    hchart(
      'column', hcaes(x='date',y='value'), name='value',
      stacking='normal' #, tooltip = list(pointFormat = "<b>{series.name}: {point.y: , f}")
    ) %>%
    hc_colorAxis(minColor='aquamarine',maxColor='darkgreen') %>%
    hc_title(text=paste0('$',ticker), align='center') %>%
    hc_subtitle(text=paste0(WHAT)) %>%
    hc_yAxis_multiples(list(title=list(text='value'), opposite = FALSE),
                       list(showLastLabel = FALSE, opposite = TRUE, title=list(text='% Change'),
                            min=-500, max=500)) %>%
    hc_add_series(round(dta$yoy_change*100,2), yAxis=1, name="YoY % Change",
                  tooltip = list(pointFormat = "<b>{series.name}: {point.y:,.2f}%")) %>%
    hc_add_series(round(dta$pct_change*100,2), yAxis=1, name="QoQ % Change",
                  tooltip = list(pointFormat = "<b>{series.name}: {point.y:,.2f}%"))
  # QoQ = Quarter-on-quarter
}
