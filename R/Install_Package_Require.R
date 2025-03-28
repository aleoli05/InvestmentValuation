#' Install_Package_Require
#' Install all package required
#' @export
Install_Package_Require <-function(){
  install.packages("DT")
  install.packages('highcharter')
  install.packages('webshot')
  install.packages('scales')
  install.packages('IntroCompFinR')
  webshot::install_phantomjs()
}
