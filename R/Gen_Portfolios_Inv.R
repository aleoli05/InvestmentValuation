#' Gen_Portfolios_Inv Generate investment portfolios specified
#' Generate portfolios selected, how Sharpe, Markowitz, ANNt, Magic Formula, Intelligent Investor
#' @param Portfolios Specified portfolios generated
#' @param N_Assets limit of asset numbers in the portfolio
#' @param Initial_Date_Testing Initial Date of Test Series
#' @param Final_Date_Testing Final Date Test Series, if '' is the system date
#' @param Rf Risk free rate
#' @param type_ANNt Select type ANNt:
#' "T1"= NNet_Signal_Traning;
#' "T2"= NNet_t_Training;
#' "T3"= MC_Signal_Training;
#' "T4"= MC_t_Training;
#' "T5"= NNet_Signal_Test;
#' "T6"= NNet_t_Test;
#' "T7"= MC_Signal_Test;
#' "T8"= Type_ANNt: MC_t_Test
#' @author Alexandre Silva de Oliveira
#' @examples
#' N_Assets <- 3
#' Initial_Date_Testing <- c('2023-01-03')
#' Final_Date_Testing <- c('')
#' Rf <- 0
#' type_ANNt <- 'T8'
#' # Generate assets portfolio (maximum N assets specified)
#' Gen_portfolios(3,'2023-01-03','',0,'T8')
#' @export
Gen_Portfolios_Inv <-function(){}
#Gen_Portfolios_Inv <-function(Portfolios, N_Assets, Initial_Date_Testing, Final_Date_Testing, Rf, type_ANNt){
#
#
#}
