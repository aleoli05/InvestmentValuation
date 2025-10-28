# Index_Bovespa_Brazil
#' Index_Bovespa_Brazil
#'@description
#' Import Ibovespa Composition from B3 and investing.com in the last stock exchange floor.
#'@param () Without parameters

#' @examples
#' Index_Bovespa_Brazil()

#' @export

Index_Bovespa_Brazil <-function(){
  # install.packages
  library(rvest)
  library(dplyr)
  library(stringr)
  library(writexl)
# URL from investing.com
url <- "https://www.investing.com/indices/bovespa-components"
# read page HTML
pagina <- read_html(url)

tabela_ibovespa <- pagina %>%
  html_nodes('.mb-6') %>% html_table(fill=TRUE)

Ibovespa<-as.data.frame((tabela_ibovespa)[[4]])
Ibovespa<-Ibovespa[2:ncol(Ibovespa)]

################################################################################
################################################################################
# URL from B3
url <- "https://sistemaswebb3-listados.b3.com.br/indexPage/day/IBOV?language=pt-br"

# read page HTML
pagina <- read_html(url)
tabela_ibov <- pagina %>%
  minimal_html('<thead><tr><th>Código</th><th>Ação</th><th>Tipo</th><th>Qtde. Teórica</th><th>Part. (%)</th></tr></thead>')
t=tabela_ibov|> html_element("thead") |> html_table()
t2=as.data.frame(t)


tabela_ibov2 <- pagina %>%
  minimal_html('<tbody><tr><td>ALOS3</td><td>ALLOS</td><td>ON  EJ
               NM</td><td class="text-right">466.632.333</td><td class="text-right">0,521</td></tr><tr><td>ABEV3</td><td>AMBEV S/A</td><td>ON</td><td class="text-right">4.273.841.357</td><td class="text-right">2,358</td></tr><tr><td>ASAI3</td><td>ASSAI</td><td>ON
               NM</td><td class="text-right">1.344.470.815</td><td class="text-right">0,504</td></tr><tr><td>AURE3</td><td>AUREN</td><td>ON
               NM</td><td class="text-right">318.338.365</td><td class="text-right">0,166</td></tr><tr><td>AZZA3</td><td>AZZAS 2154</td><td>ON
               NM</td><td class="text-right">134.911.449</td><td class="text-right">0,172</td></tr><tr><td>B3SA3</td><td>B3</td><td>ON
               NM</td><td class="text-right">5.196.371.458</td><td class="text-right">2,959</td></tr><tr><td>BBSE3</td><td>BBSEGURIDADE</td><td>ON
               NM</td><td class="text-right">616.186.808</td><td class="text-right">0,901</td></tr><tr><td>BBDC3</td><td>BRADESCO</td><td>ON
               N1</td><td class="text-right">1.467.570.789</td><td class="text-right">1,035</td></tr><tr><td>BBDC4</td><td>BRADESCO</td><td>PN
               N1</td><td class="text-right">5.106.488.516</td><td class="text-right">4,206</td></tr><tr><td>BRAP4</td><td>BRADESPAR</td><td>PN
               N1</td><td class="text-right">250.973.136</td><td class="text-right">0,202</td></tr><tr><td>BBAS3</td><td>BRASIL</td><td>ON
               NM</td><td class="text-right">2.842.496.541</td><td class="text-right">2,657</td></tr><tr><td>BRKM5</td><td>BRASKEM</td><td>PNA
               N1</td><td class="text-right">265.876.767</td><td class="text-right">0,086</td></tr><tr><td>BRAV3</td><td>BRAVA</td><td>ON
               NM</td><td class="text-right">461.715.081</td><td class="text-right">0,304</td></tr><tr><td>BPAC11</td><td>BTGP BANCO</td><td>UNT
               N2</td><td class="text-right">1.404.794.900</td><td class="text-right">3,049</td></tr><tr><td>CXSE3</td><td>CAIXA SEGURI</td><td>ON
               NM</td><td class="text-right">600.000.000</td><td class="text-right">0,401</td></tr><tr><td>CEAB3</td><td>CEA MODAS</td><td>ON
               NM</td><td class="text-right">138.420.009</td><td class="text-right">0,105</td></tr><tr><td>CMIG4</td><td>CEMIG</td><td>PN
               N1</td><td class="text-right">1.904.057.894</td><td class="text-right">0,929</td></tr><tr><td>COGN3</td><td>COGNA ON</td><td>ON
               NM</td><td class="text-right">1.810.877.718</td><td class="text-right">0,287</td></tr><tr><td>CPLE6</td><td>COPEL</td><td>PNB
               N2</td><td class="text-right">1.671.982.390</td><td class="text-right">1,039</td></tr><tr><td>CSAN3</td><td>COSAN</td><td>ON
               NM</td><td class="text-right">1.148.010.756</td><td class="text-right">0,314</td></tr><tr><td>CPFE3</td><td>CPFL ENERGIA</td><td>ON
               NM</td><td class="text-right">187.732.538</td><td class="text-right">0,349</td></tr><tr><td>CMIN3</td><td>CSNMINERACAO</td><td>ON
               N2</td><td class="text-right">1.646.519.336</td><td class="text-right">0,436</td></tr><tr><td>CURY3</td><td>CURY S/A</td><td>ON
               NM</td><td class="text-right">135.117.425</td><td class="text-right">0,207</td></tr><tr><td>CVCB3</td><td>CVC BRASIL</td><td>ON
               NM</td><td class="text-right">450.059.727</td><td class="text-right">0,037</td></tr><tr><td>CYRE3</td><td>CYRELA REALT</td><td>ON
               NM</td><td class="text-right">256.055.995</td><td class="text-right">0,351</td></tr><tr><td>DIRR3</td><td>DIRECIONAL</td><td>ON
               NM</td><td class="text-right">325.640.184</td><td class="text-right">0,240</td></tr><tr><td>ELET3</td><td>ELETROBRAS</td><td>ON
               N1</td><td class="text-right">1.821.923.647</td><td class="text-right">4,494</td></tr><tr><td>ELET6</td><td>ELETROBRAS</td><td>PNB
               N1</td><td class="text-right">270.513.025</td><td class="text-right">0,707</td></tr><tr><td>EMBR3</td><td>EMBRAER</td><td>ON
               NM</td><td class="text-right">733.564.938</td><td class="text-right">2,887</td></tr><tr><td>ENGI11</td><td>ENERGISA</td><td>UNT
               N2</td><td class="text-right">325.503.046</td><td class="text-right">0,771</td></tr><tr><td>ENEV3</td><td>ENEVA</td><td>ON
               NM</td><td class="text-right">1.912.620.486</td><td class="text-right">1,517</td></tr><tr><td>EGIE3</td><td>ENGIE BRASIL</td><td>ON
               NM</td><td class="text-right">255.215.967</td><td class="text-right">0,457</td></tr><tr><td>EQTL3</td><td>EQUATORIAL</td><td>ON
               NM</td><td class="text-right">1.246.479.596</td><td class="text-right">2,107</td></tr><tr><td>FLRY3</td><td>FLEURY</td><td>ON
               NM</td><td class="text-right">447.171.522</td><td class="text-right">0,294</td></tr><tr><td>GGBR4</td><td>GERDAU</td><td>PN
               N1</td><td class="text-right">1.276.025.309</td><td class="text-right">1,059</td></tr><tr><td>GOAU4</td><td>GERDAU MET</td><td>PN
               N1</td><td class="text-right">622.976.844</td><td class="text-right">0,298</td></tr><tr><td>HAPV3</td><td>HAPVIDA</td><td>ON
               NM</td><td class="text-right">311.257.700</td><td class="text-right">0,460</td></tr><tr><td>HYPE3</td><td>HYPERA</td><td>ON
               NM</td><td class="text-right">293.521.155</td><td class="text-right">0,317</td></tr><tr><td>IGTI11</td><td>IGUATEMI S.A</td><td>UNT
               N1</td><td class="text-right">211.401.696</td><td class="text-right">0,235</td></tr><tr><td>IRBR3</td><td>IRBBRASIL RE</td><td>ON
               NM</td><td class="text-right">81.838.787</td><td class="text-right">0,180</td></tr><tr><td>ISAE4</td><td>ISA ENERGIA</td><td>PN
               N1</td><td class="text-right">395.801.044</td><td class="text-right">0,442</td></tr><tr><td>ITSA4</td><td>ITAUSA</td><td>PN
               N1</td><td class="text-right">5.853.286.928</td><td class="text-right">3,008</td></tr><tr><td>ITUB4</td><td>ITAUUNIBANCO</td><td>PN
               N1</td><td class="text-right">4.611.334.219</td><td class="text-right">7,989</td></tr><tr><td>KLBN11</td><td>KLABIN S/A</td><td>UNT
               N2</td><td class="text-right">784.510.115</td><td class="text-right">0,642</td></tr><tr><td>RENT3</td><td>LOCALIZA</td><td>ON
               NM</td><td class="text-right">977.042.684</td><td class="text-right">1,725</td></tr><tr><td>LREN3</td><td>LOJAS RENNER</td><td>ON
               NM</td><td class="text-right">1.000.358.705</td><td class="text-right">0,674</td></tr><tr><td>MGLU3</td><td>MAGAZ LUIZA</td><td>ON
               NM</td><td class="text-right">355.414.674</td><td class="text-right">0,131</td></tr><tr><td>POMO4</td><td>MARCOPOLO</td><td>PN
               N2</td><td class="text-right">661.573.516</td><td class="text-right">0,262</td></tr><tr><td>MBRF3</td><td>MARFRIG</td><td>ON
               NM</td><td class="text-right">708.146.298</td><td class="text-right">0,485</td></tr><tr><td>BEEF3</td><td>MINERVA</td><td>ON
               NM</td><td class="text-right">433.648.983</td><td class="text-right">0,140</td></tr><tr><td>MOTV3</td><td>MOTIVA SA</td><td>ON
               NM</td><td class="text-right">988.559.510</td><td class="text-right">0,691</td></tr><tr><td>MRVE3</td><td>MRV</td><td>ON
               NM</td><td class="text-right">375.402.024</td><td class="text-right">0,122</td></tr><tr><td>MULT3</td><td>MULTIPLAN</td><td>ON
               N2</td><td class="text-right">314.022.221</td><td class="text-right">0,396</td></tr><tr><td>NATU3</td><td>NATURA</td><td>ON
               NM</td><td class="text-right">839.505.097</td><td class="text-right">0,334</td></tr><tr><td>PCAR3</td><td>P.ACUCAR-CBD</td><td>ON
               NM</td><td class="text-right">433.412.603</td><td class="text-right">0,070</td></tr><tr><td>PETR3</td><td>PETROBRAS</td><td>ON
               N2</td><td class="text-right">2.643.091.977</td><td class="text-right">3,823</td></tr><tr><td>PETR4</td><td>PETROBRAS</td><td>PN
               N2</td><td class="text-right">4.410.960.450</td><td class="text-right">5,996</td></tr><tr><td>RECV3</td><td>PETRORECSA</td><td>ON
               NM</td><td class="text-right">274.720.419</td><td class="text-right">0,156</td></tr><tr><td>PRIO3</td><td>PETRORIO</td><td>ON
               NM</td><td class="text-right">762.764.253</td><td class="text-right">1,287</td></tr><tr><td>PSSA3</td><td>PORTO SEGURO</td><td>ON
               NM</td><td class="text-right">181.016.454</td><td class="text-right">0,388</td></tr><tr><td>RADL3</td><td>RAIADROGASIL</td><td>ON
               NM</td><td class="text-right">1.285.713.898</td><td class="text-right">1,147</td></tr><tr><td>RAIZ4</td><td>RAIZEN</td><td>PN
               N2</td><td class="text-right">1.226.933.170</td><td class="text-right">0,054</td></tr><tr><td>RDOR3</td><td>REDE D OR</td><td>ON
               NM</td><td class="text-right">1.091.469.308</td><td class="text-right">2,101</td></tr><tr><td>RAIL3</td><td>RUMO S.A.</td><td>ON
               NM</td><td class="text-right">1.215.488.549</td><td class="text-right">0,852</td></tr><tr><td>SBSP3</td><td>SABESP</td><td>ON
               NM</td><td class="text-right">596.215.913</td><td class="text-right">3,615</td></tr><tr><td>SANB11</td><td>SANTANDER BR</td><td>UNT EJ</td><td class="text-right">360.347.199</td><td class="text-right">0,478</td></tr><tr><td>CSNA3</td><td>SID NACIONAL</td><td>ON</td><td class="text-right">727.975.012</td><td class="text-right">0,286</td></tr><tr><td>SLCE3</td><td>SLC AGRICOLA</td><td>ON
               NM</td><td class="text-right">197.693.014</td><td class="text-right">0,143</td></tr><tr><td>SMFT3</td><td>SMART FIT</td><td>ON
               NM</td><td class="text-right">427.078.008</td><td class="text-right">0,503</td></tr><tr><td>SUZB3</td><td>SUZANO S.A.</td><td>ON
               NM</td><td class="text-right">645.082.079</td><td class="text-right">1,443</td></tr><tr><td>TAEE11</td><td>TAESA</td><td>UNT
               N2</td><td class="text-right">218.568.234</td><td class="text-right">0,368</td></tr><tr><td>VIVT3</td><td>TELEF BRASIL</td><td>ON</td><td class="text-right">738.065.812</td><td class="text-right">1,157</td></tr><tr><td>TIMS3</td><td>TIM</td><td>ON
               NM</td><td class="text-right">804.628.136</td><td class="text-right">0,899</td></tr><tr><td>TOTS3</td><td>TOTVS</td><td>ON
               NM</td><td class="text-right">533.261.054</td><td class="text-right">1,029</td></tr><tr><td>UGPA3</td><td>ULTRAPAR</td><td>ON
               NM</td><td class="text-right">1.071.443.332</td><td class="text-right">1,034</td></tr><tr><td>USIM5</td><td>USIMINAS</td><td>PNA
               N1</td><td class="text-right">459.046.773</td><td class="text-right">0,103</td></tr><tr><td>VALE3</td><td>VALE</td><td>ON  ATZ
               NM</td><td class="text-right">4.267.772.018</td><td class="text-right">12,002</td></tr><tr><td>VAMO3</td><td>VAMOS</td><td>ON
               NM</td><td class="text-right">498.192.519</td><td class="text-right">0,076</td></tr><tr><td>VBBR3</td><td>VIBRA</td><td>ON
               NM</td><td class="text-right">1.112.563.218</td><td class="text-right">1,221</td></tr><tr><td>VIVA3</td><td>VIVARA S.A.</td><td>ON
               NM</td><td class="text-right">123.160.591</td><td class="text-right">0,172</td></tr><tr><td>WEGE3</td><td>WEG</td><td>ON
               NM</td><td class="text-right">1.485.954.732</td><td class="text-right">2,801</td></tr><tr><td>YDUQ3</td><td>YDUQS PART</td><td>ON
               NM</td><td class="text-right">260.753.144</td><td class="text-right">0,157</td></tr><!----></tbody>')
t3=tabela_ibov2|> html_element("tbody") |> html_table()
Ibov=as.data.frame(t3)
colnames(Ibov)=colnames(t2)
#View(Ibov)


################################################################################
Ibovesp<-as.data.frame(Ibovespa[order(Ibovespa[,1], decreasing=FALSE),])
Ibov_=Ibovesp[1:ncol(Ibovesp)]
#View(Ibov_)
Index_Bovespa = Ibov
Index_Bovespa[,6:13]=Ibov_
Index_Bovespa[,4]=as.numeric(str_remove_all(Index_Bovespa[,4],'[^0-9]'))
Index_Bovespa[,5]=as.numeric(gsub(',','.', Index_Bovespa[,5]))
View(Index_Bovespa)
print(Index_Bovespa)

Data=Sys.Date()
Ano=format(Data,'%Y')
nome_data=Index_Bovespa[1,13]
Mes = substr(nome_data, start=4, stop=5)
Dia = substr(nome_data, start=1, stop=2)
nome_date=paste(Ano,'-',Mes,'-',Dia, sep='')


if(file.exists("~/Ibov_Composition")==TRUE){
  nome_readme=paste('~/Ibov_Composition/Ibov_', Data,'.rda', sep='')
  save(Index_Bovespa, file=nome_readme)
  Readme_Inv_wrt = paste('~/Ibov_Composition/Ibov_', Data,'.xlsx', sep='')
  write_xlsx(Index_Bovespa, Readme_Inv_wrt)
}else{
  nome_dir_backup=paste("~/Ibov_Composition", sep="")
  dir.create(nome_dir_backup)
  nome_readme=paste('~/Ibov_Composition/Ibov_', Data, '.rda', sep='')
  save(Index_Bovespa, file=nome_readme)
  Readme_Inv_wrt = paste('~/Ibov_Composition/Ibov_', Data,'.xlsx', sep='')
  write_xlsx(Index_Bovespa, Readme_Inv_wrt)
}

}

