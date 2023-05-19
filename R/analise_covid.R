#Funções
soma <- function(x) {sum(x,na.rm = T)}
###Tirado do app harvardanalytics corona
estimaR <- function(incid,coldata = ufs_m_movel_e_tx$date) {
  x <- data.frame("dates" = coldata,"I" = incid)
  estimate_R(x, method = "parametric_si", config = make_config(list(mean_si = 4.8, std_si = 2.3)))$R$`Mean(R)`
}

estima_conf_sup <- function(incid,coldata = ufs_m_movel_e_tx$date) {
  x <- data.frame("dates" = coldata,"I" = incid)
  estimate_R(x, method = "parametric_si", config = make_config(list(mean_si = 4.8, std_si = 2.3)))$R$`Quantile.0.95(R)`
}

plota_epids <- function(incid,coldata = ufs_m_movel_e_tx$date) {
  x <- data.frame("dates" = coldata,"I" = incid)
  estimate_R_plots(estimate_R(x, method = "parametric_si", config = make_config(list(mean_si = 4.8, std_si = 2.3))))
}


#Dados gerais
dados_covid <- read_csv("https://covid.ourworldindata.org/data/owid-covid-data.csv")
dados_covid$location <- countrycode(dados_covid$location,origin = "country.name.en",destination="cldr.short.pt")
##Europa 
#rworldmap
paises_europa <- unlist(countryExData%>%dplyr::filter(EPI_regions == "Europe")%>%select(ISO3V10))


##confirmados

covid_eu_c <- dados_covid%>%dplyr::filter(iso_code %in% c(paises_europa,"OWID_EUR"))

#Países da europa e suas populações
eur_pops <- unique(covid_eu_c%>%dplyr::filter(iso_code != "OWID_EUR")%>%select(iso_code, location,population)%>%arrange(desc(population)))

eur_pops$props_pos <- prop.table(eur_pops$population)

eur_pops_rel <- (eur_pops%>%filter(props_pos>0.03))$iso_code

curva_europa <- covid_eu_c%>%filter(iso_code == "OWID_EUR")%>%select(date,new_cases,new_cases_smoothed)

curva_europa$total_cases <- cumsum(curva_europa$new_cases)

retant <- 939+70

ceur_ts <- ts(curva_europa$new_cases, start=c(2020,23),frequency=365)

datas_anteriores <- breakpoints(ceur_ts~time(ceur_ts))

data_2022 <- breakpoints(ceur_ts[-(1:648)]~time(ceur_ts[-(1:648)]))


#Com método novo
data_nova_onda <- backCUSUM::breakpoint.est(ceur_ts[-(1:retant)]~1)+retant

data_nova_onda_ant <- backCUSUM::breakpoint.est(ceur_ts[(retant-70):retant]~1)+retant-70

data_primeira_met_novo <- curva_europa[backCUSUM::breakpoint.est(ceur_ts[1:648]~1),]$date

data_segunda_met_novo <- curva_europa[backCUSUM::breakpoint.est(ceur_ts[342:length(ceur_ts)]~1),]$date+341


data_terceira_met_novo <- curva_europa[backCUSUM::breakpoint.est(ceur_ts[800:969]~1),]$date+799


#Plotado

onda_europa_g <- ggplot(curva_europa,aes(date,new_cases_smoothed))+geom_line()+
  geom_vline(xintercept = curva_europa[data_nova_onda_ant,]$date,
             linetype = 4,colour = "purple", size = 1.5)+
  geom_vline(xintercept = curva_europa[data_nova_onda,]$date,
             linetype = 4,colour = "indianred", size = 1.5)+
  geom_vline(xintercept = data_primeira_met_novo,
             linetype = 4,colour = "green", size = 2)+
  geom_vline(xintercept = data_segunda_met_novo,
             linetype = 4,colour = "green",size=2)+
  geom_vline(xintercept = data_terceira_met_novo,
             linetype = 4,colour = "green",size=2)+
  geom_vline(xintercept = curva_europa[datas_anteriores$breakpoints[c(1,3)],]$date, linetype = 3 )+
  geom_vline(xintercept = curva_europa[data_2022$breakpoints[3]+678,]$date,colour = "orange")+
  theme_minimal()+theme(panel.grid.minor = element_blank(),axis.text.x = element_text(angle=30))+
  scale_y_continuous(labels = scales::number_format(big.mark = ".",decimal.mark = ","))+
  scale_x_date(date_breaks = "3 months", labels = scales::date_format("%b/%Y"))+
  ylab("Novos casos de infecção por SARS-COV2")+xlab("Período")

saveRDS(onda_europa_g,"resultados/g0_dados_europa.rds")  
             

tot_eur_pgraf <- ggplot(covid_eu_c%>%dplyr::filter(iso_code %in% eur_pops_rel),aes(date,total_cases,col=location))+
  geom_line()+theme_minimal()+
  theme(panel.grid.minor = element_blank(),axis.text.x = element_text(angle=30))+
  scale_y_continuous(labels = scales::number_format(big.mark = ".",decimal.mark = ","))+
  scale_x_date(date_breaks = "3 months", labels = scales::date_format("%b/%Y"))+
  ylab("Total de Casos")+xlab("Período")


saveRDS(tot_eur_pgraf,"resultados/g2_dados_paises_europa.rds")


medsem <- function(x){
  frollmean(x,n=7,align="right",na.rm=T)
}


novos_casos_eur <- dados_covid%>%
  filter(iso_code %in% eur_pops_rel)%>%select(date,location,new_cases_smoothed_per_million)%>%
  group_by(location,date)



hos_mort <- dados_covid%>%
  filter(iso_code %in% eur_pops_rel)%>%select(date,location,hosp_patients,icu_patients_per_million,new_deaths_smoothed_per_million)%>%
  group_by(location,date)


mortesgr <- ggplot(hos_mort%>%filter(year(date)>2020),aes(date,new_deaths_smoothed_per_million,col=location))+geom_line()+
  theme_minimal()+theme(panel.grid.minor = element_blank(),axis.text.x = element_text(angle=30))+
  scale_y_continuous(labels = scales::number_format(big.mark = ".",decimal.mark = ","))+
  scale_x_date(date_breaks = "3 months", labels = scales::date_format("%b/%Y"))+
  ylab("Novos óbitos por COVID-19 por milhão de hab.")+xlab("Periodo")


uti_covids <- ggplot(hos_mort%>%filter(year(date)>2020),aes(date,icu_patients_per_million,col=location))+geom_line()+
  theme_minimal()+theme(panel.grid.minor = element_blank(),axis.text.x = element_text(angle=30))+
  scale_y_continuous(labels = scales::number_format(big.mark = ".",decimal.mark = ","))+
  scale_x_date(date_breaks = "3 months", labels = scales::date_format("%b/%Y"))+
  ylab("Internados UTI por COVID-19 por milhão de hab.")+xlab("Periodo")


ncgr <- ggplot(novos_casos_eur%>%filter(year(date)>2020),aes(date,new_cases_smoothed_per_million,col=location))+geom_line()+
  theme_minimal()+theme(panel.grid.minor = element_blank(),axis.text.x = element_text(angle=30))+
  scale_y_continuous(labels = scales::number_format(big.mark = ".",decimal.mark = ","))+
  scale_x_date(date_breaks = "3 months", labels = scales::date_format("%b/%Y"))+
  ylab("Novos casos por COVID-19 por milhão de hab.")+xlab("Periodo")


saveRDS(mortesgr,"resultados/mortes_paises_eur.rds")

saveRDS(uti_covids,"resultados/uti_eur.rds")



###Brasil



#dados_covid_br <- covid19br::downloadCovid19(level = "states")

covid_br <- readRDS(url("https://github.com/dest-ufmg/covid19repo/blob/master/data/brazil.rds?raw=true",method="wininet"))


##Geral Brasil

###Ver as ondas do brasil
#Com método novo
br_novosts <-  ts(covid_br$newCases, start=c(2020,56),frequency=365)
ondasbr <- backCUSUM::breakpoint.est(br_novosts[-(1:(retant-31))]~1)+(retant-31)
# 
primonda <- covid_br[backCUSUM::breakpoint.est(br_novosts[1:648]~1),]$date
# 
segondabr <- covid_br[backCUSUM::breakpoint.est(br_novosts[342:770]~1),]$date+341
# 
# 
terondabr <- covid_br[backCUSUM::breakpoint.est(br_novosts[800:(retant-121)]~1),]$date+799


brgraftot <- ggplot(covid_br,aes(date,accumCases))+
  geom_line()+theme_minimal()+
  theme(panel.grid.minor = element_blank(),axis.text.x = element_text(angle=30))+
  scale_y_continuous(labels = scales::number_format(big.mark = ".",decimal.mark = ","))+
  scale_x_date(date_breaks = "3 months", labels = scales::date_format("%b/%Y"))+
  ylab("Total de Casos")+xlab("Período")


covid_br%<>%mutate(media_movel_semanal_novos = frollmean(newCases,6,align="right",na.rm=T),
                   media_movel_semanal_mortes_novas = frollmean(newDeaths,6,align="right",na.rm=T))

brgrafnovos <- ggplot(covid_br,aes(date,media_movel_semanal_novos))+
  geom_line(size = 2)+theme_minimal()+
  geom_vline(xintercept = primonda,
             linetype = 3,colour = paleta5[5], size = 1.5)+
  geom_vline(xintercept = segondabr,
             linetype = 3,colour = paleta5[5], size = 1.5)+
  geom_vline(xintercept = terondabr,
             linetype = 3,colour = paleta5[5],size=1.5)+
  geom_vline(xintercept = covid_br[ondasbr,]$date,
             linetype = 3,colour = paleta5[5],size=2)+
  theme(panel.grid.minor = element_blank(),axis.text.x = element_text(angle=30))+
  scale_y_continuous(labels = scales::number_format(big.mark = ".",decimal.mark = ","))+
  scale_x_date(date_breaks = "3 months", labels = scales::date_format("%b/%Y"))+
  ylab("Novos Casos")+xlab("Periodo")







saveRDS(brgrafnovos,"resultados/grafbr_novos.rds")

#Mortes
brgrafnovos_mrt <- ggplot(covid_br,aes(date,media_movel_semanal_mortes_novas))+
  geom_line(size = 2)+theme_minimal()+
  geom_vline(xintercept = primonda,
             linetype = 3,colour = paleta5[5], size = 1.5)+
  geom_vline(xintercept = segondabr,
             linetype = 3,colour = paleta5[5], size = 1.5)+
  geom_vline(xintercept = terondabr,
             linetype = 3,colour = paleta5[5],size=1.5)+
  geom_vline(xintercept = covid_br[ondasbr,]$date,
             linetype = 3,colour = paleta5[5],size=2)+
  theme(panel.grid.minor = element_blank(),axis.text.x = element_text(angle=30))+
  scale_y_continuous(labels = scales::number_format(big.mark = ".",decimal.mark = ","))+
  scale_x_date(date_breaks = "3 months", labels = scales::date_format("%b/%Y"))+
  ylab("Novos óbitos")+xlab("Periodo")


saveRDS(brgrafnovos_mrt,"resultados/grafbrmrts.rds")


# ###https://opendatasus.saude.gov.br/dataset/registro-de-ocupacao-hospitalar-covid-19
# #linkopentohcovid <- "https://s3.sa-east-1.amazonaws.com/ckan.saude.gov.br/LEITOS/2022-12-31/esus-vepi.LeitoOcupacao_2022.csv"
# tohc19 <- "dadosrefs/toh_c19.csv"
# #download.file(linkopentohcovid,tohc19)
# 
# tohc19 <- read_csv(tohc19)
# 
# tochc19_ind <- tohc19%>%
#   group_by(lubridate::week(dataNotificacao),ocupacaoHospitalarUti>65)%>%
#   summarize(tohmedia = median(ocupacaoHospitalarUti,na.rm=T),ntohalta = n())
# 
# tochc19_ind%<>%pivot_wider(names_from = `ocupacaoHospitalarUti > 65`,values_from = c(tohmedia,ntohalta))
# 
# tochc19_ind[[1]] <- Sys.Date()-373+7*tochc19_ind[[1]]
# 
# names(tochc19_ind) <- c("data","tohmedia_baixa_oc","tohmedia_alta_oc","ntoh_baixa_ocupacao","ntoh_alta_ocupacao")
# 
# tochc19_ind$propalta <- tochc19_ind$ntoh_alta_ocupacao/(tochc19_ind$ntoh_alta_ocupacao+tochc19_ind$ntoh_baixa_ocupacao)
# 
# ggplot(tochc19_ind,aes(x=data,y=tohmedia_alta_oc))+
#   geom_line()+
#   theme_minimal()
##Nível Estadual

covid_br_UF <- readRDS(url("https://github.com/dest-ufmg/covid19repo/blob/master/data/states.rds?raw=true",method="wininet"))

covid_br_UF%<>%group_by(state)%>%mutate(soma_movel_semanal = frollsum(newCases,7,align="right"),na.rm=T)
##Geral Estadual

brufgrafnovos <- ggplot(covid_br_UF,aes(date,soma_movel_semanal,col=state))+
  geom_line()+theme_minimal()+
  theme(panel.grid.minor = element_blank(),axis.text.x = element_text(angle=30))+
  scale_y_continuous(labels = scales::number_format(big.mark = ".",decimal.mark = ","))+
  scale_x_date(date_breaks = "3 months", labels = scales::date_format("%b/%Y"))+
  ylab("Média móvel semanal de novos casos")+xlab("Período")







br_pops <- unique(covid_br_UF%>%select(state, pop)%>%arrange(desc(pop)))

br_pops$props_pos <- prop.table(br_pops$pop)

br_pops_rel <- (br_pops%>%filter(props_pos>0.03))$state


covid_br_UF$state <- factor(covid_br_UF$state, levels = br_pops$state)
###UFs populosas


brufgraftot <- ggplot(covid_br_UF%>%filter(state %in% br_pops_rel),aes(date,accumCases,col = state))+
  geom_line()+theme_minimal()+
  theme(panel.grid.minor = element_blank(),axis.text.x = element_text(angle=30))+
  scale_y_continuous(labels = scales::number_format(big.mark = ".",decimal.mark = ","))+
  scale_x_date(date_breaks = "3 months", labels = scales::date_format("%b/%Y"))+
  ylab("Total de Casos")+xlab("Periodo")

saveRDS(brufgraftot,"resultados/grafbr_uf_tots.rds")

{##Adicionar últimos valores DF
# df4ults <- as.data.frame(t(replicate(4,unlist(last(covid_br_UF)))))
# 
# df4ults$date <- as.Date(c("2022-10-15","2022-10-16","2022-10-17","2022-10-18"))
# df4ults$newCases <- c(0,0,93,2142)                        
# df4ults$accumCases <- 839752+cumsum(df4ults$newCases) 
# 
# df4ults%<>%mutate(across(-1:-3,as.numeric))
# df4ults$epi_week <- df4ults$epi_week+1
# 
# covid_br_UF%<>%bind_rows(df4ults)

##Adicionar últimos valores DF
}

covid_br_UF%<>%group_by(state)%>%mutate(soma_movel_semanal = frollsum(newCases,7,align="right"),
                      media_movel_semanal = round(frollmean(newCases,7,align="right",na.rm=T),0))%>%ungroup()



ufs_m_movel_e_tx <- covid_br_UF %>%select(date,state,media_movel_semanal)%>%
  pivot_wider(names_from=state,values_from = media_movel_semanal)

ufs_m_movel_e_tx[5:6,-1] <- 0

ufs_m_movel_e_tx%<>%filter(date>as.Date("2020-02-28"))

ufs_m_movel_e_tx[ufs_m_movel_e_tx<0] <- 0


ufs_m_movel_e_tx[is.na(ufs_m_movel_e_tx)] <- 0

ufs_m_movel_e_tx%<>%arrange(date)

tx_transmissao <- apply(ufs_m_movel_e_tx[-1],2,estimaR)

tx_transmissao <- cbind(ufs_m_movel_e_tx[-1:-7,"date"],tx_transmissao)%>%pivot_longer(-date,names_to="uf",values_to="tx_trans")


tx_tr_confs <- apply(ufs_m_movel_e_tx[-1],2,estima_conf_sup)

tx_tr_confs <- cbind(ufs_m_movel_e_tx[-1:-7,"date"],tx_tr_confs)%>%pivot_longer(-date,names_to="uf",values_to="tx_trans_confs")


tx_transmissao <- left_join(tx_transmissao,tx_tr_confs)

tx_transmissao$tx_trans <- sapply(tx_transmissao$tx_trans,min,12)


covidbr_r <- 
  covid_br%>%
  mutate(newCases = ifelse(newCases<0,0,newCases))

covidbr_r <- data.frame(date = covidbr_r$date[-1:-7],uf = ".BRASIL", tx_trans = estimaR(covidbr_r$newCases,covidbr_r$date),
                        tx_trans_confs = estima_conf_sup(covidbr_r$newCases,covidbr_r$date))

tx_transmissao%<>%bind_rows(covidbr_r)


#tx_transmissao$tx_trans_confs <- sapply(tx_transmissao$tx_trans_confs,min,12.5)

tx_transmissao%<>%filter(uf!=20)
#tx_transmissao %<>%pivot_longer(-1:-2,names_to="taxa",values_to="valor")

tx_60 <- tx_transmissao%>%filter(date>(Sys.Date()-61))

graf_tx_transm <- ggplot(tx_transmissao,aes(date,tx_trans,col=uf))+
  geom_ribbon(aes(ymin=tx_trans,ymax=tx_trans_confs),fill = "lightblue",colour = "white",alpha=0.6)+
  geom_line(stat = "smooth")+
  scale_y_continuous(limits=c(0,4))+
  theme_minimal()+
  facet_wrap(vars(uf))

saveRDS(graf_tx_transm,"resultados/taxas_transmissao_ufs.rds")


ufporsemana <- covid_br_UF%>%group_by(year(date),epi_week,state)%>%
  summarize(across(newCases:newFollowup,soma),across(c(region,state_code),first),date=last(date))%>%
  arrange(date)


 ufnovossemana <- ufporsemana%>%ungroup()%>%select(date,state,newCases)%>%
   pivot_wider(names_from=state,values_from=newCases)#%>%

 # Intuitivamente, Rt sendo novos casos semana x / novos casos semana x-1
 ## Muito rudimentar, encontrada  versão do PAHU Harvard Analytics
 ## a partir de boletim do DF  #   mutate(across(-(1:3), ~( ./  dplyr::lag(.))))
 
 print(br_pops%>%left_join(tx_transmissao[tx_transmissao$date == last(tx_transmissao$date),c("uf","tx_trans")], by = c("state" = "uf")),n=28)
