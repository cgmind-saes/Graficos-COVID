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

retant <- 939

ceur_ts <- ts(curva_europa$new_cases, start=c(2020,23),frequency=365)

datas_anteriores <- breakpoints(ceur_ts~time(ceur_ts))

data_2022 <- breakpoints(ceur_ts[-(1:648)]~time(ceur_ts[-(1:648)]))


#Com método novo
data_nova_onda <- backCUSUM::breakpoint.est(ceur_ts[-(1:retant)]~1)+retant

data_primeira_met_novo <- curva_europa[backCUSUM::breakpoint.est(ceur_ts[1:648]~1),]$date

data_segunda_met_novo <- curva_europa[backCUSUM::breakpoint.est(ceur_ts[342:length(ceur_ts)]~1),]$date+341


data_terceira_met_novo <- curva_europa[backCUSUM::breakpoint.est(ceur_ts[800:969]~1),]$date+799


#Plotado

onda_europa_g <- ggplot(curva_europa,aes(date,new_cases_smoothed))+geom_line()+
  geom_vline(xintercept = curva_europa[data_nova_onda,]$date,
             linetype = 4,colour = "indianred", size = 1.5)+
  geom_vline(xintercept = data_primeira_met_novo,
             linetype = 4,colour = "green", size = 2)+
  geom_vline(xintercept = data_segunda_met_novo,
             linetype = 4,colour = "green",size=2)+
  geom_vline(xintercept = data_terceira_met_novo,
             linetype = 4,colour = "green",size=2)+
  geom_vline(xintercept = curva_europa[datas_anteriores$breakpoints[c(1,3)],]$date, linetype = 3 )+
  geom_vline(xintercept = curva_europa[datas_2022$breakpoints[3]+678,]$date,colour = "orange")+
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










###Brasil



#dados_covid_br <- covid19br::downloadCovid19(level = "states")

covid_br <- readRDS(url("https://github.com/dest-ufmg/covid19repo/blob/master/data/brazil.rds?raw=true",method="wininet"))


##Geral Brasil

brgraftot <- ggplot(covid_br,aes(date,accumCases))+
  geom_line()+theme_minimal()+
  theme(panel.grid.minor = element_blank(),axis.text.x = element_text(angle=30))+
  scale_y_continuous(labels = scales::number_format(big.mark = ".",decimal.mark = ","))+
  scale_x_date(date_breaks = "3 months", labels = scales::date_format("%b/%Y"))+
  ylab("Total de Casos")+xlab("Período")


brgrafnovos <- ggplot(covid_br,aes(date,newCases))+
  geom_line()+theme_minimal()+
  theme(panel.grid.minor = element_blank(),axis.text.x = element_text(angle=30))+
  scale_y_continuous(labels = scales::number_format(big.mark = ".",decimal.mark = ","))+
  scale_x_date(date_breaks = "3 months", labels = scales::date_format("%b/%Y"))+
  ylab("Novos Casos")+xlab("Período")




##Nível Estadual

covid_br_UF <- readRDS(url("https://github.com/dest-ufmg/covid19repo/blob/master/data/states.rds?raw=true",method="wininet"))

##Geral Estadual

brufgrafnovos <- ggplot(covid_br_UF,aes(date,soma_movel_semanal,col=state))+
  geom_line()+theme_minimal()+
  theme(panel.grid.minor = element_blank(),axis.text.x = element_text(angle=30))+
  scale_y_continuous(labels = scales::number_format(big.mark = ".",decimal.mark = ","))+
  scale_x_date(date_breaks = "3 months", labels = scales::date_format("%b/%Y"))+
  ylab("Média móvel semanal de novos casos")+xlab("Período")


brufgraftot <- ggplot(covid_br_UF,aes(date,accumCases,col = state))+
  geom_line()+theme_minimal()+
  theme(panel.grid.minor = element_blank(),axis.text.x = element_text(angle=30))+
  scale_y_continuous(labels = scales::number_format(big.mark = ".",decimal.mark = ","))+
  scale_x_date(date_breaks = "3 months", labels = scales::date_format("%b/%Y"))+
  ylab("Total de Casos")+xlab("Período")



{##Adicionar últimos valores DF
df4ults <- as.data.frame(t(replicate(4,unlist(last(covid_br_UF)))))

df4ults$date <- as.Date(c("2022-10-15","2022-10-16","2022-10-17","2022-10-18"))
df4ults$newCases <- c(0,0,93,2142)                        
df4ults$accumCases <- 839752+cumsum(df4ults$newCases) 

df4ults%<>%mutate(across(-1:-3,as.numeric))
df4ults$epi_week <- df4ults$epi_week+1

covid_br_UF%<>%bind_rows(df4ults)

##Adicionar últimos valores DF
}

covid_br_UF%<>%group_by(state)%>%mutate(soma_movel_semanal = frollsum(newCases,7,align="right"),
                      media_movel_semanal = round(frollmean(newCases,7,align="right",na.rm=T),0))%>%ungroup()



ufs_m_movel_e_tx <- covid_br_UF %>%select(date,state,media_movel_semanal)%>%
  pivot_wider(names_from=state,values_from = media_movel_semanal)

ufs_m_movel_e_tx[5:6,-1] <- 0

ufs_m_movel_e_tx%<>%filter(date>as.Date("2020-02-28"))

ufs_m_movel_e_tx[ufs_m_movel_e_tx<0] <- 0

tx_transmissao <- apply(ufs_m_movel_e_tx[-1],2,estimaR)

tx_transmissao <- cbind(ufs_m_movel_e_tx[-1:-7,"date"],tx_transmissao)%>%pivot_longer(-date,names_to="uf",values_to="tx_trans")


tx_tr_confs <- apply(ufs_m_movel_e_tx[-1],2,estima_conf_sup)

tx_tr_confs <- cbind(ufs_m_movel_e_tx[-1:-7,"date"],tx_tr_confs)%>%pivot_longer(-date,names_to="uf",values_to="tx_trans_confs")


tx_transmissao <- left_join(tx_transmissao,tx_tr_confs)

#tx_transmissao %<>%pivot_longer(-1:-2,names_to="taxa",values_to="valor")

ggplot(tx_transmissao%>%filter(date>(Sys.Date()-61)),aes(date,tx_trans,col=uf))+
  geom_ribbon(aes(ymin=tx_trans,ymax=tx_trans_confs),fill = "lightblue",colour = "white",alpha=0.6)+
  geom_line()+
  theme_minimal()+
  facet_wrap(vars(uf))



ufporsemana <- covid_br_UF%>%group_by(year(date),epi_week,state)%>%
  summarize(across(newCases:newFollowup,soma),across(c(region,state_code),first),date=last(date))%>%
  arrange(date)


 ufnovossemana <- ufporsemana%>%ungroup()%>%select(date,state,newCases)%>%
   pivot_wider(names_from=state,values_from=newCases)#%>%

 # Intuitivamente, Rt sendo novos casos semana x / novos casos semana x-1
 ## Muito rudimentar, encontrada  versão do PAHU Harvard Analytics
 ## a partir de boletim do DF  #   mutate(across(-(1:3), ~( ./  dplyr::lag(.))))


