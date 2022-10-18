


#Dados gerais
dados_covid <- read_csv("https://covid.ourworldindata.org/data/owid-covid-data.csv")

##Europa 
#rworldmap
paises_europa <- unlist(countryExData%>%dplyr::filter(EPI_regions == "Europe")%>%select(ISO3V10))


##confirmados

covid_eu_c <- dados_covid%>%dplyr::filter(iso_code %in% c(paises_europa,"OWID_EUR"))




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
  ylab("Novos casos de infecção por COVID-19")+xlab("Período")

saveRDS(onda_europa_g,"resultados/g0_dados_europa.rds")  
             





























































###Brasil



dados_covid_br <- covid19br::downloadCovid19(level = "states")


## Agregação por UF - Geral e exportação do xlsx

br <- paste("T:/2022/Núcleo de Indicadores e Avaliação/Bases de Dados MS/COVID/COVID_paises/tabelas/TAXA_Covid-19_Brasil_UF_", 
            date ,".xlsx", sep = "")


Covid19Br <- filter(c19.data, c19.data$Country_Region == "Brazil")


Covid19Br <- aggregate(x = Covid19Br[c("Confirmed", "Deaths", "Recovered",
                                       "Active")],
                       by = Covid19Br[c("Province_State")], FUN = sum)

#view(Covid19Br)

names(Covid19)

Covid19 %>% 
  mutate(   death_per_million = Deaths / (pop / 1000000),
            lethality_rate = (Deaths / Confirmed) *100,
            mortality_rate = (Deaths / pop) *1000,
  ) -> Covid19Hoje

names(Covid19Hoje)


Covid19Hoje <- Covid19Hoje[,c(1,2,3,4,5, 7, 8, 9, 10, 6)]

Covid19Hoje <-  arrange(Covid19Hoje, desc(Confirmed))


Covid19Taxas <-  Covid19Hoje

names(Covid19Taxas) <- c("Country", "Total confirmed cases", "Total deaths", "Total recovered (*)", "Active", "Confirmed cases per million population", "Deaths per million population", "Lethality Rate", "Mortality Rate", "Population")

write_xlsx(head(Covid19Taxas, 30), pathPaisesXls)

Covid19Hoje
sum(Covid19Hoje$pop)
sum(Covid19Hoje$pop) / 1000000
TotalDeaths / (sum(Covid19Hoje$pop) / 1000000)

TotalConfirmedCases <-  sum(Covid19Hoje$Confirmed, na.rm = TRUE)
TotalDeaths <-  sum(Covid19Hoje$Deaths, na.rm = TRUE)
TotalRecovered <-  sum(Covid19Hoje$Recovered, na.rm = TRUE)
Active <- sum(Covid19Hoje$Active, na.rm = TRUE)
ConfirmedPerMillion <- TotalConfirmedCases / (sum(Covid19Hoje$pop) / 1000000)
DeathPerMillion <-  TotalDeaths / (sum(Covid19Hoje$pop) / 1000000)
LethalityRate <- (TotalDeaths / sum(Covid19Hoje$Confirmed)) *100
MortalityRate <-  (TotalDeaths / sum(Covid19Hoje$pop)) *1000
wPopulation <- sum(Covid19Hoje$pop)

Covid19Totais <-  NULL

Covid19Totais <- cbind(TotalConfirmedCases, TotalDeaths, TotalRecovered, Active, ConfirmedPerMillion, DeathPerMillion, LethalityRate, MortalityRate, wPopulation) 

Covid19Totais

write_xlsx(Covid19Br, br)


#View(Covid19Br)

#view(Covid19Taxas)




