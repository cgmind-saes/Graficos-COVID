pacman::p_load(
  covid19.analytics, 
  writexl, 
  here,
  rio,
  openxlsx,
  dplyr,
  rJava, 
  devtools, 
  tidyverse 
)


covid <- read_csv("https://covid.ourworldindata.org/data/owid-covid-data.csv")


covid_euro <- covid[covid$location == "Europe",]

#novos casos de covid x novos obitos
prop <- (max(covid$new_cases_smoothed,na.rm = T)/max(covid$new_deaths_smoothed,na.rm = T))

#grafico de novos casos covid na europa
ggplot(covid_euro,aes(x=date,y=new_cases_smoothed))+geom_col()+theme_minimal()

covid_euro%<>%mutate(tx_aceleracao = round(100*(new_cases/lag(new_cases)-1),1),
                     tx_crescimento = round(100*(new_cases/lag(total_cases)),1))
#grafico tx de aceleração
ggplotly(ggplot(covid_euro,aes(x=date,y=tx_aceleracao))+
           geom_line(color= "purple")+theme_minimal())

