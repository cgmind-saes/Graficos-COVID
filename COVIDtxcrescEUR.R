pacman::p_load(
  covid19.analytics, 
  writexl, 
  here,
  rio,
  openxlsx,
  dplyr,
  rJava, 
  devtools, 
  rnaturalearth,
  tidyverse,
  gganimate,
  magrittr,
  plotly,
  sf
)

covid <- read_csv("https://covid.ourworldindata.org/data/owid-covid-data.csv")



covid_euro <- covid[covid$location == "Europe",]
covid_fr <- covid[covid$location == "France",]
covid_gy <- covid[covid$location == "Germany",]
covid_uk <- covid[covid$location == "United Kingdom",]
covid_ita <- covid[covid$location == "Italy",]
covid_esp <- covid[covid$location == "Spain",]




#novos casos de covid x novos obitos
prop <- (max(covid$new_cases_smoothed,na.rm = T)/max(covid$new_deaths_smoothed,na.rm = T))

#grafico de novos casos covid na europa
ggplotly(ggplot(covid_euro,aes(x=date,y=new_cases_smoothed))+geom_col()+theme_minimal()) #europa
ggplotly(ggplot(covid_fr,aes(x=date,y=new_cases_smoothed))+geom_col()+theme_minimal()) #frança
ggplot(covid_gy,aes(x=date,y=new_cases_smoothed))+geom_col()+theme_minimal() #alemanha
ggplot(covid_uk,aes(x=date,y=new_cases_smoothed))+geom_col()+theme_minimal() #reino unido
ggplot(covid_ita,aes(x=date,y=new_cases_smoothed))+geom_col()+theme_minimal() #talia
ggplot(covid_esp,aes(x=date,y=new_cases_smoothed))+geom_col()+theme_minimal() #espanha


covid_euro%<>%mutate(tx_aceleracao = round(100*(new_cases/lag(new_cases)-1),1),
                     tx_crescimento = round(100*(new_cases/lag(total_cases)),1)) #Europa

covid_fr%<>%mutate(tx_aceleracao = round(100*(new_cases/lag(new_cases)-1),1),
                     tx_crescimento = round(100*(new_cases/lag(total_cases)),1)) #frança

covid_gy%<>%mutate(tx_aceleracao = round(100*(new_cases/lag(new_cases)-1),1),
                     tx_crescimento = round(100*(new_cases/lag(total_cases)),1)) #Alemanha

covid_uk%<>%mutate(tx_aceleracao = round(100*(new_cases/lag(new_cases)-1),1),
                     tx_crescimento = round(100*(new_cases/lag(total_cases)),1)) #reino unido

covid_ita%<>%mutate(tx_aceleracao = round(100*(new_cases/lag(new_cases)-1),1),
                     tx_crescimento = round(100*(new_cases/lag(total_cases)),1)) #talia

covid_esp%<>%mutate(tx_aceleracao = round(100*(new_cases/lag(new_cases)-1),1),
                     tx_crescimento = round(100*(new_cases/lag(total_cases)),1)) #espanha


#grafico tx de aceleração
ggplotly(ggplot(covid_euro,aes(x=date,y=tx_aceleracao))+
           geom_line(color= "purple")+theme_minimal())   #europa

ggplotly(ggplot(covid_fr,aes(x=date,y=tx_aceleracao))+
           geom_line(color= "purple")+theme_minimal()) #frança

ggplotly(ggplot(covid_gy,aes(x=date,y=tx_aceleracao))+
           geom_line(color= "purple")+theme_minimal()) #alemanha

ggplotly(ggplot(covid_uk,aes(x=date,y=tx_aceleracao))+
           geom_line(color= "purple")+theme_minimal()) #reino unido

ggplotly(ggplot(covid_ita,aes(x=date,y=tx_aceleracao))+
           geom_line(color= "purple")+theme_minimal()) #italia

ggplotly(ggplot(covid_esp,aes(x=date,y=tx_aceleracao))+
           geom_line(color= "purple")+theme_minimal()) #espanha

#salvar em xlsx para europa e paises
library(xlsx)
write.xlsx(covid_euro,file="covid_euro.xlsx")
write.xlsx(covid,file="covid.xlsx")
write.xlsx(covid_fr,file="covid_fr.xlsx")
write.xlsx(covid_esp,file="covid_esp.xlsx")
write.xlsx(covid_gy,file="covid_alemanha.xlsx")
write.xlsx(covid_ita,file="covid_italia.xlsx")
