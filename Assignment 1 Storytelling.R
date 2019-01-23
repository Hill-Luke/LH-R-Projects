library(dplyr)
library(ggplot2)
library(plotly)
library(sqldf)
library(readxl)

crime_rate<-read_excel("/Users/lukeofthehill/Desktop/R/Data/Criminal Justice Reform/crime_rate.xlsx") 
Federal_offense<-read_excel("/Users/lukeofthehill/Desktop/R/Data/Criminal Justice Reform/Federal_offense.xlsx")
State_offense<-read_excel("/Users/lukeofthehill/Desktop/R/Data/Criminal Justice Reform/State_offense.xlsx")
sentenced_prison_pop<-read_excel("/Users/lukeofthehill/Desktop/R/Data/Criminal Justice Reform/sentenced_prison_pop.xlsx")
sentenced_prison_pop<-read_excel("/Users/lukeofthehill/Desktop/R/Data/Criminal Justice Reform/sentenced_prison_pop.xlsx")

names(crime_rate)

crime_rate_graph<-crime_rate %>% ggplot(aes(x=Year, y=`Violent Crime rate`))+
  geom_line(stat="identity")
plot(crime_rate_graph)


crime_rate_prison_pop<-left_join(crime_rate, select(sentenced_prison_pop, c("Year", "US_total")), by="Year")
crime_rate_prison_pop<-subset(crime_rate_prison_pop, Year>=1978)
crime_rate_prison_pop$`US Total Prison Population`<-crime_rate_prison_pop$US_total
crime_rate_prison_pop$US_total<-NULL

crime_rate_pop_graph<-crime_rate_prison_pop %>% ggplot(aes(x=Year))+
 geom_line(stat="identity", aes(y=`US Total Prison Population`))
                                                                 
ggplotly(crime_rate_pop_graph)


