library(tidyverse)
library(janitor)
library(stringr)
library(lubridate)
library(dplyr)


read_csv("INDIA-AQI-DATA-2015-2020.csv") -> adf
adf
adf %>% 
  View()
clean_names(adf)->adf2

# air quality trends for ahmedabad
adf2 %>% 
  mutate(year=date %>% year(),
         month=date %>% month(),
         day=date%>%day(),
         week=date %>% week(),
         weekday=date %>% wday(label=T))->adf3
colnames(adf3)
unique(adf3$city)

adf3 %>% 
  pivot_longer(3:15,names_to = "parameter", values_to = "values") -> adf4


#yearwise pollutant trend
adf4 %>% 
  group_by(year,parameter) %>% 
  summarise(mean_value=mean(values,na.rm=T))->adf5
adf5

library(ggplot2)

#aqi trend form 2015-2020
  ggplot(adf5,aes(x=year,y=mean_value))+
  geom_line()+
  facet_wrap(.~parameter,scale="free")+
    labs(title = "AQI Trend",subtitle = "from 2015-2020",
         caption ="Source: AQI data",x="year",y="mean_values")+
  theme_linedraw()-> plot1
  
  #to save ahmedabad in pdf format
  ggsave("air pollutant trend.pdf",
         plot=plot1,
         units = "in",
         width = 10,
         height=6)
 
#heat map
  
  adf4%>%
    filter(parameter=="co")%>%
    group_by(week,weekday,month)%>%
    summarise(meanval=mean(values,na.rm=T))%>%
    ggplot(aes(x=week,y=weekday,
               fill = meanval)) +
    geom_tile() +
    facet_wrap(~month, scales = "free_x") +
  
    # scale_fill_gradient(low = "yellow", high = "red") +
    scale_fill_gradientn(colours = c("darkgreen", "yellow", "red")) +
    theme_minimal() +
    labs(title = "CO heat map",
         subtitle = "For all cities",
         x = NULL,
         y=NULL)
  