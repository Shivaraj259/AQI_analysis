#2. Air quality trends for bengaluru

adf2 %>% 
  mutate(year=date %>% year(),
         month=date %>% month(),
         day=date%>%day(),
         week=date %>% week(),
         weekday=date %>% wday(label=T))->adf3
colnames(adf3)
adf3%>%
  filter(city=="Bengaluru")  

adf3 %>% 
  pivot_longer(3:15,names_to = "parameter", values_to = "values") -> bdf1

#yearwise pollutant trend for bengaluru
bdf1 %>% 
  group_by(year,parameter) %>% 
  summarise(mean_value=mean(values,na.rm=T))->bdf2
bdf2


#aqi trend form 2015-2020 for bengaluru
ggplot(bdf2,aes(x=year,y=mean_value))+
  geom_line()+
  facet_wrap(.~parameter,scale="free")+
  labs(title = "AQI bengaluru Trend",subtitle = "from 2015-2020",
       caption ="Source: AQI data",x="year",y="mean_values")+
  theme_linedraw()-> plot2

#to save Bengaluru in pdf format
ggsave("Bengaluru air pollutant trend.pdf",
       plot=plot2,
       units = "in",
       width = 10,
       height=6)