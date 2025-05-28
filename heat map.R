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