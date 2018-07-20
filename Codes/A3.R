library(ggplot2)

airline_safety = read_csv("Downloads/R/DA_Project/Data/airline_safety.csv") %>% 
  mutate(death_ask_85_99 = (fatalities_85_99/avail_seat_km_per_week)*10^6,
         death_ask_00_14 = (fatalities_00_14/avail_seat_km_per_week)*10^6)

ggplot(airline_safety, aes(x = death_ask_85_99, y = death_ask_00_14)) +
  geom_point() + 
  geom_text(aes(label=ifelse(death_ask_00_14>0.4 | death_ask_85_99>0.4,as.character(airline),'')),
            hjust=-0.1,
            vjust=-0.1,
            angle=10,
            size=3) +
  scale_x_continuous(name="Death in 1985 - 1999 ASK", limits=c(0, 1.2)) +
  scale_y_continuous(name="Death in 2000 - 2014 ASK", limits=c(0, 1)) + 
  geom_smooth(method = lm, se = FALSE) + 
  ggtitle("Fatalities of Companies")
  
cor.test(airline_safety$fatalities_85_99, airline_safety$fatalities_00_14)

ggplot(airline_safety, aes(x = incidents_85_99, y = incidents_00_14)) +
  geom_point() + 
  geom_text(aes(label=ifelse(incidents_00_14>10 | incidents_85_99>20,as.character(airline),'')),
            hjust=0,
            vjust=0,
            angle=10,
            size=3) +
  scale_x_continuous(name="Incidents in 1985 - 1999 ASK") +
  scale_y_continuous(name="Incidents in 2000 - 2014 ASK") + 
  geom_smooth(method = lm, se = FALSE) + 
  ggtitle("Incidents of Companies")

cor.test(airline_safety$incidents_85_99, airline_safety$incidents_00_14)
