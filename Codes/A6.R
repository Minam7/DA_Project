library(ggplot2)

# worst airline
worst_airline <- casn %>% filter(!is.na(Operator)) %>% 
  filter(is_army == FALSE) %>% 
  group_by(Operator) %>% 
  summarise(Total_occupants = sum(Total_occupants), Total_fatalities = sum(Total_fatalities),
            Total_survivors = sum(Total_survivors), Survival_rate = 100*Total_survivors/Total_occupants) %>% 
  ungroup() %>% 
  filter(Total_occupants > 500) %>% 
  top_n(20, wt = desc(Survival_rate)) %>% 
  arrange(Survival_rate)

p = ggplot(data = worst_airline, mapping = aes(x = reorder(Operator, Survival_rate), y = Survival_rate, fill = Total_fatalities)) + 
  geom_bar(stat="identity") + scale_fill_gradient(low="brown1", high="brown4") + 
  ggtitle("Worst Airlines with lowest survival rate") + 
  xlab("Airline") + 
  ylab("survival ratee") + guides(color=guide_legend(title="fatality"), fill=guide_legend(title="fatality")) + 
  coord_flip()
p

# worst airplane
worst_airplane <- casn %>% filter(!is.na(Type)) %>% 
  filter(is_army == FALSE) %>% 
  group_by(Type) %>% 
  summarise(Total_occupants = sum(Total_occupants), Total_fatalities = sum(Total_fatalities),
            Total_survivors = sum(Total_survivors), Survival_rate = 100*Total_survivors/Total_occupants) %>% 
  ungroup() %>% 
  filter(Total_occupants > 500) %>% 
  top_n(20, wt = desc(Survival_rate)) %>% 
  arrange(Survival_rate)

p = ggplot(data = worst_airplane, mapping = aes(x = reorder(Type, Survival_rate), y = Survival_rate, fill = Total_fatalities)) + 
  geom_bar(stat="identity") +
  ggtitle("Worst Airplanes with lowest survival rate") + 
  xlab("Airplane") + 
  ylab("survival ratee") + guides(color=guide_legend(title="fatality"), fill=guide_legend(title="fatality")) + 
  coord_flip()
p

# worst route
worst_departure_airport <- casn %>% filter(!is.na(DepartureAirport)) %>% 
  filter(is_army == FALSE) %>% 
  group_by(DepartureAirport) %>% 
  summarise(Total_occupants = sum(Total_occupants), Total_fatalities = sum(Total_fatalities),
            Total_survivors = sum(Total_survivors), Survival_rate = 100*Total_survivors/Total_occupants) %>% 
  ungroup() %>% 
  filter(Total_occupants > 500) %>% 
  top_n(20, wt = desc(Survival_rate)) %>% 
  arrange(Survival_rate)

p = ggplot(data = worst_departure_airport, mapping = aes(x = reorder(DepartureAirport, Survival_rate), y = Survival_rate, fill = Total_fatalities)) + 
  geom_bar(stat="identity") + scale_fill_gradient(low="midnightblue", high="darkred") +
  ggtitle("Worst Departure Airports with lowest survival rate") + 
  xlab("Departure Airport") + 
  ylab("survival ratee") + guides(color=guide_legend(title="fatality"), fill=guide_legend(title="fatality")) + 
  coord_flip()
p
