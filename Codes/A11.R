# find safety for flights
casn_flight_safety = casn_topics %>% filter(!is.na(Type) & !is.na(Operator) & Total_occupants != 0) %>% 
  group_by(Type) %>% 
  mutate(airplane_total_occupants = sum(Total_occupants), airplane_total_fatalities = sum(Total_fatalities),
         airplane_total_survivors = sum(Total_survivors), airplane_survival_rate = airplane_total_survivors/airplane_total_occupants,
         airplane_occurance = n(), airplane_mean_occupants = mean(Total_occupants)) %>% 
  ungroup() %>% 
  group_by(Operator) %>% 
  mutate(airline_total_occupants = sum(Total_occupants), airline_total_fatalities = sum(Total_fatalities),
         airline_total_survivors = sum(Total_survivors), airline_survival_rate = airline_total_survivors/airline_total_occupants,
         airline_occurance = n(), airline_mean_occupants = mean(Total_occupants)) %>% 
  ungroup() %>% 
  mutate(score = (airplane_survival_rate*airplane_mean_occupants) + (airline_survival_rate*airline_mean_occupants) + (airplane_survival_rate*airline_survival_rate*Total_occupants)) %>% 
  group_by(Operator, Type) %>% 
  summarise(flight_safety_indicator = mean(score))
  
