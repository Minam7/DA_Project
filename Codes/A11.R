# find safety for airplanes
casn_airplanes <- casn_topics %>% filter(!is.na(Type)) %>% 
  group_by(Type) %>% 
  summarise(Total_occupants = sum(Total_occupants), Total_fatalities = sum(Total_fatalities),
            Total_survivors = sum(Total_survivors), Survival_rate = 100*Total_survivors/Total_occupants,
            occurance = n(), mean_occupants = mean(Total_occupants)) %>% 
  ungroup() %>% 
  arrange(desc(Total_survivors)) %>% 
  mutate(rank = row_number(), safety = ifelse(Total_occupants < 1, 0.1, Survival_rate - mean_occupants/(occurance*Total_occupants)))

casn_airplanes <- casn_airplanes %>% select(Type, rank, safety)

# find safety for airlines
casn_airlines <- casn_topics %>% filter(!is.na(Operator)) %>% 
  group_by(Operator) %>% 
  summarise(Total_occupants = sum(Total_occupants), Total_fatalities = sum(Total_fatalities),
            Total_survivors = sum(Total_survivors), Survival_rate = 100*Total_survivors/Total_occupants,
            occurance = n(), mean_occupants = mean(Total_occupants)) %>% 
  ungroup() %>% 
  arrange(desc(Total_survivors)) %>% 
  mutate(rank = row_number(), safety = ifelse(Total_occupants < 1, 0.1, Survival_rate - mean_occupants/(occurance*Total_occupants)))

casn_airlines <- casn_airlines %>% select(Operator, rank, safety)
