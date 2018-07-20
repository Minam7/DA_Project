casn <- casn %>% mutate(occ_no = as.character(occ_no))

casn_topics <- casn %>% inner_join(accidents_data, by = c("occ_no" = "document"))

casn_topics_sum <- casn_topics %>% group_by(topic) %>% 
  summarise(Total_occupants = sum(Total_occupants), 
            Total_fatalities = sum(Total_fatalities),
            Total_survivors = sum(Total_survivors)) %>% 
  mutate(Survival_rate = 100*Total_survivors/Total_occupants, Fatality_rate = 100*Total_fatalities/Total_occupants) %>% 
  inner_join(top_terms_merge, by = c("topic"))



