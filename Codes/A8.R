casn <- casn %>% mutate(occ_no = as.character(occ_no))

casn_topics <- casn %>% inner_join(accidents_data, by = c("occ_no" = "document"))

casn_topics_sum <- casn_topics %>% group_by(topic) %>% 
  summarise(Total_occupants = sum(Total_occupants), 
            Total_fatalities = sum(Total_fatalities),
            Total_survivors = sum(Total_survivors)) %>% 
  mutate(Survival_rate = 100*Total_survivors/Total_occupants, Fatality_rate = 100*Total_fatalities/Total_occupants) %>% 
  inner_join(top_terms_merge, by = c("topic"))


casn_topics_sum %>% arrange(desc(Fatality_rate)) %>% 
  hchart(type = "column", hcaes(x = words ,y = Fatality_rate, color = Total_occupants)) %>% 
  hc_yAxis(title = list(text = "Fatality Rate")) %>% 
  hc_xAxis(title = list(text = "Topic")) %>% 
  hc_title(text = "Safety Occurence Topics Fatality Rate", style = list(fontWeight = "bold")) %>% 
  hc_add_theme(hc_theme_google())

casn_topics_yearly <- casn_topics %>% group_by(topic, Date) %>% 
  summarise(Total_occupants = sum(Total_occupants), 
            Total_fatalities = sum(Total_fatalities),
            Total_survivors = sum(Total_survivors)) %>% 
  mutate(Survival_rate = 100*Total_survivors/Total_occupants, Fatality_rate = 100*Total_fatalities/Total_occupants) %>% 
  inner_join(top_terms_merge, by = c("topic")) %>% 
  mutate(Survival_rate = ifelse(is.na(Survival_rate), 0, Survival_rate),
         Fatality_rate = ifelse(is.na(Fatality_rate), 0, Fatality_rate))

casn_topics_yearly %>% arrange(Date) %>% 
hchart("heatmap", hcaes(x = Date, y = words,value = Fatality_rate)) %>% 
  hc_title(text = "Safety Occurence Topics Fatality Rate in Years", style = list(fontWeight = "bold"))

casn_topic_air <- casn_topics %>% group_by(Type, topic) %>% 
  summarise(Total_occupants = sum(Total_occupants), 
            Total_fatalities = sum(Total_fatalities),
            Total_survivors = sum(Total_survivors),
            occ = n()) %>% 
  mutate(Survival_rate = 100*Total_survivors/Total_occupants, Fatality_rate = 100*Total_fatalities/Total_occupants) %>% 
  inner_join(top_terms_merge, by = c("topic")) %>% 
  mutate(Survival_rate = ifelse(is.na(Survival_rate), 0, Survival_rate),
         Fatality_rate = ifelse(is.na(Fatality_rate), 0, Fatality_rate)) %>% 
  filter(Total_fatalities > 250) %>% 
  ungroup() %>% 
  group_by(Type) %>% 
  arrange(desc(occ)) %>% 
  slice(1) %>% 
  select(Aircraft = Type, Cause = words, occurance = occ, Fatality_rate, Total_occupants, Total_fatalities) %>% 
  arrange(desc(Total_fatalities))

knitr::kable(casn_topic_air)

casn_topic_air <- casn_topics %>% group_by(topic, Type) %>% 
  summarise(Total_occupants = sum(Total_occupants), 
            Total_fatalities = sum(Total_fatalities),
            Total_survivors = sum(Total_survivors),
            occ = n()) %>% 
  mutate(Survival_rate = 100*Total_survivors/Total_occupants, Fatality_rate = 100*Total_fatalities/Total_occupants) %>% 
  inner_join(top_terms_merge, by = c("topic")) %>% 
  mutate(Survival_rate = ifelse(is.na(Survival_rate), 0, Survival_rate),
         Fatality_rate = ifelse(is.na(Fatality_rate), 0, Fatality_rate)) %>% 
  ungroup() %>% 
  filter(Survival_rate < 100) %>% 
  group_by(topic) %>% 
  arrange(desc(occ)) %>% 
  slice(1:5) %>% 
  select(Aircraft = Type, Cause = words, occurance = occ, Fatality_rate, Total_occupants, Total_fatalities) %>% 
  arrange(desc(Total_fatalities))

knitr::kable(casn_topic_air %>% filter(topic == 1))
knitr::kable(casn_topic_air %>% filter(topic == 2))
knitr::kable(casn_topic_air %>% filter(topic == 3))
knitr::kable(casn_topic_air %>% filter(topic == 4))
knitr::kable(casn_topic_air %>% filter(topic == 5))
knitr::kable(casn_topic_air %>% filter(topic == 6))
knitr::kable(casn_topic_air %>% filter(topic == 7))
knitr::kable(casn_topic_air %>% filter(topic == 8))
knitr::kable(casn_topic_air %>% filter(topic == 9))
knitr::kable(casn_topic_air %>% filter(topic == 10))
knitr::kable(casn_topic_air %>% filter(topic == 11))
knitr::kable(casn_topic_air %>% filter(topic == 12))
knitr::kable(casn_topic_air %>% filter(topic == 13))
knitr::kable(casn_topic_air %>% filter(topic == 14))
knitr::kable(casn_topic_air %>% filter(topic == 15))
knitr::kable(casn_topic_air %>% filter(topic == 16))
knitr::kable(casn_topic_air %>% filter(topic == 17))
knitr::kable(casn_topic_air %>% filter(topic == 18))
knitr::kable(casn_topic_air %>% filter(topic == 19))
knitr::kable(casn_topic_air %>% filter(topic == 20))