iran_casn <- casn_topics %>% filter(str_detect(Operator, "Iran")) %>% 
  bind_rows(casn_topics %>% filter(str_detect(Operator, "Qeshm")))

rest_Iranian_operators = as.data.frame(c("Mahan Air", "Zagros Air", "Kish Air", "Taban Air", "Caspian Airlines",
                           "Saha Air"))
colnames(rest_Iranian_operators) = c("Operator")

iran_casn <- iran_casn %>% 
  bind_rows(casn %>% inner_join(rest_Iranian_operators, by = c("Operator")))

iran_casn <- iran_casn %>% 
  mutate(kind = ifelse(Total_fatalities > 0, "Crash", "Incident"))

# a: sanctions effect
iran_sum <- iran_casn %>% group_by(Date, kind) %>% 
  summarise(count = n(), fatalities = sum(Total_fatalities))

iran_sum <- iran_casn %>% ungroup() %>% group_by(Date) %>% 
  summarise(count = n(), fatalities = sum(Total_fatalities)) %>% 
  mutate(kind = "All") %>% bind_rows(iran_sum)

iran_sum  %>% 
  hchart(type = "column", hcaes(x = Date, y = count, group = kind)) %>% 
  hc_yAxis(title = list(text = "Count")) %>% 
  hc_xAxis(title = list(text = "Year"), 
           plotLines = list(list(color = "#FF0000", width = 2, value = 1978, dashStyle = 'shortdash', 
                                 label = list(text = "Iran Revolution - Sanctions")))) %>% 
  hc_title(text = "Iran Safety Occurance", style = list(fontWeight = "bold")) %>% 
  hc_add_theme(hc_theme_flat())

iran_before <- iran_sum %>% filter(Date < 1978 & kind == "All")
iran_after <- iran_sum %>% filter(Date >= 1978 & kind == "All")
t.test(iran_before$count, iran_after$count, alternative = "less")

# 6
# worst airline
worst_airline <- iran_casn %>% filter(!is.na(Operator)) %>% 
  filter(is_army == FALSE) %>% 
  group_by(Operator) %>% 
  summarise(Total_occupants = sum(Total_occupants), Total_fatalities = sum(Total_fatalities),
            Total_survivors = sum(Total_survivors), Survival_rate = 100*Total_survivors/Total_occupants) %>% 
  ungroup() %>% 
  filter(Survival_rate != 0) %>% 
  top_n(5, wt = desc(Survival_rate)) %>% 
  arrange(Survival_rate)

p = ggplot(data = worst_airline, mapping = aes(x = reorder(Operator, Survival_rate), y = Survival_rate, fill = Total_fatalities)) + 
  geom_bar(stat="identity") + scale_fill_gradient(low="brown1", high="brown4") + 
  ggtitle("Worst Iranian Airlines with lowest survival rate") + 
  xlab("Airline") + 
  ylab("Survival rate") + guides(color=guide_legend(title="fatality"), fill=guide_legend(title="fatality")) + 
  coord_flip()
p

# worst airplane
worst_airplane <- iran_casn %>% filter(!is.na(Type)) %>% 
  filter(is_army == FALSE) %>% 
  group_by(Type) %>% 
  summarise(Total_occupants = sum(Total_occupants), Total_fatalities = sum(Total_fatalities),
            Total_survivors = sum(Total_survivors), Survival_rate = 100*Total_survivors/Total_occupants) %>% 
  ungroup() %>% 
  filter(Survival_rate != 0) %>% 
  top_n(5, wt = desc(Survival_rate)) %>% 
  arrange(Survival_rate)

p = ggplot(data = worst_airplane, mapping = aes(x = reorder(Type, Survival_rate), y = Survival_rate, fill = Total_fatalities)) + 
  geom_bar(stat="identity") +
  ggtitle("Worst Iranian Airplanes with lowest survival rate") + 
  xlab("Airplane") + 
  ylab("Survival rate") + guides(color=guide_legend(title="fatality"), fill=guide_legend(title="fatality")) + 
  coord_flip()
p

# worst route
worst_departure_airport <- iran_casn %>% filter(!is.na(DepartureAirport)) %>% 
  filter(is_army == FALSE) %>% 
  group_by(DepartureAirport) %>% 
  summarise(Total_occupants = sum(Total_occupants), Total_fatalities = sum(Total_fatalities),
            Total_survivors = sum(Total_survivors), Survival_rate = 100*Total_survivors/Total_occupants) %>% 
  ungroup() %>% 
  filter(Survival_rate != 0) %>% 
  top_n(5, wt = desc(Survival_rate)) %>% 
  arrange(Survival_rate)

p = ggplot(data = worst_departure_airport, mapping = aes(x = reorder(DepartureAirport, Survival_rate), y = Survival_rate, fill = Total_fatalities)) + 
  geom_bar(stat="identity") + scale_fill_gradient(low="midnightblue", high="darkred") +
  ggtitle("Worst Iranian Departure Airports with lowest survival rate") + 
  xlab("Departure Airport") + 
  ylab("Survival rate") + guides(color=guide_legend(title="fatality"), fill=guide_legend(title="fatality")) + 
  coord_flip()
p

# 7
# army and civil flights
year_fat <- iran_casn %>% filter(!is.na(Date)) %>% group_by(Date) %>% 
  summarise(Total_occupants = sum(Total_occupants), Total_fatalities = sum(Total_fatalities),
            Total_survivors = sum(Total_survivors), Survival_rate = 100*Total_survivors/Total_occupants)

highchart() %>% 
  hc_add_series(data = year_fat, type = "spline", hcaes(x = Date, y = Total_fatalities), name = "Total Fatalities") %>% 
  hc_add_series(data = year_fat, type = "spline", hcaes(x = Date, y = Total_survivors), name = "Total Survivors") %>% 
  hc_yAxis(title = list(text = "Count")) %>% 
  hc_xAxis(title = list(text = "Year")) %>% 
  hc_title(text = "Iran Fatalities Per Year", style = list(fontWeight = "bold")) %>%
  hc_add_theme(hc_theme_flat())

# 8
iran_casn_topics_sum <- iran_casn %>% group_by(topic) %>% 
  summarise(Total_occupants = sum(Total_occupants), 
            Total_fatalities = sum(Total_fatalities),
            Total_survivors = sum(Total_survivors)) %>% 
  mutate(Survival_rate = 100*Total_survivors/Total_occupants, Fatality_rate = 100*Total_fatalities/Total_occupants) %>% 
  inner_join(top_terms_merge, by = c("topic"))


iran_casn_topics_sum %>% arrange(desc(Fatality_rate)) %>% 
  hchart(type = "column", hcaes(x = words ,y = Fatality_rate, color = Total_occupants)) %>% 
  hc_yAxis(title = list(text = "Fatality Rate")) %>% 
  hc_xAxis(title = list(text = "Topic")) %>% 
  hc_title(text = "Iran Safety Occurence Topics Fatality Rate", style = list(fontWeight = "bold")) %>% 
  hc_add_theme(hc_theme_google())

iran_casn_topics_yearly <- iran_casn %>% group_by(topic, Date) %>% 
  summarise(Total_occupants = sum(Total_occupants), 
            Total_fatalities = sum(Total_fatalities),
            Total_survivors = sum(Total_survivors)) %>% 
  mutate(Survival_rate = 100*Total_survivors/Total_occupants, Fatality_rate = 100*Total_fatalities/Total_occupants) %>% 
  inner_join(top_terms_merge, by = c("topic")) %>% 
  mutate(Survival_rate = ifelse(is.na(Survival_rate), 0, Survival_rate),
         Fatality_rate = ifelse(is.na(Fatality_rate), 0, Fatality_rate))

iran_casn_topics_yearly %>% arrange(Date) %>% 
  hchart("heatmap", hcaes(x = Date, y = words,value = Fatality_rate)) %>% 
  hc_title(text = "Iran Safety Occurence Topics Fatality Rate in Years", style = list(fontWeight = "bold"))

iran_casn_topic_air <- iran_casn %>% group_by(Type, topic) %>% 
  summarise(Total_occupants = sum(Total_occupants), 
            Total_fatalities = sum(Total_fatalities),
            Total_survivors = sum(Total_survivors),
            occ = n()) %>% 
  mutate(Survival_rate = 100*Total_survivors/Total_occupants, Fatality_rate = 100*Total_fatalities/Total_occupants) %>% 
  inner_join(top_terms_merge, by = c("topic")) %>% 
  mutate(Survival_rate = ifelse(is.na(Survival_rate), 0, Survival_rate),
         Fatality_rate = ifelse(is.na(Fatality_rate), 0, Fatality_rate)) %>% 
  ungroup() %>% 
  group_by(Type) %>% 
  arrange(desc(occ)) %>% 
  slice(1) %>% 
  select(Aircraft = Type, Cause = words, occurance = occ, Fatality_rate, Total_occupants, Total_fatalities) %>% 
  arrange(desc(Total_fatalities))

knitr::kable(iran_casn_topic_air)

iran_casn_topic_air <- iran_casn %>% group_by(topic, Type) %>% 
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
  slice(1:3) %>% 
  select(Aircraft = Type, Cause = words, occurance = occ, Fatality_rate, Total_occupants, Total_fatalities) %>% 
  arrange(desc(Total_fatalities))

knitr::kable(iran_casn_topic_air %>% filter(topic == 1))
knitr::kable(iran_casn_topic_air %>% filter(topic == 2))
knitr::kable(iran_casn_topic_air %>% filter(topic == 3))
knitr::kable(iran_casn_topic_air %>% filter(topic == 4))
knitr::kable(iran_casn_topic_air %>% filter(topic == 5))
knitr::kable(iran_casn_topic_air %>% filter(topic == 6))
knitr::kable(iran_casn_topic_air %>% filter(topic == 7))
knitr::kable(iran_casn_topic_air %>% filter(topic == 8))
knitr::kable(iran_casn_topic_air %>% filter(topic == 9))
knitr::kable(iran_casn_topic_air %>% filter(topic == 10))
knitr::kable(iran_casn_topic_air %>% filter(topic == 11))
knitr::kable(iran_casn_topic_air %>% filter(topic == 12))
knitr::kable(iran_casn_topic_air %>% filter(topic == 13))
knitr::kable(iran_casn_topic_air %>% filter(topic == 14))
knitr::kable(iran_casn_topic_air %>% filter(topic == 15))
knitr::kable(iran_casn_topic_air %>% filter(topic == 16))
knitr::kable(iran_casn_topic_air %>% filter(topic == 17))
knitr::kable(iran_casn_topic_air %>% filter(topic == 18))
knitr::kable(iran_casn_topic_air %>% filter(topic == 19))
knitr::kable(iran_casn_topic_air %>% filter(topic == 20))

