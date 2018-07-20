library(highcharter)
library(ggplot2)

company_year = read_csv("Downloads/R/DA_Project/Data/asn_c.csv") %>% as.data.frame(stringsAsFactors = F) %>% 
  mutate(Total_occupants = ifelse(Total_occupants == 0 & Total_fatalities != 0, Total_fatalities, Total_occupants),
         Total_survivors = abs(Total_occupants - Total_fatalities)) %>% 
  mutate(is_army = str_detect(Operator, regex("Force|Navy",ignore_case = T))) %>% 
  filter(is_army == FALSE) %>% 
  select(C.n.msn,
         year = Date,
         Operator,
         FirstFlight,
         Total_occupants,
         Total_fatalities,
         TotalAirframeHrs,
         Crew_occupants) %>% 
  na.omit() %>% 
  group_by(Operator, year) %>% 
  summarise(count = n(), tot_fatal = sum(Total_fatalities), tot_occu = sum(Total_occupants)) %>% 
  filter(tot_occu > 20, tot_fatal > 5) %>%
  mutate(index = 1, index = cumsum(index), index = max(index)) %>% 
  filter(index > 2) %>% 
  select(-index) %>% 
  mutate(death_rate = tot_fatal*100/tot_occu)

company_year %>% 
  hchart(type = "line",
         hcaes(x = year, y = count, group = Operator)) %>%
  hc_xAxis(title = list(text = "year")) %>%
  hc_yAxis(title = list(text = "Accidents Count"),
           max = 22,
           tickInterval = 1,
           min = 0,
           plotLines = list(list(color = "#FF0000",
                                 width = 2,
                                 value = 11,
                                 dashStyle = 'shortdash'))) %>% 
  hc_title(text = "Accidents Count of an Airline in years",
           style = list(fontWeight = "bold"))

ggplot(company_year, aes(x = year , y = death_rate, group = Operator, fill = death_rate)) + geom_bar(stat = "identity")
