library(dplyr)
library(readr)
library(stringr)

age_death = read_csv("Downloads/R/DA_Project/Data/asn_c.csv") %>% as.data.frame(stringsAsFactors = F) %>% 
  mutate(Total_occupants = ifelse(Total_occupants == 0 & Total_fatalities != 0, Total_fatalities, Total_occupants),
         Total_survivors = abs(Total_occupants - Total_fatalities)) %>% 
  mutate(Total_survivors = ifelse(Total_survivors > Total_occupants, Total_occupants, abs(Total_occupants - Total_fatalities))) %>% 
  mutate(is_army = str_detect(Operator, regex("Force|Navy",ignore_case = T))) %>% 
  mutate(occ_no = row_number()) %>% 
  select(C.n.msn, Date, FirstFlight, Total_occupants, Total_fatalities, TotalAirframeHrs) %>% 
  na.omit() %>% 
  mutate(age = (Date - FirstFlight),  death_rate = (Total_fatalities/Total_occupants)) %>% 
  group_by(C.n.msn) %>% 
  summarise(date = max(Date), first_flight = min(FirstFlight), age = max(age), total_airframe = max(TotalAirframeHrs), total_occupants = max(Total_occupants), total_fatalities = max(Total_fatalities)) %>% 
  mutate(death_rate = (total_fatalities/total_occupants)*100) %>% 
  filter(!is.nan(death_rate))

cor.test(age_death$age, age_death$death_rate)
