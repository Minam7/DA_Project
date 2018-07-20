company_attr = read_csv("Downloads/R/DA_Project/Data/asn_c.csv") %>% as.data.frame(stringsAsFactors = F) %>% 
  mutate(Total_occupants = ifelse(Total_occupants == 0 & Total_fatalities != 0, Total_fatalities, Total_occupants),
         Total_survivors = abs(Total_occupants - Total_fatalities)) %>% 
  mutate(Total_survivors = ifelse(Total_survivors > Total_occupants, Total_occupants, abs(Total_occupants - Total_fatalities))) %>% 
  mutate(is_army = str_detect(Operator, regex("Force|Navy",ignore_case = T))) %>% 
  mutate(occ_no = row_number()) %>% 
  filter(is_army == FALSE) %>% 
  select(C.n.msn,
         Date,
         Operator,
         FirstFlight,
         Total_occupants,
         Total_fatalities,
         TotalAirframeHrs,
         Crew_occupants) %>% 
  na.omit() %>% 
  mutate(airplane_age = (Date - FirstFlight),
         death_rate = (Total_fatalities/Total_occupants)*100) %>% 
  filter(!is.nan(death_rate)) %>% 
  group_by(Operator) %>% 
  summarise(last_event = max(Date),
            first_event = min(FirstFlight),
            mean_age_plane = mean(airplane_age),
            total_airframe = sum(TotalAirframeHrs),
            mean_airframe = mean(TotalAirframeHrs),
            total_occupants = sum(Total_occupants),
            mean_occupants = mean(Total_occupants),
            total_fatalities = sum(Total_fatalities),
            mean_fatalities = mean(Total_fatalities),
            total_crew = sum(Crew_occupants),
            mean_crew = mean(Crew_occupants),
            death_rate_avg = mean(death_rate),
            event_count = n()) %>% 
  mutate(death_rate_company = (total_fatalities/total_occupants)*100,
         company_age = last_event - first_event) %>% 
  filter(!is.nan(death_rate_company))

# total death rate
cor.test(company_attr$company_age, company_attr$death_rate_company)
cor.test(company_attr$mean_age_plane, company_attr$death_rate_company)

cor.test(company_attr$total_airframe, company_attr$death_rate_company)
cor.test(company_attr$mean_crew, company_attr$death_rate_company)
cor.test(company_attr$event_count, company_attr$death_rate_company)

# mean death rate per airplane
cor.test(company_attr$company_age, company_attr$death_rate_avg)
cor.test(company_attr$mean_age_plane, company_attr$death_rate_avg)

cor.test(company_attr$total_airframe, company_attr$death_rate_avg)
cor.test(company_attr$mean_crew, company_attr$death_rate_avg)
cor.test(company_attr$event_count, company_attr$death_rate_avg)
