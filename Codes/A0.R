casn <- readr::read_csv("Data/asn_c.csv") %>% as.data.frame(stringsAsFactors = F) %>% 
  mutate(Total_occupants = ifelse(Total_occupants == 0 & Total_fatalities != 0, Total_fatalities, Total_occupants),
         Total_survivors = abs(Total_occupants - Total_fatalities)) %>% 
  mutate(Total_survivors = ifelse(Total_survivors > Total_occupants, Total_occupants, abs(Total_occupants - Total_fatalities))) %>% 
  mutate(is_army = str_detect(Operator, regex("Force|Navy",ignore_case = T))) %>% 
  mutate(occ_no = row_number())
