library(highcharter)

# army and civil flights
year_fat <- casn %>% filter(!is.na(Date)) %>% group_by(Date) %>% 
  summarise(Total_occupants = sum(Total_occupants), Total_fatalities = sum(Total_fatalities),
            Total_survivors = sum(Total_survivors), Survival_rate = 100*Total_survivors/Total_occupants)

# remove bad data
year_fat <- year_fat[-c(3),]

highchart() %>% 
  hc_add_series(data = year_fat, type = "spline", hcaes(x = Date, y = Total_fatalities), name = "Total Fatalities") %>% 
  hc_add_series(data = year_fat, type = "spline", hcaes(x = Date, y = Total_survivors), name = "Total Survivors") %>% 
  hc_yAxis(title = list(text = "Count")) %>% 
  hc_xAxis(title = list(text = "Year")) %>% 
  hc_title(text = "Fatalities Per Year", style = list(fontWeight = "bold")) %>%
  hc_add_theme(hc_theme_flat())


year_fat  %>% 
  hchart(type = "spline", hcaes(x = Date, y = Survival_rate), name = "Survival Rate") %>% 
  hc_yAxis(title = list(text = "Survival Rate")) %>% 
  hc_xAxis(title = list(text = "Year")) %>% 
  hc_title(text = "Survival Rate Per Year", style = list(fontWeight = "bold")) %>%
  hc_add_theme(hc_theme_sandsignika())