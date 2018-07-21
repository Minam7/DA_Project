airfare <- read_csv("Data/Consumer_Airfare_Report__Table_5_-_Detailed_Fare_Information_For_Highest_and_Lowest_Fare_Markets_Under_750_Miles.csv")

american_airlines <- airfare %>% select(car) %>% distinct(car) %>% arrange(car)
pattern_name = c("Tropic", "Air Plus Comet", "American Airlines", "Alaska Airlines", "JetBlue", "Continental Air Lines",
                 "Discovery Airways", "Delta Air Lines", "Frontier Airlines (FL)", "Tran Airways", "Allegiant Air",
                 "America West Airlines", "", "", "Kiwi Regional Airlines", "", "Spirit Airlines", "Northwest",
                 "", "", "Horizon Air", "", "", "", "Skydive Twin Cities", "", "United Airlines", "US Airways",
                 "American Virginia", "Pacific Western Airlines", "", "Southwest Airlines", "", "", "Mesa Airlines",
                 "Midwest", "Air Wisconsin")

american_airlines <- data.frame(american_airlines, pattern_name)
colnames(american_airlines ) <- c("car","Operator")

airfare <- na.omit(airfare)
airfare <- airfare %>% mutate(price = as.numeric(str_extract(mkt_fare,"\\d+")))
sum_airfare <- airfare %>% group_by(car, Year) %>% summarise(price = mean(price))
sum_airfare <- sum_airfare %>% inner_join(american_airlines, by = c("car")) %>% ungroup()

american_crashes <- casn_topics %>% ungroup() %>% 
  inner_join(american_airlines, by = c("Operator"))

airline_safety_total = read_csv("../Data/airline_safety.csv") %>% 
  mutate(total_fatal_accidents = (fatal_accidents_85_99 + fatal_accidents_00_14),
         total_incidents = (incidents_85_99 + incidents_00_14)) %>% 
  select(airline, avail_seat_km_per_week, total_fatal_accidents, total_incidents) %>% 
  mutate(score = 20 - (9*total_fatal_accidents + total_incidents)*10^8/avail_seat_km_per_week)

sum_american_crashes <- american_crashes %>% group_by(Operator, Date) %>% 
  summarise(Total_occupants = sum(Total_occupants), 
            Total_fatalities = sum(Total_fatalities),
            Total_survivors = sum(Total_survivors),
            occurance = n()) %>% 
  mutate(Survival_rate = ifelse(Total_occupants < 1, 0, 100*Total_survivors/Total_occupants), 
         Fatality_rate = ifelse(Total_occupants < 1, 0, 100*Total_fatalities/Total_occupants)) %>% 
  ungroup()
  
Total_occupants_avg <- mean(sum_american_crashes$Total_occupants)
sum_american_crashes <- sum_american_crashes %>% 
  mutate(safety = ifelse(Total_occupants < 1, 0.1, Survival_rate - Total_occupants_avg/(occurance*Total_occupants)))

crash_price <- sum_airfare %>% inner_join(sum_american_crashes, by = c("Operator", "Year" = "Date"))

price_avg <- mean(crash_price$price)
crash_price_high <- crash_price %>% filter(price > price_avg)
crash_price_low <- crash_price %>% filter(price <= price_avg)

kruskal.test(price ~ safety, data = crash_price)
wilcox.test(crash_price_high$safety, crash_price_low$safety, alternative = "less", exact = FALSE, correct = FALSE)
cor.test(formula = ~price + safety, data = crash_price)
t.test(crash_price_high$safety, crash_price_low$safety, alternative = "less")

