library(reshape2)
sum_airfare <- airfare %>% group_by(car, Year) %>% summarise(price = mean(price))
sum_airfare <- sum_airfare %>% inner_join(american_airlines, by = c("car")) %>% ungroup()

american_crashes <- casn_topics %>% ungroup() %>% 
  inner_join(american_airlines, by = c("Operator"))

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

crash_efect = crash_price %>% 
  select(year = Year, airline = Operator, price, occurance) %>% 
  filter(airline == "American Airlines" |
           airline == "United Airlines" |
           airline == "Delta Air Lines")

ggplot(crash_efect, aes(x = year, y = occurance , group = airline, fill = airline)) +
  geom_bar(stat="identity",position="dodge") + ggtitle("Airline Safety Occurance Yearly")

ggplot(crash_efect, aes(x = year, y = price , group = airline, fill = airline)) +
  geom_bar(stat="identity",position="dodge") + ggtitle("Airline Average Price Yearly")

cor.test(crash_efect$price, crash_efect$occurance)