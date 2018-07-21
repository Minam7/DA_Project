airfare <- read_csv("../Data/Consumer_Airfare_Report__Table_5_-_Detailed_Fare_Information_For_Highest_and_Lowest_Fare_Markets_Under_750_Miles.csv")

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
sum_airfare <- sum_airfare %>% inner_join(american_airlines, by = c("car")) %>% ungroup() %>% 
  group_by(Operator) %>% summarise(price = mean(price)) %>% 
  arrange(desc(Operator))

need = as.data.frame(c(2, 3, 0, 0, 0, 4, 0, 0, 5, 0, 0, 0, 0, 0, 0, 0, 5, 3, 1, 6, 6, 0, 7, 0, 0, 0))
colnames(need) = c('need')

sum_airfare <- sum_airfare %>% bind_cols(need) %>% group_by(need) %>% summarise(price = mean(price)) %>% 
  ungroup() %>% filter(need != 0)

airlines_name = as.data.frame(c("Virgin Atlantic",
                  "Southwest Airlines",
                  "United / Continental*",
                  "Alaska Airlines*",
                  "American*",
                  "Delta / Northwest*",
                  "US Airways / America West*"))
colnames(airlines_name) =  c("airline")

need = as.data.frame(c(1,2,3,4,5,6,7))
colnames(need) = c('need')

airline_safety_total = read_csv("Data/airline_safety.csv") %>% 
  mutate(total_fatal_accidents = (fatal_accidents_85_99 + fatal_accidents_00_14),
         total_incidents = (incidents_85_99 + incidents_00_14)) %>% 
  select(airline, avail_seat_km_per_week, total_fatal_accidents, total_incidents) %>% 
  mutate(score = 20 - (9*total_fatal_accidents + total_incidents)*10^8/avail_seat_km_per_week)

airline_safety_total = merge(airline_safety_total, airlines_name, sort = FALSE) %>% 
  arrange(desc(airline)) %>% 
  bind_cols(need)

safety_price <- airline_safety_total %>% inner_join(sum_airfare, by = c("need"))

cor.test(formula = ~price + score, data = safety_price)

safety_price_avg = mean(safety_price$price)
safety_price_high <- safety_price %>% filter(price > safety_price_avg)
safety_price_low <- safety_price %>% filter(price <= safety_price_avg)
wilcox.test(safety_price_high$score, safety_price_low$score, alternative = "less", exact = FALSE, correct = FALSE)

