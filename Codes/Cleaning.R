library(dplyr)
library(stringr)
library(readr)

asn <- readr::read_csv("Data/asn.csv")

# delete junk rows
asn <- asn %>% arrange(Engines)
asn = asn[-c(1:2),]

# fix shifted data
bug <- asn %>% arrange(desc(Engines)) %>% slice(1:10)
asn <- asn %>% arrange(desc(Engines))
asn = asn[-c(1:12),]
asn$Engines = as.numeric(asn$Engines)
names_list <- colnames(bug)
x = names_list[1:3]
y = names_list[5:21]
x = append(x,y)
x[21] = "Operator"
colnames(bug) = x
bug <- bug %>% select(colnames(asn))
bug$Engines = as.numeric(bug$Engines)
asn <- bind_rows(asn, bug)
remove(bug, x, y, names_list)

asn <- asn %>% select(-Time, -Registration, -C.n.msn)

# change strings to data for passengers
s <- asn %>% select(Passengers)
s <- str_split_fixed(s$Passengers, pattern = " ", 5) %>% as.data.frame(stringsAsFactors = F)
colnames(s) = c("kill","kill_count", "slash", "busy", "busy_kill_count")
s <- s %>% select(Passenger_fatalities = kill_count, Passenger_occupants = busy_kill_count)
s[is.na(s)] <- 0
s[s == ""] <- 0
s$Passenger_fatalities = as.numeric(s$Passenger_fatalities)
s$Passenger_occupants = as.numeric(s$Passenger_occupants)

asn <- asn %>% select(-Passengers) %>% append(s) %>% as.data.frame(stringsAsFactors = F)
remove(s)

# change strings to data for crew
s <- str_split_fixed(asn$Crew, pattern = " ", 5) %>% as.data.frame(stringsAsFactors = F)
colnames(s) = c("kill","kill_count", "slash", "busy", "busy_kill_count")
s <- s %>% select(Crew_fatalities = kill_count, Crew_occupants = busy_kill_count)
s[is.na(s)] <- 0
s[s == ""] <- 0
s$Crew_fatalities = as.numeric(s$Crew_fatalities)
s$Crew_occupants = as.numeric(s$Crew_occupants)

asn <- asn %>% select(-Crew) %>% append(s) %>% as.data.frame(stringsAsFactors = F)
remove(s)

# change strings to data for passengers
asn <- asn %>% as.data.frame(stringsAsFactors = F) %>% select(-Total) %>% 
  mutate(Total_fatalities = Crew_fatalities + Passenger_fatalities,
         Total_occupants = Crew_occupants + Passenger_occupants)

# change date of first flight
asn = asn[-c(17376),]

asn$FirstFlight <- str_remove_all(asn$FirstFlight, "\\s")

asn <- asn %>% mutate(find = grepl(pattern = "^\\d{4}", asn$FirstFlight))
asn_false <- asn %>% filter(find == FALSE) %>% mutate(FirstFlight = NA)
asn_true <- asn %>% filter(find == TRUE)
asn <- asn %>% select(-FirstFlight)
asn_true$FirstFlight = str_match(asn_true$FirstFlight, pattern = "^\\d{4}")
asn <- bind_rows(asn_false, asn_true) %>% select(-find)

remove(asn_false, asn_true)

# change date of first flight
asn <- asn %>% mutate(find = grepl(pattern = "\\d{4}", asn$Date))
asn_true <- asn %>% filter(find == TRUE)
asn_true$Date = str_match(asn_true$Date, pattern = "\\d{4}")
asn_false <- asn %>% filter(find == FALSE) %>% mutate(Date = NA)
asn <- asn %>% select(-Date)
asn <- bind_rows(asn_false, asn_true)
asn <- asn %>% select(-find)

remove(asn_false, asn_true)

# change data of aeroflot
asn <- asn %>% mutate(find = str_detect(Operator, regex("aeroflot",ignore_case = T)))
asn_true <- asn %>% filter(find == TRUE)
asn_true$Operator = "Aeroflot"
asn_false <- asn %>% filter(find == FALSE)
asn <- bind_rows(asn_false, asn_true)
asn <- asn %>% select(-find)

remove(asn_false, asn_true)

asn[asn == "Unknown"] <- NA

asn[asn == "unknown"] <- NA

asn[asn == ""] <- NA

asn <- asn %>% mutate(DepartureAirport = ifelse(DepartureAirport == "?" | DepartureAirport == "-", NA, DepartureAirport))

write.csv(asn, file = "Data/asn_c.csv",row.names=FALSE)
