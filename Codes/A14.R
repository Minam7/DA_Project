iran_casn <- casn_topics %>% filter(str_detect(Operator, "Iran")) %>% 
  bind_rows(casn_topics %>% filter(str_detect(Operator, "Qeshm")))

rest_Iranian_operators = c("Mahan Air", "Zagros Air", "Kish Air", "Taban Air", "Caspian Airlines",
                           "Saha Air")
