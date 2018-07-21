iran_casn <- casn_topics %>% filter(str_detect(Operator, "Iran")) %>% 
  bind_rows(casn_topics %>% filter(str_detect(Operator, "Qeshm")))

rest_Iranian_operators = as.data.frame(c("Mahan Air", "Zagros Air", "Kish Air", "Taban Air", "Caspian Airlines",
                           "Saha Air"))
colnames(rest_Iranian_operators) = c("Operator")

iran_casn <- iran_casn %>% 
  bind_rows(casn %>% inner_join(rest_Iranian_operators, by = c("Operator")))


