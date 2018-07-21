iran_casn <- casn_topics %>% filter(str_detect(Operator, "Iran")) %>% 
  bind_rows(casn_topics %>% filter(str_detect(Operator, "Qeshm")))

rest_Iranian_operators = as.data.frame(c("Mahan Air", "Zagros Air", "Kish Air", "Taban Air", "Caspian Airlines",
                           "Saha Air"))
colnames(rest_Iranian_operators) = c("Operator")

iran_casn <- iran_casn %>% 
  bind_rows(casn %>% inner_join(rest_Iranian_operators, by = c("Operator")))

iran_casn <- iran_casn %>% 
  mutate(kind = ifelse(Total_fatalities > 0, "Crash", "Incident"))

# a: sanctions effect
iran_sum <- iran_casn %>% group_by(Date, kind) %>% 
  summarise(count = n(), fatalities = sum(Total_fatalities))

iran_sum <- iran_casn %>% ungroup() %>% group_by(Date) %>% 
  summarise(count = n(), fatalities = sum(Total_fatalities)) %>% 
  mutate(kind = "All") %>% bind_rows(iran_sum)

iran_sum  %>% 
  hchart(type = "column", hcaes(x = Date, y = count, group = kind)) %>% 
  hc_yAxis(title = list(text = "Count")) %>% 
  hc_xAxis(title = list(text = "Year"), 
           plotLines = list(list(color = "#FF0000", width = 2, value = 1978, dashStyle = 'shortdash', 
                                 label = list(text = "Iran Revolution - Sanctions")))) %>% 
  hc_title(text = "Iran Safety Occurance", style = list(fontWeight = "bold")) %>% 
  hc_add_theme(hc_theme_flat())

iran_before <- iran_sum %>% filter(Date < 1978 & kind == "All")
iran_after <- iran_sum %>% filter(Date >= 1978 & kind == "All")
t.test(iran_before$count, iran_after$count, alternative = "less")

# 2

# 6

# 7

# 8




