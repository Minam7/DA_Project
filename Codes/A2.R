accidents_gamma <- tidy(accident_lda, matrix = "gamma")

accidents_data <- accidents_gamma %>% group_by(document) %>% 
  top_n(1, gamma) %>%
  ungroup()

accident_summary <- accidents_data %>% group_by(topic) %>% summarise(count = n()) %>% 
  arrange(desc(count))

accident_summary <- accident_summary %>% inner_join(top_terms_merge, by = c("topic"))

occurance_sum = sum(accident_summary$count)
accident_summary <- accident_summary %>% mutate(count_percent = 100*count/occurance_sum)

knitr::kable(accident_summary %>% select(-count_percent))

accident_summary %>% arrange(topic) %>% 
  hchart(type = "pie", hcaes(x = words ,y = count_percent)) %>% 
  hc_yAxis(title = list(text = "Count")) %>% 
  hc_xAxis(title = list(text = "Topic")) %>% 
  hc_title(text = "Airsafety Occurance Based on Topic", style = list(fontWeight = "bold")) %>% 
  hc_add_theme(hc_theme_538())
