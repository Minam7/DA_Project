library(topicmodels)
library(stringr)

cause <- casn %>% select(occ_no, Narrative)
# split into words
cause_word <- cause %>%
  unnest_tokens(output = word, input = Narrative)

# plane stopwords
word = c("aircraft", "airplane", "plane", "flight")
plane_stop_words = data_frame(word)

word_counts <- cause_word %>%
  anti_join(stop_words) %>%
  anti_join(plane_stop_words) %>%
  count(occ_no, word, sort = TRUE)

flight_dtm <- word_counts %>%
  cast_dtm(occ_no, word, n)

accident_lda <- LDA(flight_dtm, k = 20, control = list(seed = 1234))
accident_lda
saveRDS(accident_lda, file="Data/lda.rds")

accident_lda = readRDS(file="Data/lda.rds")
accident_topics <- tidy(accident_lda, matrix = "beta")

top_terms <- accident_topics %>%
  group_by(topic) %>%
  top_n(5, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free", nrow = 5) +
  coord_flip()

top_terms_merge <- top_terms %>% group_by(topic) %>% summarise(words = paste(term, collapse=", "))

knitr::kable(top_terms_merge)
