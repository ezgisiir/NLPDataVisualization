#############################################
####  Topic Modelling                  #####
####  US presidential Inaugural Speeches ### 
###   ISTE 782 RIT version             ###
####  Ezgi Siir Kibris                ####
#### Updated: 17 November 2024        ####
##########################################



# Sources:

# https://sicss.io/2019/materials/day3-text-analysis/topic-modeling/rmarkdown/Topic_Modeling.html#limitations-of-topic-models

# https://content-analysis-with-r.com/6-topic_models.html

# https://www.youtube.com/watch?v=4YyoMGv1nkc&ab_channel=KasperWelbers


### LDA Topic Model ###

# Presidential inaugural speeches #

# Data Preprocessing

rm(list=ls())

library(quanteda) 
data_corpus_inaugural


corp = corpus_reshape(data_corpus_inaugural, to = "paragraphs")
toks <- tokens(corp, remove_punct = TRUE)
dfm <- dfm(toks)
dfm <- dfm_remove(dfm, pattern = stopwords("english"))
dfm <- dfm_trim(dfm, min_docfreq = 5)
dfm


library(topicmodels)
dtm = convert(dfm, to = "topicmodels") 
set.seed(1)
m = LDA(dtm, method = "Gibbs", k = 10,  control = list(alpha = 0.1))



terms(m, 5)

topic = 6
words = posterior(m)$terms[topic, ]
topwords = head(sort(words, decreasing = T), n=50)
head(topwords)


# Wordclouds 

library(wordcloud)

wordcloud(names(topwords), topwords)

# Add color to word cloud
library(RColorBrewer)
wordcloud(names(topwords), topwords,colors=brewer.pal(8, "Dark2"))


# Top words

library(tidytext)
library(dplyr)
library(ggplot2)

president_topics <- tidy(m, matrix = "beta")

president_top_terms <- 
  president_topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

president_top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()


# Evaluation

# LDA vis produces html
# Circles are topic. when we click we get most common words and their distribution.
# when circles are closer maybe we need less topics
# when they are further apart maybe we need more topics
# Hover around topics and words

#install.packages(LDAvis)

library(LDAvis)   

dtm = dtm[slam::row_sums(dtm) > 0, ]
phi = as.matrix(posterior(m)$terms)
theta <- as.matrix(posterior(m)$topics)
vocab <- colnames(phi)
doc.length = slam::row_sums(dtm)
term.freq = slam::col_sums(dtm)[match(vocab, colnames(dtm))]

json = createJSON(phi = phi, theta = theta, vocab = vocab,
                  doc.length = doc.length, term.frequency = term.freq)
serVis(json)


############################################################################################
