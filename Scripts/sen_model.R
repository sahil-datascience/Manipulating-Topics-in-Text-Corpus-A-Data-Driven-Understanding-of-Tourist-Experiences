

## Sentence Based Model


#SetWD

setwd("D:/3. Research Main Room/Publications/1 Current Projects/Amity Conference 2025")


all_reviews

sample_sentence_based <- all_reviews[1:100,c(1,3,4,5,6,7)]

#corpus creation
library(quanteda)

review_corpus <- corpus(sample_sentence_based,
                        text_field = "Scrape_Review_Text")

#docvars = c("Date", "Tourist_Home_Location","Scrape_Tagline", "Source")

summary(review_corpus, 5)

#Change Unit of texts to Sentences

sentence_corpus <- corpus_reshape(review_corpus, to = "sentences")

#sentiments at sentence level
sentences <- as.character(sentence_corpus)

sentiment_scores <- sentimentr::sentiment(sentences)

sentiments <- sentimentr::sentiment(as.character(review_corpus))


library(dplyr)

df <- sentiments %>% group_by(element_id) %>% 
        summarize(n_words = sum(word_count),
                  n_sen = max(sentence_id),
                  senti = sum(sentiment))

sentiments %>% group_by(element_id) %>% 
        arrange(desc(sentiment))

