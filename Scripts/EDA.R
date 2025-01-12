

## Exploratory Data Analysis


#SetWD

setwd("D:/3. Research Main Room/Publications/1 Current Projects/Amity Conference 2025")

#Load Data, Add Source Identifier, and Merge

library(readr)

mall_reviews <- read_csv("Data/Mall_reviews.csv")

ridge_reviews <- read_csv("Data/Ridge_reviews.csv")

#Add Source
mall_reviews$Source <- "Mall"
ridge_reviews$Source <- "Ridge"

#Merge
all_reviews <- rbind(mall_reviews, ridge_reviews)

names(all_reviews)

#corpus creation
library(quanteda)

corpus <- corpus(all_reviews, text_field = "Scrape_Review_Text",
                 docvars = c("Date", "Tourist_Home_Location",
                             "Scrape_Tagline", "Source"))


#Clean Data
corpus <- corpus %>% 
        tokens(remove_punct = T, remove_numbers = T) %>% 
        tokens_tolower() %>% 
        tokens_remove(stopwords("english")) %>%
        tokens_wordstem()



head(docvars(corpus))


#Document Feature Matrix

dfm_corpus <- dfm(corpus)
dfm_corpus

#TF-IDF
tf_idf <- dfm_tfidf(dfm_corpus)
tf_idf

#Explore Features

doc_feature_freq <- rowSums(tf_idf)

feature_freq <- colSums(tf_idf)

### feature selection

#Keep only features of atleast 2 charactors

dfm_corpus <- dfm_keep(dfm_corpus, min_nchar = 3)

#Keep only selected features
sel_features <- dfm_trim(tf_idf, min_termfreq = 10)




### Topic Modeling

library(seededlda)

topics <- textmodel_lda(sel_features, k = 10)

terms(topics, 10)




