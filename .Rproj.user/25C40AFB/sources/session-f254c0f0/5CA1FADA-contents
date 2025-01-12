

#### STM 

#Directly use all_reviews object (See EDA for background)

data <- all_reviews
names(data)


#### Prepare Meta-covariates
library(stringr)

#Dates
data$Date <- str_extract(data$Date, "\\d{1,2} \\w+ \\d{4}")

   #Plot Date
    data$Date <- as.Date(data$Date, format = "%d %b %Y")
    
   #Count date year wise
    count_years <- data %>% 
            count(format(Date, "%Y"))
    
   #count date year+month wise 
   count_year_month <- data %>% 
           count(format(Date, "%Y-%m"))
   
   #Plot results
   ggplot(count_years, aes(x = `format(Date, "%Y")`, y = n)) + 
           geom_bar(stat = "identity", fill = "skyblue", alpha = 0.5) + 
           geom_text(aes(label = n), size = 2.5, vjust = -0.2,
                     family = "Times New Roman") + 
           labs(title = "",
                x = "Year",
                y = "Number of Reviews") + 
           theme(text = element_text("Times New Roman"))
   
   #save
   ggsave("date_years.jpeg", 
          height = 3,
          width = 6,
          units = "in",
          dpi = 300)


library(lubridate)
data$Date <- dmy(data$Date)

### Location (City and Countries)
#Cities
data$Cities <- str_extract(data$Tourist_Home_Location, "^[^,]+")

data$Cities <- ifelse(str_detect(data$Cities, "contribut"), NA, data$Cities)

head(sort(table(data$Cities), decreasing = T), 20)

#Countries
data$Countries <- str_extract(data$Tourist_Home_Location, "(?<=, ).*$")
data$Countries <- #Delete State Names
        ifelse(str_detect(data$Countries, ","), 
               str_extract(data$Countries, "(?<=\\,\\s).*$"), 
               data$Countries)

head(sort(table(data$Countries), decreasing = T), 20)

        #Plot Countries
        
        #Rename Misspelled Countries [Iterative Step]

        #Create a vector of codes to rplace
        us_codes <- c("NY", "NJ", "TX", "Utah", "PA U.S.A", "GA", "IL", "MN", "PA", 
              "FL", "MD", "MI", "WA", "AR", "AZ", "DC", "IN", "KS", "NH", 
              "OH", "TN", "UT", "VA")
        canada_codes <- c("CA","Alberta","Ontario")
        
        #replace
        count_countries <- data %>% 
                mutate(Countries = case_when(
                        Countries %in% us_codes ~ "United States",
                        Countries %in% canada_codes ~ "Canada",
                        Countries == "UK" ~ "United Kingdom",
                        Countries == "uk" ~ "United Kingdom",
                        Countries == "Paschim Vihar" ~ "India",
                        T ~ Countries #keep original if no match
                )) %>%
                count(Countries) #Count the countries
        
        #Clean data
        freq_countries <- count_countries %>% 
                filter(n > 3 & Countries != "India")
        freq_countries
       
         #Order/Sort
        freq_countries <- freq_countries %>% 
                arrange(desc(n))
        freq_countries
        
        #plot
        ggplot(freq_countries, aes(x = n,
                                   y = reorder(Countries, n),
                                   fill = Countries)) + 
                geom_bar(stat = "identity") + 
                geom_text(aes(label = n), hjust = -0.09,
                          family = "Times New Roman") + 
                labs(x = "Number of Review",
                     y = "Countries") +
                theme(text = element_text("Times New Roman"),
                      legend.position = "none",
                      plot.margin = unit(c(0.5,1,0.5,0.5), "cm"))
                
        #Save
        ggsave("country_freq.jpeg",
               height = 4,
               width = 8,
               units = "in",
               dpi = 500)
                

#ID
ID <- seq(1, nrow(data))

### Meta Covariates

meta <- data_frame("ID" = ID,
                   "Source" = data$Source,
                   "Date" = data$Date,
                   "Country" = data$Counties,
                   "City" = data$Cities,
                   "Tagline" = data$Scrape_Tagline,
                   "Review_text" = data$Scrape_Review_Text)
        #Tagline and Review were added to meta-covariates
        #to read them original reivew in future analysis.


#####
## Text Pre-processing
#####

library(stm)

temp <- textProcessor(documents = data$Scrape_Review_Text,
                      metadata = meta,
                      lowercase = T,
                      removenumbers = T,
                      removestopwords = T,
                      removepunctuation = T,
                      stem = T,
                      striphtml = T,
)

out <- prepDocuments(temp$documents,
                     temp$vocab,
                     temp$meta)


#####
## Find ideal K
#####

#Find Ideal Topics
storage_hot <- searchK(out$documents,
                       out$vocab,
                       K = c(3:40),
                       N <- 1000,
                       init.type = "Spectral",
                       heldout.seed = 12345
)

#plot indicators for optimum number of topics, takes good time!  
library(ggplot2)
library(tidyr)
library(purrr)

storage_hot$results %>% 
        pivot_longer(
                cols = -K,
                names_to = "metric",
                values_to = "value") %>% 
        filter(metric %in% 
                       c("lbound", "exclus", 
                         "residual", "semcoh")) %>% 
        mutate(value = map_dbl(value, 1)) %>% 
        mutate(K = map_dbl(K, 1)) %>% 
        ggplot(aes(x = K,
                   y = value,
                   color = metric)) + 
        geom_point() + 
        geom_vline(xintercept = 14, linetype = "dotted", color = "blue") +  
        facet_wrap( ~ metric, scales = "free_y") +
        labs(x = "K", y = "Value", color = "Metric") + 
        theme(text = element_text("Times New Roman"))

#save
ggsave("find_k.jpeg",
       height = 6,
       width = 8,
       units = "in",
       dpi = 500)

#OptionsK: 3, 7, 22, 17, 14, 10, 31, 33,35


## Compare optionsK

k0 <- stm(documents = out$documents,
          vocab = out$vocab,
          K = 0,
          data = out$meta,
          seed = 12345,
          init.type = "Spectral")

k3 <- stm(documents = out$documents,
          vocab = out$vocab,
          K = 3,
          data = out$meta,
          seed = 12345,
          init.type = "LDA")

k7 <- stm(documents = out$documents,
          vocab = out$vocab,
          K = 7,
          data = out$meta,
          seed = 12345,
          init.type = "LDA")


k10 <- stm(documents = out$documents,
                         vocab = out$vocab,
                         K = 10,
                         data = out$meta,
                         seed = 12345,
                         init.type = "Spectral")

k14 <- stm(documents = out$documents,
          vocab = out$vocab,
          K = 14,
          data = out$meta,
          seed = 12345,
          init.type = "Spectral") 

k14_lda <- stm(documents = out$documents,
           vocab = out$vocab,
           K = 14,
           data = out$meta,
           seed = 12345,
           init.type = "LDA") 

k17 <- stm(documents = out$documents,
           vocab = out$vocab,
           K = 17,
           data = out$meta,
           seed = 12345,
           init.type = "Spectral") 

k22 <- stm(documents = out$documents,
           vocab = out$vocab,
           K = 22,
           data = out$meta,
           seed = 12345,
           init.type = "Spectral")

k31 <- stm(documents = out$documents,
           vocab = out$vocab,
           K = 31,
           data = out$meta,
           seed = 12345,
           init.type = "Spectral")

k33 <- stm(documents = out$documents,
           vocab = out$vocab,
           K = 33,
           data = out$meta,
           seed = 12345,
           init.type = "Spectral")

k35 <- stm(documents = out$documents,
           vocab = out$vocab,
           K = 35,
           data = out$meta,
           seed = 12345,
           init.type = "Spectral")

#visualise

plot.STM(k35, type = "summary", n = 10,
         labeltype = "frex")

# After reviewing plots of every model (print out)
# k14 seems to be a good fit

# TOPIC LABELING 
sel_k_model <- k14

Extract_context <- function(topic_numbers, total_statements,
                            total_FREX, META_n,
                            threshold) {
        
        review_text <- findThoughts(sel_k_model, texts = out$meta$Review_text,
                                    topics = topic_numbers, thresh = threshold,
                                    n = total_statements)
        source <- findThoughts(sel_k_model, texts = out$meta$Source,
                               topics = topic_numbers, thresh = threshold,
                               n = total_statements) 
        city <- findThoughts(sel_k_model, texts = out$meta$City,
                                topics = topic_numbers, thresh = threshold,
                                n = META_n) 
        #'META'
        print(data_frame(unlist(city$docs),
                         unlist(city$index)))
        #'KEYWORDS'
        print(labelTopics(sel_k_model, topics = topic_numbers,
                          n = total_FREX))
       
        #'DOCS'
        print(review_text$docs)
        
}

Extract_context(topic_numbers = c(14),
                total_statements = 10,
                total_FREX = 10,
                META_n = 10,
                threshold = 0.5)

## Tentative Topic Nams

topic_names <- c("T1-Place Descriptions","T2-Ridge", "T3-Accessibility",
                 "T4-Infrastructure", "T5-Time Spending", "T6-Shopping",
                 "T7-Mall Road", "T8-Heritage", "T9-Views",
                 "T10-Not Amazed", "T11-Food" ,
                 "T12-Good Experiences", "T13-Walking Experiences",
                  "T14-Shimla")

#Topic Correlations 
top_corr <- topicCorr(k14)
library(igraph)
plot.topicCorr(top_corr, vlabels = topic_names)


#Topic Pravelance
#Estimating MetaData/Topic Relationships [Topic Pravelence]

out$meta$Source <- as.factor(out$meta$Source)

prep <- estimateEffect(1:14 ~ Source, k14, meta = out$meta,
                       uncertainty = "Global")

summary(prep)

plot_pravelence <- plot(prep, covariate = "Source", 
                        model = k14, method = "difference",
                        cov.value1 = "Mall",
                        cov.value2 = "Ridge",
                        labeltype = "custom", custom.labels = topic_names,
                        xlab = "Ridge <--------> Mall")

plot_pravelence

# Topical Content
plot(k14, type = "perspectives", topics = c(12, 13))



#####################
### Lets work on answering research questions
#####################

### Q1 What are the main topics that tourists write about in reviews
### to describe their experience in study locations?

## Visualisation of all topics is necessary

library(tidyverse)

top_labels <- labelTopics(k7, n = 10)

top_frex <- top_labels$frex

top_frex <- as_data_frame(top_frex) 

top_frex[1:10,1]


beta_exp <- exp(k14$beta[1:10]$logbeta)


#This method can tell what are the highly probable topics in a docuement
tibble(make.dt(k14, meta = meta$Country)[1]) %>% pivot_longer(2:15) %>%
        arrange(desc(value))

all_reviews$Scrape_Review_Text[1]



###########
### BETA MATRIX (TOPIC-TERM)
###########
log_beta <- k14$beta$logbeta[[1]] 
topic_term_matrix <- exp(log_beta) 

rownames(topic_term_matrix) <- topic_names  # Topic names
colnames(topic_term_matrix) <- k14$vocab


top_terms <- as.data.frame(topic_term_matrix) %>%
        mutate(topic = rownames(.)) %>%
        pivot_longer(cols = -topic, names_to = "term",
                     values_to = "beta") %>%
        group_by(topic) %>%
        slice_max(beta, n = 10) %>%
        arrange(topic, -beta)

library(tidytext)

ggplot(top_terms, aes(x = reorder_within(term, beta, topic),
                      y = beta, fill = topic)) +
        geom_col(show.legend = FALSE) +
        facet_wrap(~ topic, scales = "free", ncol = 4) +
        coord_flip() +
        scale_x_reordered() +
        labs(x = "Term Features", y = "Probability", title = "") +
        theme(plot.margin = unit(c(1,2,1,1), "cm"),
              axis.title.x = element_text(size = 12, face = "bold",
                                          margin = unit(c(1,0,0,0), "cm")),
              axis.title.y = element_text(size = 12, face = "bold",
                                          margin = unit(c(0,1,0,0), "cm")),
              strip.text = element_text(face = "bold", size = 14),
              axis.text.y = element_text(size = 12),
              axis.text.x = element_text(size = 10))

#save plot
ggsave("topics.jpeg",
       width = 16,
       height = 16,
       units = "in",
       dpi = 600)






#######
### Topic Popularity
#######

# Extract theta matrix (document-topic distribution)
theta_matrix <- k14$theta

# Find the dominant topic for each document
dominant_topics <- apply(theta_matrix, 1, which.max)

apply(theta_matrix, 2, which.max)

# Count the occurrences of each topic
topic_counts <- as.vector(table(dominant_topics))

topic_popularity <- 
        tibble(topic_names, topic_counts) %>%
        arrange(desc(topic_counts))

topic_popularity <-  rename(topic_popularity,"Topics" = topic_names)
topic_popularity <- rename(topic_popularity, "Frequency" = topic_counts)

topic_popularity

# plot most common topic using base plot.stm()
plot.STM(k14)

#custom GGPLOT
ggplot(topic_popularity, aes(x = reorder(Topics, -Frequency),
                             y = Frequency,
                             fill = Frequency)) +
        geom_bar(stat = "identity") + 
        geom_text(aes(label = Frequency), vjust = -0.5) +
        scale_fill_viridis_c() +  # Use viridis color scale
        labs(title = "",
             x = "Topics", y = "Frequency") +
        theme_minimal() +
        theme(plot.margin = unit(c(1,1,1,2), "cm"),
                axis.text.x = element_text(angle = 45, hjust = 1,
                                         size = 9)
              )
#save plot
ggsave("topic_popularity.jpeg",
       width = 8,
       height = 6,
       units = "in",
       dpi = 600)


#####################
#### Topic Exploration for Report Writing
#####################

## Highly associated documents with review
findThoughts(k14, all_reviews$Scrape_Review_Text, topic = 13, n = 3)

        #Find Indices of Top Review in Original Data
        tibble(make.dt(k14)[, c(1, 14)]) %>% 
                slice_max(order_by = Topic13, n = 10) 
        
        #Try Experimenting with Features 
        labelTopics(k14,topic = 11, n = 20)
        
        #Find nationalities of top contributors
        tibble(make.dt(k14, meta = meta$Country)[,c(1,3,16)]) %>% 
        slice_max(order_by = Topic2, n = 10)
        
        #Explore dimenions of particular document
        doc_index <- c(173) #enter doc index here!
        all_reviews[doc_index,c(3,4,5,6,7)] %>% 
                pivot_longer(1:5)

## Explore Meta Data Information Associated with Topics

"meta$Country was chosen as meta data characteristics,
 in c(1,6,16), 1 gives index of representing documents, 6 or the middle 
 number is for topic index position t+1 always and the last term 16 
 or anything else is the meta data, order_by arguement will also 
 change according to variable name describing topic"

tibble(make.dt(k14, meta = meta$Country)[,c(1,13,16)]) %>% 
        slice_max(order_by = Topic12, n = 100) %>%
        count(meta) %>% arrange(-n)
        
## 
        
        
###########
### Sentiment Analysis score using AFINN
###########
        
beta_matrix <- as.data.frame(topic_term_matrix) %>%
        mutate(topic = rownames(.)) %>%
        pivot_longer(cols = -topic, names_to = "term",
                     values_to = "beta") %>%
        group_by(topic) %>%
        slice_max(beta, n = 3062) %>%
        arrange(topic, -beta)

library(SnowballC)
#Sentiment lexicon
afinn_lexicon <- get_sentiments("afinn")
afinn_lexicon <- afinn_lexicon %>%
        mutate(stemmed_word = wordStem(word)) %>%
        select(stemmed_word, value)


#Sentiment Score

topic_sentiment <- beta_matrix %>% 
        left_join(afinn_lexicon, by = c("term" = "stemmed_word")) %>%
        mutate(value = replace_na(value, 0)) %>% 
        group_by(topic) %>%
        mutate(value = value /sum(value)) %>% #normalise
        mutate(value = value * beta) %>% #weight
        summarise(topic_sentiments = sum(value)) %>% 
        arrange(desc(topic_sentiment))
topic_sentiment

#All topics on a sentiment planes
# Create a sentiment plot
library(ggrepel)

#Bring Sentiment Score on a -1 to 1 scale
#topic_sentiment <- topic_sentiment %>% 
        #mutate(topic_sentiments = 2 * ((topic_sentiments - min(topic_sentiments)) / (max(topic_sentiments) - min(topic_sentiments))) - 0.5)

ggplot(topic_sentiment, aes(x = topic, y = (topic_sentiments))) +
        geom_point(aes(color = topic_sentiments), size = 3) +
        geom_text_repel(aes(label = topic), hjust = 0.3, vjust = -0.22,
                  size = 3.5) +  # , fontface = "bold"
        scale_color_gradient(low = "red", high = "green", 
                             name = ""
                             ) +  # Color scale from negative to positive sentiment
        labs(title = "", x = "Topic", y = "Sentiment Score") +
        theme(plot.margin = unit(c(0,0.5,0.5,1), "cm"),
              axis.title.x = element_text(face = "bold", size = 10),
              axis.title.y = element_text(face = "bold", size = 10,
                                          vjust = 5),
                axis.text.x = element_text(angle = 45, hjust = 1,
                                         size = 9))

#save plot
ggsave("sentiment.jpeg",
       width = 10,
       height = 8,
       units = "in",
       dpi = 600)

#Barplot (Alternative)
ggplot(topic_sentiment, aes(x = reorder(topic, topic_sentiments), y = topic_sentiments, fill = topic_sentiments)) +
        geom_bar(stat = "identity", show.legend = FALSE) +
        coord_flip() +  # Flips the axes to make the labels more readable
        scale_fill_gradient2(low = "red", high = "green", mid = "yellow", midpoint = 0) +
        labs(title = "Topic Sentiment Analysis", x = "Topic", y = "Sentiment") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))


##############
#### Sentiment Analysis (Review Level) using NRC emotions
##############

library(tidytext)
reviews_tidy <- all_reviews %>% 
        unnest_tokens(word, Scrape_Review_Text) %>%
        left_join(get_sentiments("nrc"))

# Calculate sentiment scores
reviews_sentiment <- reviews_tidy %>%
        group_by(`web-scraper-order`) %>%  # Replace 'id' with your unique identifier column
        summarise(
                positive = sum(sentiment == "positive", na.rm = T),
                negative = sum(sentiment == "negative", na.rm = T)) %>%
        mutate(sentiment = positive - negative)

# Classify reviews
reviews_classified <- reviews_sentiment %>%
        mutate(sentiment_class = ifelse(sentiment > 0, "positive", "negative"))

"This senitment classification I can attach as meta covariate to STM model"

## Explore

#How many reviews were negative
table(reviews_classified$sentiment_class)

#Most Positive Reviews
reviews_classified %>% filter(sentiment_class == "positive") %>%
        arrange(-sentiment)

#Most Negative Reviews
reviews_classified %>% filter(sentiment_class == "negative") %>%
        arrange(sentiment)

#Check Original Content
all_reviews$Scrape_Review_Text[2376]


#Top Representing Documents from a topic in terms of Sentiment
tibble(make.dt(k14, meta = reviews_classified$sentiment)[,c(1,5,16)]) %>% 
        slice_max(order_by = Topic4, n = 100) %>% arrange(meta)

print(all_reviews$Scrape_Review_Text[c(1612,2114,1539,685,2538)])


#Explore Countries in terms of sentiment

meta$sentiment <- reviews_classified$sentiment_class

#Negative
meta %>% filter(sentiment == "negative") %>%
        group_by(Country) %>% count(Country) %>% arrange(-n)
        
#Positive
meta %>% filter(sentiment == "positive") %>%
        count(Country) %>% arrange(-n)



##############################
#### Exploring some other methods to STM
##############################

## stmCorrViz
"It displays the hierarchical structure of the STM,
 based on topic correlations"

stmCorrViz(k14, file_out = "temp.html",
documents_raw = meta$Review_text,
title = topic_names)

        #Static plot form STM
        topic_corr <- topicCorr(k14)
        #Plot corr
        plot.topicCorr(topic_corr, vlabels = topic_names)
        
