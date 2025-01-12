

#### STM 

setwd("D:/3. Research Main Room/Publications/1 Current Projects/Amity Conference 2025")

#Directly use all_reviews object (See EDA for background)

data <- all_reviews
names(data)


#### Prepare Meta-covariates
library(stringr)

#Dates
data$Date <- str_extract(data$Date, "\\d{1,2} \\w+ \\d{4}")

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
        geom_line() + 
        theme_light() + 
        facet_wrap( ~ metric, scales = "free_y") +
        labs(x = "K", y = "Value", color = "Metric")

#OptionsK: 22, 17, 14, 10, 31, 33,35


## Compare optionsK

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

topic_names <- c("T1-Ridge","T2-Monuments", "T3-Horse Ride",
                 "T4-No Vehicle Zone", "T5-Time Spending", "T6-Shopping",
                 "T7-Mall Road", "T8-Heritage", "T9-Views",
                 "T10-Average Experience", "T11-Cafes and Restaurants" ,
                 "T12-Good Experiences", "T13-Great Experiences",
                  "T14-Place Descriptions")

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

top_labels <- labelTopics(k14, n = 10)

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

ggplot(top_terms, aes(x = reorder_within(term, beta, topic), y = beta, fill = topic)) +
        geom_col(show.legend = FALSE) +
        facet_wrap(~ topic, scales = "free") +
        coord_flip() +
        scale_x_reordered() +
        labs(x = "Terms", y = "Probability", title = "Top Terms by Topic")


