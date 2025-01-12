

#############################################
########## Data Preprocessing ##############
#############################################

### Load Libraries
library(tidyverse)

###
### Import Data
###

mall_reviews <- read_csv("Data/mall_reviews.csv")
ridge_reviews <- read_csv("Data/ridge_reviews.csv")



###
### Pre - Processing
###

#Add Source
mall_reviews$Source <- "Mall"
ridge_reviews$Source <- "Ridge"

#Merge Data
all_reviews <- rbind(mall_reviews, ridge_reviews)


#Format date variable 
all_reviews$Date <- str_extract(all_reviews$Date, "\\d{1,2} \\w+ \\d{4}")
all_reviews$Date <- lubridate::dmy(all_reviews$Date)

#Location Variable

#Cities
all_reviews$Cities <- str_extract(all_reviews$Tourist_Home_Location, "^[^,]+")
all_reviews$Cities <- ifelse(str_detect(all_reviews$Cities, "contribut"), NA, all_reviews$Cities)

#Countries
all_reviews$Countries <- str_extract(all_reviews$Tourist_Home_Location, "(?<=, ).*$")
all_reviews$Countries <- ifelse(str_detect(all_reviews$Countries, ","), 
                                str_extract(all_reviews$Countries, "(?<=\\,\\s).*$"), 
                                all_reviews$Countries)
        #Rename Misspelled Countries
        us_codes <- c("NY", "NJ", "TX", "Utah", "PA U.S.A", "GA", "IL", "MN", "PA", 
                      "FL", "MD", "MI", "WA", "AR", "AZ", "DC", "IN", "KS", "NH", 
                      "OH", "TN", "UT", "VA")
        canada_codes <- c("CA","Alberta","Ontario")
        
        #replace
        all_reviews <- all_reviews %>% 
                mutate(Countries = case_when(
                        Countries %in% us_codes ~ "United States",
                        Countries %in% canada_codes ~ "Canada",
                        Countries == "UK" ~ "United Kingdom",
                        Countries == "uk" ~ "United Kingdom",
                        Countries == "Paschim Vihar" ~ "India",
                        T ~ Countries #keep original if no match
                )) 

