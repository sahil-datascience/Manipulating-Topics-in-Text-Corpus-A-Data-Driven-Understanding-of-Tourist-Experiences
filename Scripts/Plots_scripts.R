
######
## Plots Scripts
######

#Directly use all_reviews object (See Data Pre-Processing for background)

head(all_reviews)

###
### Plot number of reviews per year
###

#Format date variable 
all_reviews$Date <- str_extract(all_reviews$Date, "\\d{1,2} \\w+ \\d{4}")
all_reviews$Date <- lubridate::dmy(all_reviews$Date)

#Get count for each year
count_years <- all_reviews %>% count(format(Date, "%Y"))

#Plot
ggplot(count_years, aes(x = `format(Date, "%Y")`, y = n)) + 
        geom_bar(stat = "identity", fill = "skyblue", alpha = 0.5) + 
        geom_text(aes(label = n), size = 2.5, vjust = -0.2,
                  family = "Times New Roman") + 
        labs(title = "",
             x = "Year",
             y = "Number of Reviews") + 
        theme(text = element_text("Times New Roman"))
#save
ggsave("Plots/date_years.jpeg", 
       height = 3,
       width = 6,
       units = "in",
       dpi = 300)



