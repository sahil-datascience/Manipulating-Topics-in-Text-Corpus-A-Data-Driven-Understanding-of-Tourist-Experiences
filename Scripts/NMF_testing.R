
### NMF Topic Model

sample_review <- all_reviews$Scrape_Review_Text[1:100]

sample_review <- all_reviews$Scrape_Review_Text

library(tm)

sample_corpus <- Corpus(VectorSource(sample_review))

#clean corpus
library(magrittr)

sample_corpus <- sample_corpus %>%
        tm_map(content_transformer(tolower)) %>%
        tm_map(removePunctuation) %>%
        tm_map(removeNumbers) %>%
        tm_map(stripWhitespace) %>%
        tm_map(removeWords, stopwords("english"))


tdm <- TermDocumentMatrix(sample_corpus)

library(dplyr)
tdm_matrix <- as.matrix(tdm)

library(NMF)

nmf_result <- nmf(tdm_matrix, 5)

# Extract the basis and coefficient matrices
W <- basis(nmf_result)
H <- coef(nmf_result)

# Example visualization
barplot(W[,1], main = "Topic 1", col = "blue")
barplot(W[,2], main = "Topic 2", col = "red")


# Display the top terms for each topic
top_terms <- apply(H, 1, function(x) {
        terms <- rownames(tdm_matrix)
        terms[order(x, decreasing = TRUE)[1:10]]
})

print(top_terms)


