library(tm)
library(pdftools)
library(ggplot2)
library(dplyr)

# Function to extract text from PDF
extract_text_from_pdf <- function(file_path) {
  pdf_text <- pdf_text(file_path)
  text <- paste(pdf_text, collapse = " ")
  return(text)
}

# Function to preprocess text
preprocess_corpus <- function(corpus) {
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, removeWords, stopwords("en"))
  corpus <- tm_map(corpus, stripWhitespace)
  return(corpus)
}

# Function to calculate TF-IDF
calculate_tfidf <- function(dtm) {
  term_freq <- colSums(as.matrix(dtm))
  doc_freq <- apply(as.matrix(dtm), 2, function(x) sum(x > 0))
  tfidf <- term_freq / doc_freq
  tfidf <- sort(tfidf, decreasing = TRUE)
  return(tfidf)
}

# Paths to your curriculum and textbook PDF files
curriculum_pdf <- "/home/ashutosh/Documents/rProject/ccRemoved.pdf"
textbook_pdf <- "/home/ashutosh/Documents/rProject/book.pdf"

# Extract text
curriculum_text <- extract_text_from_pdf(curriculum_pdf)
textbook_text <- extract_text_from_pdf(textbook_pdf)

# Create a Corpus
corpus <- Corpus(VectorSource(c(curriculum_text, textbook_text)))

# Preprocess the text
corpus <- preprocess_corpus(corpus)

# Create Document-Term Matrices (DTMs)
dtm <- DocumentTermMatrix(corpus)
dtm_matrix <- as.matrix(dtm)

# Split DTMs into curriculum and textbook
curriculum_dtm <- dtm_matrix[1, , drop = FALSE]
textbook_dtm <- dtm_matrix[2, , drop = FALSE]

# Perform TF-IDF Analysis
curriculum_tfidf <- calculate_tfidf(curriculum_dtm)
textbook_tfidf <- calculate_tfidf(textbook_dtm)

# Convert to data frames for visualization
curriculum_tfidf_df <- data.frame(term = names(curriculum_tfidf), tfidf_curriculum = curriculum_tfidf)
textbook_tfidf_df <- data.frame(term = names(textbook_tfidf), tfidf_textbook = textbook_tfidf)

# Merge to find common terms
alignment_df <- merge(curriculum_tfidf_df, textbook_tfidf_df, by = "term", all = TRUE)
alignment_df <- alignment_df[order(-alignment_df$tfidf_curriculum), ]

# Top N terms for visualization (increase to top 50)
top_n <- 50
alignment_df <- alignment_df[1:top_n, ]

# Plot the alignment of TF-IDF scores
ggplot(alignment_df, aes(x = term, y = tfidf_curriculum, fill = factor(!is.na(tfidf_textbook)))) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = round(tfidf_curriculum, 2)), vjust = -0.5, size = 3) +
  geom_text(data = alignment_df[!is.na(alignment_df$tfidf_textbook), ],
            aes(label = round(tfidf_textbook, 2)), vjust = -0.5, size = 3, color = "blue") +
  scale_fill_manual(values = c("grey", "yellow")) +
  ggtitle("TF-IDF Scores of Top Terms: Curriculum vs. Textbook") +
  xlab("Term") +
  ylab("TF-IDF Score") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Original TF-IDF bar plots (Curriculum and Textbook)
ggplot(curriculum_tfidf_df[1:top_n, ], aes(x = reorder(term, tfidf_curriculum), y = tfidf_curriculum)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  ggtitle("Top 50 TF-IDF Terms in Curriculum") +
  xlab("Term") +
  ylab("TF-IDF")

ggplot(textbook_tfidf_df[1:top_n, ], aes(x = reorder(term, tfidf_textbook), y = tfidf_textbook)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  ggtitle("Top 50 TF-IDF Terms in Textbook") +
  xlab("Term") +
  ylab("TF-IDF")


# Calculate Alignment Score
curriculum_top_terms <- names(curriculum_tfidf)[1:top_n]
textbook_top_terms <- names(textbook_tfidf)[1:top_n]

alignment <- intersect(curriculum_top_terms, textbook_top_terms)
alignment_score <- length(alignment) / top_n

# Print alignment results
cat("Alignment Score (0 to 1):", alignment_score, "\n")
cat("Aligned Terms:\n")
print(alignment)

