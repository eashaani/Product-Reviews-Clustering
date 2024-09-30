# Load Necessary Libraries
library(tidyverse)  # For data manipulation and visualization
library(tm)         # For text mining
library(SnowballC)  # For text stemming
library(cluster)    # For clustering algorithms
library(ggplot2)    # For enhanced plotting
library(gridExtra)  # For arranging multiple ggplots

# Step 1: Create a Sample Dataset
product_reviews <- data.frame(
  Review = c(
    "I love this product! It works great.",
    "This is the worst purchase I have ever made.",
    "Excellent quality and fast shipping.",
    "I am not satisfied with this product.",
    "Amazing experience! Highly recommend.",
    "The product did not meet my expectations.",
    "Fantastic! Will buy again.",
    "Terrible service, I will not be back.",
    "I enjoy using this product every day.",
    "This product is okay, not the best.",
    "Absolutely love it! Will buy more.",
    "Horrible, it broke after one use.",
    "Very happy with my purchase, worth every penny.",
    "The quality is just bad, not worth it.",
    "I would recommend this to my friends.",
    "Not what I expected, very disappointed."
  )
)

# Step 2: Preprocess Text Data
corpus <- Corpus(VectorSource(product_reviews$Review))
corpus <- corpus %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(removePunctuation) %>%
  tm_map(removeNumbers) %>%
  tm_map(removeWords, stopwords("en")) %>%
  tm_map(stripWhitespace)

# Filter out any empty documents
corpus <- corpus[which(sapply(corpus, function(x) nchar(as.character(x)) > 0))]

# Step 3: Convert Text to TF-IDF
dtm <- DocumentTermMatrix(corpus)
tfidf <- weightTfIdf(dtm)
matrix <- as.matrix(tfidf)

# Step 4: Apply K-means Clustering
set.seed(123)
kmeans_result <- kmeans(matrix, centers = 3)

# Step 5: Add Cluster Labels to Original Data
product_reviews$Cluster <- as.factor(kmeans_result$cluster)

# Step 6: Analyze Clusters
cluster_summary <- product_reviews %>%
  group_by(Cluster) %>%
  summarise(Count = n())

# Step 7: Create plots
# Pie chart
pie_data <- as.data.frame(table(product_reviews$Cluster))
pie_data$Percentage <- round(100 * pie_data$Freq / sum(pie_data$Freq), 1)

pie_chart <- ggplot(pie_data, aes(x = "", y = Freq, fill = Var1)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y") +
  geom_text(aes(label = paste0(Percentage, "%")), position = position_stack(vjust = 0.5), size = 3) +
  labs(title = "Distribution of Product Reviews by Cluster") +
  theme_void() +
  theme(legend.position = "right")

# Bar graph
bar_graph <- ggplot(pie_data, aes(x = Var1, y = Freq, fill = Var1)) +
  geom_bar(stat = "identity") +
  labs(title = "Count of Reviews by Cluster", x = "Cluster", y = "Count") +
  theme_minimal()

# Line graph
line_graph <- ggplot(cluster_summary, aes(x = Cluster, y = Count)) +
  geom_line(aes(group = 1), color = "blue") +
  geom_point(size = 3, color = "red") +
  labs(title = "Line Graph of Review Counts by Cluster", x = "Cluster", y = "Count") +
  theme_minimal()

# Arrange all plots in one window
grid.arrange(pie_chart, bar_graph, line_graph, nrow = 1)

