# NARVAEZ BETZABETH 20181016117 - 고급데이터분석 U.S. Presidential Analysis
# Libraries --------------------
library(dplyr)
library(tidyr)
library(tidytext)
library(rtweet)
library(ROAuth)
library(base64enc)
library(ggplot2)
library(sentimentr)
library(tm)
library(wordcloud)
library(wordcloud2)
library(RColorBrewer)
library(purrr)


# Data Collection --------------------

# API Authorizations
consumerKey <- "----"
consumerSecret <- "-----"
accessToken <- "------"
accessTokenSecret <- "-----"

setup_twitter_oauth(consumerKey, consumerSecret, accessToken, accessTokenSecret)


# Get Tweets for Trump
trump_data <- search_tweets("#Trump", n=10000, lang="en", include_rts = FALSE)


# Get Tweets for Biden
biden_data <- search_tweets("#Biden", n=10000, lang="en", include_rts = FALSE)


# Save Text Only
trump_tdata <- trump_data$text

biden_tdata <- biden_data$text


# Save Data
write.csv(trump_tdata,file="trump_tdata.csv")

write.csv(biden_tdata,file="biden_tdata.csv")



# Data Pre-Processing --------------------

# Load Data
trump_twitter_data <- read.csv("C:/Users/betzy/Documents/ADA - US Presidential Election Analysis/NEW ANALYSIS/trump_tdata.csv", header = TRUE)

biden_twitter_data <- read.csv("C:/Users/betzy/Documents/ADA - US Presidential Election Analysis/NEW ANALYSIS/biden_tdata.csv", header = TRUE)
biden_twitter_data

# Convert Data into Corpus
trump_doc <- Corpus(VectorSource(trump_twitter_data))

biden_doc <- Corpus(VectorSource(biden_twitter_data))


# Data Pre-Processing
# convert to lower case
trump_doc <- tm_map(trump_doc, content_transformer(tolower))

biden_doc <- tm_map(biden_doc, content_transformer(tolower))


#remove emojis
removeEmoji <- function(x) gsub("<U+[^[:space:]]*", "", x)
trump_doc <- tm_map(trump_doc, content_transformer(removeEmoji))

biden_doc <- tm_map(biden_doc, content_transformer(removeEmoji))


#remove links
removeLink <- function(x) gsub("http[^[:space:]]*", "", x)
trump_doc <- tm_map(trump_doc, content_transformer(removeLink))

biden_doc <- tm_map(biden_doc, content_transformer(removeLink))


#remove anything other than English Letters or space
removeNumPunct <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)
trump_doc <- tm_map(trump_doc, content_transformer(removeNumPunct))

biden_doc <- tm_map(biden_doc, content_transformer(removeNumPunct))


#remove stopwords
my_stops <- c("realdonaldtrump", "will", "amp", "peopl", "trump")
trump_doc <- tm_map(trump_doc, removeWords, stopwords("english"))
trump_doc <- tm_map(trump_doc, removeWords, my_stops)

my_stops2 <- c("realdonaldtrump", "will", "amp", "peopl", "biden", "joebiden", "bidenharri")
biden_doc <- tm_map(biden_doc, removeWords, stopwords("english"))
biden_doc <- tm_map(biden_doc, removeWords, my_stops2)


#remove extra whitespace
trump_doc <- tm_map(trump_doc, stripWhitespace)

biden_doc <- tm_map(biden_doc, stripWhitespace)


#remove numbers
trump_doc <- tm_map(trump_doc, removeNumbers)

biden_doc <- tm_map(biden_doc, removeNumbers)


#remove punctuation
trump_doc <- tm_map(trump_doc, removePunctuation)

biden_doc <- tm_map(biden_doc, removePunctuation)


#Stemming (converting multiple words of similar origin into one)
trump_doc <- tm_map(trump_doc, stemDocument)

biden_doc <- tm_map(biden_doc, stemDocument)


#Create Term Matrix and Store as DTM
trump_dtm <- DocumentTermMatrix(trump_doc)

biden_dtm <- DocumentTermMatrix(biden_doc)



# Exploratory Data Analysis --------------------

# Word Cloud
# Top 50 Words in #Trump
trump_m <- as.matrix(trump_dtm)
trump_v <- sort(colSums(trump_m), decreasing = TRUE)
trump_d <- data.frame(word = names(trump_v), freq = trump_v)
wordcloud(words = trump_d$word, freq = trump_d$freq, min.freq = 1, max.words = 50, random.order = FALSE, rot.per = 0.50, colors = pal, family = "mono", font = 2)
wordcloud2(trump_d[1:50,])

# Top 50 Words in #Biden
biden_m <- as.matrix(biden_dtm)
biden_v <- sort(colSums(biden_m), decreasing = TRUE)
biden_d <- data.frame(word = names(biden_v), freq = biden_v)
wordcloud(words = biden_d$word, freq = biden_d$freq, min.freq = 1, max.words = 50, random.order = FALSE, rot.per = 0.50, colors = pal, family = "mono", font = 2)
wordcloud2(biden_d[1:50,])

# Frequency Bar Plot
# Top 10 Words in #Trump
barplot(trump_d$freq[1:10], las = 2, names.arg = trump_d$word[1:10], col = "lightblue", main = 'Top 10 Words in #Trump', ylab = 'Word Count')

# Top 10 Words in #Biden
barplot(biden_d$freq[1:10], las = 2, names.arg = biden_d$word[1:10], col = "lightblue", main = 'Top 10 Words in #Biden', ylab = 'Word Count')

# Sentimental Analysis (HARD WAY) --------------------
# Clean Data
biden_twitter_data$stripped_text <- gsub("http\\S+", "", biden_twitter_data$text)
biden_twitter_data_stem <- biden_twitter_data %>% select(stripped_text) %>% unnest_tokens(word, stripped_text)
biden_clean_twitter_data <- biden_twitter_data_stem %>% anti_join(stop_words)

# Sentimental Analysis using the Bing sentiments dataset
bing_biden <- biden_clean_twitter_data %>% inner_join(get_sentiments("bing")) %>% count(word, sentiment, sort = TRUE) %>% ungroup()
bing_biden

# Delete the word 'trump' since that skews our data
bing_biden = bing_biden[-1,]
bing_biden

# Count how many negatives and positives
biden_w <- table(bing_biden$sentiment)
biden_t <- as.data.frame(biden_w)

# Overall negatives and positives for Biden
biden_t


# Clean Data
trump_twitter_data$stripped_text_t <- gsub("http\\S+", "", trump_twitter_data$text)
trump_twitter_data_stem <- trump_twitter_data %>% select(stripped_text_t) %>% unnest_tokens(word, stripped_text_t)
trump_clean_twitter_data <- trump_twitter_data_stem %>% anti_join(stop_words)

# Sentimental Analysis using the Bing sentiments data set
bing_trump <- trump_clean_twitter_data %>% inner_join(get_sentiments("bing")) %>% count(word, sentiment, sort = TRUE) %>% ungroup()
bing_trump

# Delete the word 'trump' since that skews our data
bing_trump = bing_trump[-1,]
bing_trump

# Count how many negatives and positives
trump_w = table(bing_trump$sentiment)
trump_t = as.data.frame(trump_w)

# Overall negatives and positives and Trump
trump_t



# Sentimental Analysis (EASY WAY) --------------------
# Sentimental analysis using the 'sentimentr' package
biden_sentiment = sentiment_by(biden_twitter_data$stripped_text)
trump_sentiment = sentiment_by(trump_twitter_data$stripped_text_t)

# Show the average sentiment for Trump and Biden
summary(biden_sentiment$ave_sentiment)
summary(trump_sentiment$ave_sentiment)





