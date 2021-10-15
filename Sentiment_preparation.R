#-----------------------------------------------------------------------------------------
# Load libraries
#-----------------------------------------------------------------------------------------
library(R.utils)
library(ggplot2)
library(plyr)
library(anytime)
library(tm)
library(utils)
library(wordcloud)
library(tidytext)
library(dplyr)

# Clean up working environment
remove(list=ls())
cat("\f")

#-----------------------------------------------------------------------------------------
# Set up work space
#-----------------------------------------------------------------------------------------

# WD Jasper
dir <- "C:/Users/jaspe/OneDrive - Erasmus University Rotterdam/MScBA BAM/Advanced Statistics and Programming/ASP Group Assignment/R-Bestanden/R_project/"

# WD Mathijs
dir <- "/Users/Mathijs/OneDrive - Erasmus University Rotterdam/ASP Group Assignment/R-Bestanden/R_project/"

# WD Freek
dir <- "/Users/Freek/OneDrive - Erasmus University Rotterdam/ASP Group Assignment/R-Bestanden/R_project/"

# Mapping
dirData <- paste0(dir, "Data/")
dirProg <- paste0(dir, "Programs/")
dirRslt <- paste0(dir, "Results/")

#-----------------------------------------------------------------------------------------
# Import and preparing data
#-----------------------------------------------------------------------------------------

# Transform downloaded files from .gz to .tsv
myFiles <- list.files(pattern="dataset.*tsv")
for(i in myFiles) {
  gunzip(i)
}

# Transform .tsv to .csv, including only US tweets in English.
myFilesTSV <- list.files(pattern="dataset.tsv")
for (j in myFilesTSV) {
  data <- read.csv(j, sep = "\t")
  data <- data[grep("US", data$country_code), ]
  data <- data[grep("en", data$lang), ]
    write.csv(data, file = paste0(dir,j,".csv"))
      file.remove(j)
}

# Importing a dataframe with all individual daily tweets files
dataset <- ldply(list.files(), read.csv, header=TRUE)

# Remove scientific notation of tweet IDs
options(scipen = 100)

# Saving tweet IDs in a list variable
TweetID1 <- as.character(dataset$tweet_id)

# Export to .txt file
writeLines(TweetID1, "IDFILE.txt")

# Processing in Hydrator
# Importing the processed csv
dfDataset <- read.csv("Dataset.csv")

# Using only the variables that are relevant to the analysis
dfTweets <- subset(dfDataset, select = c(text, created_at, user_location))

# Transforming the created_at variable into a date
dfTweets$created_at <- substring(dfTweets$created_at,1,10)
dfTweets$created_at <- substring(dfTweets$created_at,4,10)
dfTweets$created_at[-length(dfTweets$created_at)] <- paste0(dfTweets$created_at[-length(dfTweets$created_at)], ' 2021')
dfTweets$created_at <- anydate(dfTweets$created_at)

#-----------------------------------------------------------------------------------------
# Cleaning and preparation
#-----------------------------------------------------------------------------------------

# Cleaning the tweet text cells

# Convert all text to lower cases
dfTweets$text <- tolower(dfTweets$text)

# Remove emoticons
dfTweets$text <- gsub("\\p{So}|\\p{Cn}", "", dfTweets$text, perl = TRUE)

# Remove retweets
dfTweets$text <- gsub("rt", "", dfTweets$text)

# Remove usernames
dfTweets$text <- gsub("@\\w+", "", dfTweets$text)

# Remove punctuation
dfTweets$text <- gsub("[[:punct:]]", "", dfTweets$text)

# Remove links
dfTweets$text <- gsub("http\\w+", "", dfTweets$text)

# Remove tabs
dfTweets$text <- gsub("[ |\t]{2,}", "", dfTweets$text)

# Remove blank spaces at the beginning
dfTweets$text <- gsub("^ ", "", dfTweets$text)

# Remove blank spaces at the end
dfTweets$text <- gsub(" $", "", dfTweets$text)

# Remove stop words
stopwords <- stopwords::stopwords(language = "en", source = "snowball", simplify = TRUE)
stopwords_regex <- paste(stopwords::stopwords(language = "en", source = "snowball", simplify = TRUE), collapse = '\\b|\\b')
stopwords_regex <- paste0("\\b", stopwords_regex, "\\b")
dfTweets$text <- stringr::str_replace_all(dfTweets$text, stopwords_regex, "")

# Remove all non-English characters
dfTweets <- dfTweets[which(!grepl("[^\x01-\x7F]+", dfTweets$text)),]

# Limiting Tweets to Vaccine themed
dfTweets <- dfTweets[grep(" vaccine | vaccination | injection | vaccinated", dfTweets$text), ]


# Saving the data set
write.csv(dfTweets, "Tweets_data_set.csv")

#-----------------------------------------------------------------------------------------
# Descriptive visualizations
#-----------------------------------------------------------------------------------------

# Importing saved data set
dfVaccine <- read.csv(file=paste0(dirData,"Tweets_data_set.csv"))

# Create a corpus
TweetCorpus <- VCorpus(VectorSource(dfVaccine))

# Word cloud
wordcloud::wordcloud(TweetCorpus, min.freq = 15, 
                     colors = brewer.pal(8, "Dark2"), random.color = TRUE, 
                     max.words = 150, random.order = FALSE)

#-----------------------------------------------------------------------------------------
# Sentiment score calculation
#-----------------------------------------------------------------------------------------

# Afinn lexicon
afinn = as.data.frame(get_sentiments('afinn'))
get_sentiments("afinn")

# Creating a group variable

# Extract words from all tweets
tweets_words = dfVaccine %>% 
  group_by(X) %>% 
  unnest_tokens(output = word, input = text) %>% 
  ungroup() %>% 
  mutate(row=1:n())
as.data.frame(tweets_words)[1:9999999, c('X', 'word')]

# Sentiment Score for EACH TWEET
sent <- tweets_words %>%
  inner_join(afinn,by = 'word')%>%
  group_by(X)%>%
  summarize(tweet_sentiment = mean(value,na.rm=T)) %>% 
  ungroup()
dfSentimentScores <- merge(dfVaccine, sent, by=c("X"))

dfSentimentScores <- subset(dfSentimentScores, select = c(created_at, text, tweet_sentiment))
write.csv(dfSentimentScores, "dfSentimentNoMean.csv")

# Calculating mean sentiment scores by day
for(i in dfSentimentScores$created_at) { 
  dfSentimentScores$tweet_sentiment[dfSentimentScores$created_at == i] <- 
    mean(dfSentimentScores$tweet_sentiment[dfSentimentScores$created_at == i])
}

# Rounding sentiment scores to 2 digits
dfSentimentScores$tweet_sentiment <- round(dfSentimentScores$tweet_sentiment, digits = 2)

# Removing duplicate values
dfSentimentScores <- dfSentimentScores[!duplicated(dfSentimentScores$created_at), ]

# Changing created_at to date, removing text column
colnames(dfSentimentScores$created_at) <- dfSentimentScores$date
dfSentimentScores$text <- NULL

# Adding column with day number
dfSentimentScores$day <- seq.int(nrow(dfSentimentScores))

# Saving the data set with sentiment scores
write.csv(dfSentimentScores, "SentimentScoresDaily.csv")
dfSentimentScores <- read.csv(file=paste0(dirData,"SentimentScoresDaily.csv"))

#-----------------------------------------------------------------------------------------
# Plotting sentiment data
#-----------------------------------------------------------------------------------------

# Distribution of sentiment scores
ggplot(dfSentimentScores, aes(x=tweet_sentiment)) + geom_histogram(binwidth=.12)

