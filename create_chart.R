library(ggplot2)
library(readr)
library(anytime)
library(dplyr)
library(lubridate)
library(stringr)
library(tidytext)
library(padr)
library(easyGgplot2)

amazon <- read.csv("https://github.com/seonnyseo/Streaming_Sentiment_Analysis/raw/master/Data/amazon.csv", stringsAsFactors = FALSE)
spotify <- read.csv("https://github.com/seonnyseo/Streaming_Sentiment_Analysis/raw/master/Data/spotify.csv", stringsAsFactors = FALSE)
pandora <- read.csv("https://github.com/seonnyseo/Streaming_Sentiment_Analysis/raw/master/Data/pandora.csv", stringsAsFactors = FALSE)

amazon <- pre_process(amazon)
spotify <- pre_process(spotify)
pandora <- pre_process(pandora)

amazon$service <- "amazon"
spotify$service <- "spotify"
pandora$service <- "pandora"

pre_process <- function(service)
{
  # PreProcessing
  service$review_id <- 1:nrow(service)
  service$date <- anydate(service$date)
  service <- service[c(3,1,2)]
  
  # Sentiment Score
  tidy_service <- service %>% unnest_tokens(word, review)
  
  # Edit Dictionary, I only add 4 words with sentiment at this time. This can be expanded later. 
  bing_edit <- rbind(get_sentiments("bing"), c("commercial", "negative"))
  bing_edit <- rbind(bing_edit, c("commercials", "negative"))
  bing_edit <- rbind(bing_edit, c("ad", "negative"))
  bing_edit <- rbind(bing_edit, c("ads", "negative"))
  bing_edit <- rbind(bing_edit, c("wish", "negative"))
  
  bing_sentiments <- tidy_service %>% inner_join(bing_edit, by = "word")
  
  bing_sentiments$score <- ifelse(bing_sentiments$sentiment == "negative", -1, 1)
  bing_aggregate <- bing_sentiments %>% select(review_id, score) %>% group_by(review_id) %>% summarise(bing_score = sum(score))
  
  service <- merge(x = service, y = bing_aggregate, all.x = TRUE, by = 'review_id')
  service[is.na(service)] <- 0
  service$bing_judgement <- ifelse(service$bing_score > 0, "positive", 
                                   ifelse(service$bing_score < 0, "negative", "neutral" ))
  
  return(service)
}



####### Frequency Check ##########

word_data <-function(service, start, end, word, sentiment){

  word <- tolower(word)
  
  # Filter Data between start date & end date
  extracted <- service[service$date >= start & service$date <= end,]
  # Filter Date that only contains word
  extracted <- extracted[grepl(word, tolower(extracted$review)),]
  
 
  set.seed(101)

  # Neutral / (Positive|Neutral) / (Negative/Neutral)
  # Pandora	0.14	0.54	0.46
  # Spotify	0.12	0.59	0.41
  # Amazon	0.14	0.70	0.30
  
  ifelse(service$service == "pandora", positive_weight <- 0.54,
         ifelse(service$service == "spotify", positive_weight <- 0.59, positive_weight <- 0.70))
    
  neutral_reviews <- extracted[extracted$bing_judgement == "neutral",] %>% select(review_id)
  positive_neutral <- neutral_reviews[sample(nrow(neutral_reviews), nrow(neutral_reviews) * positive_weight),]
  negative_neutral <- neutral_reviews[!(neutral_reviews$review_id %in% positive_neutral),]
    
  extracted$bing_judgement <- ifelse(extracted$review_id %in% positive_neutral, "positive",
                                      ifelse(extracted$review_id %in% negative_neutral, "negative",
                                            extracted$bing_judgement))
  
  
 
  extracted <- extracted[extracted$bing_judgement == sentiment,]
  
  return(extracted)
}


frequency_month <- function(service, start, end, word, sentiment){
  
  extracted <- word_data(service, start, end, word, sentiment)

  # Make year-month column
  extracted$year_month <- anydate(format(as.Date(extracted$date), "%Y-%m"))

  frequency_df <- extracted %>% group_by(year_month) %>% summarise(frequency = n())
  frequency_df <- frequency_df %>% pad(interval = 'month', start_val = anydate(start), end_val = anydate(end))
  frequency_df[is.na(frequency_df)] <- 0
  return (frequency_df)
}


word_graph <- function(service, word, start, end){

    positive = frequency_month(service, start, end, word, "positive")
  negative = frequency_month(service, start, end, word, "negative")
  
  positive$sentiment <- 'positive'
  negative$sentiment <- 'negative'
  
  frequency_df <- positive %>% full_join(negative)
  
  ret <- ggplot(frequency_df, aes(x = year_month)) +
        geom_line(aes(y = frequency, col = sentiment)) +
        theme(axis.title.x=element_blank())
  
  return(ret)
}

## Word Data
word_review <- word_data(pandora, "2011-01-01", "2017-12-31", "commercial", "positive")
View(word_review)


## Word frequency graph
graph <- word_graph(spotify, "", "2014-01-01", "2017-12-31")
graph
