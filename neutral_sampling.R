library(ggplot2)
library(tidytext)
library(dplyr)
library(readr)
library(anytime)

# Load manually scored review files here. ("../service_scored.csv")
service <- read_csv("https://raw.githubusercontent.com/seonnyseo/Streaming_Sentiment_Analysis/master/Data/amazon_scored.csv")

# I decided to consider vague reviews as a negative
service$score <- ifelse(service$score == 0, -1, 1)

# Split reviews by each word 
tidy_service <- service %>% unnest_tokens(word, review)

bing_sentiments <- tidy_service %>% inner_join(get_sentiments("bing"), by = "word")
bing_sentiments$score <- ifelse(bing_sentiments$sentiment == "negative", -1, 1)
bing_aggregate <- bing_sentiments %>% select(review_id, score) %>% group_by(review_id) %>% summarise(bing_score = sum(score))

score_compare_service <- merge(x = service, y = bing_aggregate, all.x = TRUE, by = 'review_id')
score_compare_service[is.na(score_compare_service)] <- 0
View(score_compare_service)

# score_compare_service(review_id, date, review, score, bing_score)
# Pick 50 each time 

resampling <- function(service){
  
  neutral_average <- 0
  postive_average <- 0
  negative_average <- 0
  
  for(i in c(2000:3000)){
    set.seed(i)
    random_data <- service[sample(nrow(service), 50),]
    
    neutral_count <- sum(random_data$bing_score == 0)
    positive_neutral <- sum(random_data$bing_score == 0 & random_data$score == 1)
    negative_neutral <- sum(random_data$bing_score == 0 & random_data$score == -1)
    cat(sprintf("Neutral : %.3f P-N : %.3f N-N : %.3f\n", neutral_count/50, 
            positive_neutral/neutral_count, negative_neutral/neutral_count))
    
    neutral_average <- neutral_average + neutral_count/50
    postive_average <- postive_average + positive_neutral/neutral_count
    negative_average <- negative_average + negative_neutral/neutral_count
  }
  cat(sprintf("Neutral : %.3f  Positive : %.3f  Negative : %.3f\n", 
              neutral_average/1000, postive_average/1000, negative_average/1000))
}

resampling(score_compare_service)

