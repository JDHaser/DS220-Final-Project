library(DataComputing)
library(RCurl)
library(tidyr)
library(lubridate)

x <- getURL("https://raw.githubusercontent.com/JDHaser/DS220-Final-Project/master/FinalProjectRawTweets.csv")
RawTweetData <- read.csv(text = x)

ex <- "^([0-9]{4}[-][0-9]{2}[-][0-9]{2}[ ][0-9]{2}[:][0-9]{2}[:][0-9]{2})"

TimedTweets <- RawTweetData %>% filter(grepl(ex, Tweets))

ValuedTweets <- TimedTweets %>% mutate(Value = ifelse(Level.1.label == "Neutral", 0, ifelse(Level.1.label == "Negative", -1, 1)))

WeightedTweets <- ValuedTweets %>% mutate(weightedValue = Value * Level.1.probability) %>% select("Tweets", "Value", "weightedValue")

TimeStampedTweets <- WeightedTweets %>% mutate(Time = substr(Tweets, 12,19))

DayTweets <- TimeStampedTweets %>% mutate(Day = substr(Tweets, 0, 10))

SummarisedTweets <- DayTweets %>% mutate(Time = ifelse(Day == "2017-11-27",  (as.numeric(hms(Time)) / 3600), (as.numeric(hms(Time)) / 3600) + 24))

SummarisedTweets <- SummarisedTweets %>% group_by(Time) %>% summarise(Sentiment = mean(weightedValue))

ggplot(SummarisedTweets, aes(x = Time, y = Sentiment)) + geom_line() +
  labs(title = "Sentiment Analysis of the Houston Rockets vs Brooklyn Nets Game", x = "Time of Day(Hours)", y = "Average Sentiment") +
  expand_limits(y = c(-.8, .8)) +
  scale_x_continuous(breaks = seq(0, 28, by = 4)) +
  theme_minimal() +
  geom_hline(yintercept = 0, color = "red") +
  geom_vline(xintercept = 20, color = "green") +
  geom_vline(xintercept = 22.5, color = "green")
  
GameTimeTweets <- SummarisedTweets %>% filter(Time >= 20 & Time <= 22.5)

ggplot(GameTimeTweets, aes(x = Time, y = Sentiment)) + geom_line() +
  labs(title = "Sentiment Analysis of the Houston Rockets vs Brooklyn Nets Game", x = "Time of Day(Hours)", y = "Average Sentiment") +
  expand_limits(y = c(-.8, .8)) +
  theme_minimal() +
  geom_hline(yintercept = 0, color = "red")