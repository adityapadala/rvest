
library(dplyr)
a = read_html('https://www.yelp.com/biz/laz-fly-economy-parking-windsor-locks-3')
a1 = html_nodes(x = a, css = ".rating-qualifier")
a2 = html_text(a1)
a3 = c()
for (i in  0:length(a2))
{
  a3[i] =  a2[i]
}
  
a3 <- unlist(lapply(a3,function(x) gsub("^\\s+|\\s+$", "",x)))
a3 <- a3[-c(1,23,24,25)]
a3 <- trimws(gsub("\n|Updated|review|Previous","",a3))
  
b = read_html('https://www.yelp.com/biz/laz-fly-economy-parking-windsor-locks-3?start=20')
b1 = html_nodes(x = b, css = ".rating-qualifier")
b2 = html_text(b1)
b3 = c()
for (i in  0:length(b2))
{
  b3[i] =  b2[i]
}
  
b3 <- unlist(lapply(b3,function(x) gsub("^\\s+|\\s+$", "",x)))
b3 <- b3[-c(1,8,9,10)]
b3 <- trimws(gsub("\n|Updated|review|Previous","",b3))

c3 <- c(a3,b3)
c3 <- c3[-c(21)]
  
  
#Text Cleaning
laz_yelp_data <- unlist(lapply(laz_yelp_data,function(x) gsub("^\\s+|\\s+$", "",x)))
laz_yelp_data <- unlist(lapply(laz_yelp_data,function(x) gsub("[^0-9A-Za-z@///' ]", "",x)))
laz_yelp_data <- unlist(lapply(laz_yelp_data,function(x) gsub("[[:digit:]]", "",x)))
laz_yelp_data <- unlist(lapply(laz_yelp_data,function(x) gsub("[[:punct:]]", "",x)))

laz_df <- data.frame(laz_yelp_data)
laz_df[] <- lapply(laz_df, as.character)
laz_df <- subset(laz_df, !laz_yelp_data == "Was this review")
laz_df <- as.data.frame(laz_df[!duplicated(laz_df),])
colnames(laz_df) <- "laz_yelp"
laz_df <- data.frame(laz_df[-c(22,23),])
laz_df <- as.data.frame(laz_df[-c(2),])
View(laz_df)
laz_df$date <- c3

str(laz_df)
laz_df$date <- as.Date(laz_df$date,"%m/%d/%Y")


laz_corpus <- Corpus(VectorSource(laz_df$laz_yelp))
laz_corpus<- tm_map(laz_corpus, tolower)
laz_corpus <- tm_map(laz_corpus, PlainTextDocument)
laz_corpus <- tm_map(laz_corpus, stripWhitespace)
laz_corpus <- tm_map(laz_corpus, removePunctuation)
laz_corpus <- tm_map(laz_corpus, removeWords, stopwords("english"))

polarity_score = function(tweets, positive, negative)
{
  score =laply(tweets, function(tweet, positive, negative)
  {
    words = str_split(tweet,"\\s+")
    words = unlist(words)
    positive_overlap = match(words, positive) 
    negative_overlap= match(words, negative) 
    positive_overlap =!is.na(positive_overlap) 
    negative_overlap= !is.na(negative_overlap)
    score =sum(positive_overlap) - sum(negative_overlap)
    return(score)
  }, positive, negative)
  scores =data.frame(score=score, text=tweets)
  return(scores)
}

positive <- scan('positive-words.txt', what='character', comment.char=';')
negative <- scan('negative-words.txt', what='character', comment.char=';') 

laz_corpus_df=data.frame(text = sapply(laz_corpus, as.character), stringsAsFactors = FALSE)
Sentiment_scores <- polarity_score(laz_corpus_df$text, positive, negative)
laz_score <- data.frame(cbind(Sentiment_scores$score,laz_df))
View(laz_score)

laz_score$month <- month(laz_score$date)
laz_trend <- laz_score%>%group_by(month)%>%summarise(mean = mean(Sentiment_scores.score))

ggplot(data = laz_trend, aes(x=month,y=mean)) + 
  geom_line() +
  scale_x_continuous(name = "Months", breaks = seq(2, 12, 1)) +
  scale_y_continuous(name = "Average Sentiment Score", breaks = seq(-2.5, 2, 0.5)) +
  geom_hline(yintercept = 0, size = 1, colour = "#FF3721", linetype = "dashed") +
  ggtitle("Sentiment Monthly Trend")









               