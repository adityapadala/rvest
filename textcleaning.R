
setwd('path/webscraping')
library("stringr")
library("tm")
library("wordcloud")
library("plyr")
library("ggplot2")

laz_data <- c(laz_yelp_data,laz_groupon_data)

#Text Cleaning
laz_data <- unlist(lapply(laz_data,function(x) gsub("^\\s+|\\s+$", "",x)))
laz_data <- unlist(lapply(laz_data,function(x) gsub("[^0-9A-Za-z@///' ]", "",x)))
laz_data <- unlist(lapply(laz_data,function(x) gsub("[[:digit:]]", "",x)))
laz_data <- unlist(lapply(laz_data,function(x) gsub("[[:punct:]]", "",x)))

laz_df <- data.frame(laz_data)
laz_df[] <- lapply(laz_df, as.character)
laz_df <- subset(laz_df, !laz_data == "Was this review ")
laz_df <- as.data.frame(laz_df[!duplicated(laz_df),])
colnames(laz_df) <- "laz_data"
View(laz_df)

laz_corpus <- Corpus(VectorSource(laz_df$laz_data))
laz_corpus<- tm_map(laz_corpus, tolower)
laz_corpus <- tm_map(laz_corpus, PlainTextDocument)
laz_corpus <- tm_map(laz_corpus, stripWhitespace)
laz_corpus <- tm_map(laz_corpus, removePunctuation)
laz_corpus <- tm_map(laz_corpus, removeWords, stopwords("english"))

View(data.frame(text=unlist(sapply(laz_corpus, `[`, "content")), stringsAsFactors=F))

#building frequency table:
laz_dtm <- TermDocumentMatrix(laz_corpus)
m <- as.matrix(laz_dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d,10)

#creating word cloud using corpus and frequency table
#wordcloud(laz_corpus, scale=c(5,0.5), random.order=FALSE, rot.per=0.35, max.words = 300,use.r.layout=FALSE, colors=brewer.pal(8,"Dark2"))

set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1, random.order=FALSE, rot.per=0.35, 
          max.words = 200,colors=brewer.pal(8, "Dark2"))

#terms associations
findAssocs(laz_dtm, terms = "laz", corlimit = 0.4)

#term frequency plot
barplot(d[1:25,]$freq, las = 2, names.arg = d[1:25,]$word,
        col ="lightblue", main ="Most frequent words",
        ylab = "Word frequencies")

#Sentiment Analysis using lexicon based method
  
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
laz_score<-laz_score[-c(2),]
View(laz_score)
write.csv(laz_score,"laz_score.csv")

#hist(as.numeric(as.character(laz_score$Sentiment_scores.score)), col="orange", 
#     breaks = seq(-8,6,1),
#     main = "Histogram of sentiment polarity",
#      xlab = "Scores", ylab = "Freq")

m<-ggplot(data = laz_score, aes(x=Sentiment_scores.score))
m+geom_histogram(aes(fill = ..count..),binwidth = 1,colour = "darkgreen") +
  scale_x_continuous(name = "Sentiment Scores", breaks = seq(-8, 8, 1)) +
  geom_vline(xintercept = -0.5, size = 1, colour = "#FF3721", 
             linetype = "dashed") +
  ggtitle("Frequency histogram of Sentiment Score")+
  theme_bw()


laz_score$cat <- ifelse(as.numeric(as.character(laz_score$Sentiment_scores.score)) < 0, "Negative", "Positive")
laz_score[laz_score$Sentiment_scores.score == 0,]$cat <- "Neutral"





