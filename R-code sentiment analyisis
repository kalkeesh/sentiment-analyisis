library(dplyr)
library(syuzhet)
library(sentimentr)
library(ggplot2)
library(wordcloud2)
library(tm)
library(readxl)
review <- read_excel("F:\\review.xlsx")
View(review)

#syuzhet
review<-as.character(review)
s1<-get_nrc_sentiment(review)
View(review)
s1
review_sentiment<-cbind(review,s1)
barplot(colSums(s1),col=rainbow(10),ylab="count",main="amazon_feedback")

#sentimetr
sentiment(review)
senticont<-review %>% 
  get_sentences() %>% 
  sentiment()

polarity<-senticont %>% 
  mutate(polarity_level=ifelse(sentiment>0,"positive","negative")) %>% 
  count(polarity_level)
polarity
v<-c(1,2)
cbind(polarity,v)
summary(polarity)
polarity %>%
  ggplot()+geom_col(aes(x=n,y=polarity_level,fill=v))+coord_flip()




#creating word clouds 
#word cloud for amazon mobile reviews
review.corpus=Corpus(VectorSource(review))
review.corpus=review.corpus %>% 
  tm_map(removeNumbers) %>% 
  tm_map(removePunctuation) %>% 
  tm_map(stripWhitespace) %>% 
  tm_map(content_transformer(tolower)) %>% 
  tm_map(removeWords,stopwords("english")) %>% 
  tm_map(removeWords,stopwords("SMART"))

tdm = TermDocumentMatrix(review.corpus) %>% 
  as.matrix()
View(tdm)

words=sort(rowSums(tdm),decreasing = TRUE)
df=data.frame(word=names(words),freq=words)
df=df %>%
  filter(nchar(as.character(word))>2,
         word!="don'")
dfe<-iconv(df$word,to="utf-8")
dfy<-data.frame(dfe,freq=words)
wordcloud2(dfy)

uxc.colors=c("#91ebfa","#50bee1","#2596be","#ffba3d")
uxc.background = "#111002"
wordcloud2(dfy,
           color = rep_len(uxc.colors, nrow(dfy)),
           backgroundColor = uxc.background)
