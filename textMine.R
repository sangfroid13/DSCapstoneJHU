directory<-"C:/Users/hp/Desktop/Coursera/Capstone/final/en_US"
setwd(directory)

library(dplyr)
library(quanteda)
library(data.table)

##reading data
con3 <- file("en_US.twitter.txt", encoding = "UTF-8") 
twitterData<- readLines(con3)
close(con3)

con2 <- file("en_US.news.txt", encoding = "UTF-8") 
newsData<- readLines(con2)
close(con2)

con1 <- file("en_US.blogs.txt", encoding = "UTF-8") 
blogsData<- readLines(con1)
close(con1)

## Generating a random sapmle of all sources
length(twitterData)
length(newsData)
length(blogsData)


sampleTwitter <- sample(twitterData, 50000)
sampleNews <- sample(newsData, 30000)
sampleBlogs <- sample(blogsData, 50000)
sampleData <- c(sampleTwitter,sampleNews,sampleBlogs)

rm(blogsData)
rm(twitterData)
rm(newsData)
rm(sampleTwitter)
rm(sampleNews)
rm(sampleBlogs)



doc.corpus <- corpus(sampleData)
doc.tokens <- tokens(doc.corpus)
##summary(doc.corpus)

doc.tokens <- tokens(doc.tokens, 
                     remove_punct = TRUE, 
                     remove_numbers = TRUE, 
                     remove_hyphens = TRUE,

                     remove_symbols = TRUE,
                     remove_url = TRUE)


doc.tokens2 <- tokens_select(doc.tokens, stopwords('english'),selection='remove')
doc.tokens2 <- tokens_wordstem(doc.tokens2)

doc.tokens <- tokens_tolower(doc.tokens)
doc.tokens2 <- tokens_tolower(doc.tokens2)

##summary(doc.tokens)



unigram <- dfm(doc.tokens)
unigram2 <- dfm(doc.tokens2)

colsumU<-colSums(unigram)
colsumU2<-colSums(unigram2)

table_unigram<-data.table(word=names(colsumU),count=colsumU)
table_unigram2<-data.table(word=names(colsumU2),count=colsumU2)

library(ggplot2)
library(gridExtra)

graphU<-arrange(table_unigram, desc(count))
graphU2<-arrange(table_unigram2, desc(count))

g1<- ggplot(data=graphU[1:20,], aes(x=word, y=count)) + geom_bar(stat="identity") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
g2<- ggplot(data=graphU2[1:20,], aes(x=word, y=count)) + geom_bar(stat="identity") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  ggtitle("Stemmed and stopwords removed")

grid.arrange(
  g1, g2,
  nrow = 2,
  top = "Most used words"
  
)
ggsave("plot1.png", width = 8, height = 8, units = "in")
write.csv(graphU2,"graphU2.csv", row.names = FALSE)

rm(list=ls())
gc(reset=TRUE)

directory<-"C:/Users/hp/Desktop/Coursera/Capstone/final/en_US"
setwd(directory)

library(dplyr)
library(quanteda)
library(data.table)

##reading data
con3 <- file("en_US.twitter.txt", encoding = "UTF-8") 
twitterData<- readLines(con3)
close(con3)

con2 <- file("en_US.news.txt", encoding = "UTF-8") 
newsData<- readLines(con2)
close(con2)

con1 <- file("en_US.blogs.txt", encoding = "UTF-8") 
blogsData<- readLines(con1)
close(con1)

## Generating a random sapmle of all sources
length(twitterData)
length(newsData)
length(blogsData)


sampleTwitter <- sample(twitterData, 50000)
sampleNews <- sample(newsData, 30000)
sampleBlogs <- sample(blogsData, 50000)
sampleData <- c(sampleTwitter,sampleNews,sampleBlogs)

rm(blogsData)
rm(twitterData)
rm(newsData)
rm(sampleTwitter)
rm(sampleNews)
rm(sampleBlogs)



doc.corpus <- corpus(sampleData)
doc.tokens <- tokens(doc.corpus)
##summary(doc.corpus)

doc.tokens <- tokens(doc.tokens, 
                     remove_punct = TRUE, 
                     remove_numbers = TRUE, 
                     remove_hyphens = TRUE,

                     remove_symbols = TRUE,
                     remove_url = TRUE)


doc.tokens <- tokens_tolower(doc.tokens)



bigram<-tokens_ngrams(doc.tokens, n = 2, concatenator = " ")
bigram<-dfm(bigram)
bigram<-dfm_trim(bigram, min_termfreq = 4, min_docfreq = 0.05)
colsumB<-colSums(bigram)
table_bigram<-data.table(word=names(colsumB),count=colsumB)
library(ggplot2)
graphB<-arrange(table_bigram, desc(count))
g3<- ggplot(data=graphB[1:20,], aes(x=word, y=count)) + geom_bar(stat="identity") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
g3
ggsave("plot3.png", width = 8, height = 8, units = "in")
write.csv(graphB,"graphB.csv", row.names = FALSE)

rm(list=ls())
gc(reset=TRUE)

directory<-"C:/Users/hp/Desktop/Coursera/Capstone/final/en_US"
setwd(directory)

library(dplyr)
library(quanteda)
library(data.table)

##reading data
con3 <- file("en_US.twitter.txt", encoding = "UTF-8") 
twitterData<- readLines(con3)
close(con3)

con2 <- file("en_US.news.txt", encoding = "UTF-8") 
newsData<- readLines(con2)
close(con2)

con1 <- file("en_US.blogs.txt", encoding = "UTF-8") 
blogsData<- readLines(con1)
close(con1)

## Generating a random sapmle of all sources
length(twitterData)
length(newsData)
length(blogsData)


sampleTwitter <- sample(twitterData, 50000)
sampleNews <- sample(newsData, 30000)
sampleBlogs <- sample(blogsData, 50000)
sampleData <- c(sampleTwitter,sampleNews,sampleBlogs)

rm(blogsData)
rm(twitterData)
rm(newsData)
rm(sampleTwitter)
rm(sampleNews)
rm(sampleBlogs)



doc.corpus <- corpus(sampleData)
doc.tokens <- tokens(doc.corpus)
##summary(doc.corpus)

doc.tokens <- tokens(doc.tokens, 
                     remove_punct = TRUE, 
                     remove_numbers = TRUE, 
                     remove_hyphens = TRUE,

                     remove_symbols = TRUE,
                     remove_url = TRUE)


doc.tokens <- tokens_tolower(doc.tokens)

trigram<- tokens_ngrams(doc.tokens, n = 3, concatenator = " ")
trigram<-dfm(trigram)
trigram<-dfm_trim(trigram, min_termfreq = 4, min_docfreq = 0.05)
colsumT<-colSums(trigram)
table_trigram<-data.table(word=names(colsumT),count=colsumT)
library(ggplot2)
graphT<-arrange(table_trigram, desc(count))
g4<- ggplot(data=graphT[1:20,], aes(x=word, y=count)) + geom_bar(stat="identity") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
g4
ggsave("plot4.png", width = 8, height = 8, units = "in")
write.csv(graphT,"graphT.csv", row.names = FALSE)

rm(list=ls())
gc(reset=TRUE)

directory<-"C:/Users/hp/Desktop/Coursera/Capstone/final/en_US"
setwd(directory)

library(dplyr)
library(quanteda)
library(data.table)

##reading data
con3 <- file("en_US.twitter.txt", encoding = "UTF-8") 
twitterData<- readLines(con3)
close(con3)

con2 <- file("en_US.news.txt", encoding = "UTF-8") 
newsData<- readLines(con2)
close(con2)

con1 <- file("en_US.blogs.txt", encoding = "UTF-8") 
blogsData<- readLines(con1)
close(con1)

## Generating a random sapmle of all sources
length(twitterData)
length(newsData)
length(blogsData)


sampleTwitter <- sample(twitterData, 50000)
sampleNews <- sample(newsData, 30000)
sampleBlogs <- sample(blogsData, 50000)
sampleData <- c(sampleTwitter,sampleNews,sampleBlogs)

rm(blogsData)
rm(twitterData)
rm(newsData)
rm(sampleTwitter)
rm(sampleNews)
rm(sampleBlogs)



doc.corpus <- corpus(sampleData)
doc.tokens <- tokens(doc.corpus)
##summary(doc.corpus)

doc.tokens <- tokens(doc.tokens, 
                     remove_punct = TRUE, 
                     remove_numbers = TRUE, 
                     remove_hyphens = TRUE,

                     remove_symbols = TRUE,
                     remove_url = TRUE)


doc.tokens <- tokens_tolower(doc.tokens)
quadgram<-tokens_ngrams(doc.tokens, n = 4, concatenator = " ")
quadgram<-dfm(quadgram)
quadgram<-dfm_trim(quadgram, min_termfreq = 4, min_docfreq = 0.05)
colsumQ<-colSums(quadgram)
table_quadgram<-data.table(word=names(colsumQ),count=colsumQ)
library(ggplot2)
graphQ<-arrange(table_quadgram, desc(count))
g5<- ggplot(data=graphQ[1:20,], aes(x=word, y=count)) + geom_bar(stat="identity") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
g5
ggsave("plot5.png", width = 8, height = 8, units = "in")
write.csv(graphQ,"graphQ.csv", row.names = FALSE)

rm(list=ls())
gc(reset=TRUE)

directory<-"C:/Users/hp/Desktop/Coursera/Capstone/final/en_US"
setwd(directory)

library(dplyr)
library(quanteda)
library(data.table)

##reading data
con3 <- file("en_US.twitter.txt", encoding = "UTF-8") 
twitterData<- readLines(con3)
close(con3)

con2 <- file("en_US.news.txt", encoding = "UTF-8") 
newsData<- readLines(con2)
close(con2)

con1 <- file("en_US.blogs.txt", encoding = "UTF-8") 
blogsData<- readLines(con1)
close(con1)

## Generating a random sapmle of all sources
length(twitterData)
length(newsData)
length(blogsData)


sampleTwitter <- sample(twitterData, 50000)
sampleNews <- sample(newsData, 30000)
sampleBlogs <- sample(blogsData, 50000)
sampleData <- c(sampleTwitter,sampleNews,sampleBlogs)

rm(blogsData)
rm(twitterData)
rm(newsData)
rm(sampleTwitter)
rm(sampleNews)
rm(sampleBlogs)


doc.corpus <- corpus(sampleData)
doc.tokens <- tokens(doc.corpus)
##summary(doc.corpus)

doc.tokens <- tokens(doc.tokens, 
                     remove_punct = TRUE, 
                     remove_numbers = TRUE, 
                     remove_hyphens = TRUE,

                     remove_symbols = TRUE,
                     remove_url = TRUE)


doc.tokens <- tokens_tolower(doc.tokens)

pentagram<- tokens_ngrams(doc.tokens, n = 5, concatenator = " ")
pentagram<-dfm(pentagram)
pentagram<-dfm_trim(pentagram, min_termfreq = 4, min_docfreq = 0.05)
colsumP<-colSums(pentagram)
table_pentagram<-data.table(word=names(colsumP),count=colsumP)
library(ggplot2)
graphP<-arrange(table_pentagram, desc(count))
g6<- ggplot(data=graphP[1:20,], aes(x=word, y=count)) + geom_bar(stat="identity") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
g6
ggsave("plot6.png", width = 8, height = 8, units = "in")
write.csv(graphP,"graphP.csv", row.names = FALSE)

