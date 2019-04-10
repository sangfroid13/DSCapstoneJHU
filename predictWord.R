directory<-"C:/Users/hp/Desktop/Coursera/Capstone/final/en_US/ngram_data"
setwd(directory)

library(dplyr)
library(stringr)

bigramFile<-read.csv("graphB.csv", header = T)
trigramFile<-read.csv("graphT.csv", header = T)
quadgramFile<-read.csv("graphQ.csv", header = T)
pentagramFile<-read.csv("graphP.csv", header = T)
##r<-regexec('^.* ([[:alpha:]]+)$',bigramFile$word)
##m<-regmatches(bigramFile$word,r)
##bigramFile$nextWord<-sapply(m, function(x) x[2])

bigramFile$nextWord<-word(bigramFile$word,-1)
bigramFile$phrase<-word(bigramFile$word,1,-2)
save(bigramFile,file="bigram.Rdata")

##write.csv(bigramFile, "bigramFile.csv", row.names = F)

trigramFile$nextWord<-word(trigramFile$word,-1)
trigramFile$phrase<-word(trigramFile$word,1,-2)
save(trigramFile,file ="trigram.Rdata")
##write.csv(trigramFile, "trigramFile.csv", row.names = F)

quadgramFile$nextWord<-word(quadgramFile$word,-1)
quadgramFile$phrase<-word(quadgramFile$word,1,-2)
save(quadgramFile,file="quadgram.Rdata")
##write.csv(quadgramFile, "quadgramFile.csv", row.names = F)

pentagramFile$nextWord<-word(pentagramFile$word,-1)
pentagramFile$phrase<-word(pentagramFile$word,1,-2)
save(pentagramFile,file="pentagram.Rdata")
##write.csv(pentagramFile, "pentagramFile.csv", row.names = F)


predictNextWord<-function(string1){
  
  mystring<-as.character(string1)
  mystring<-trimws(mystring)
  mystring<-gsub("[^[:alpha:][:space:]']", "", mystring)
  mystring<-trimws(tolower(mystring))
  
  wordCount<-str_count(mystring,  '[\\w\']+')
  
  if (wordCount>4){
    mystring<-word(mystring,-4,-1)
    
  }
  wordCount<-str_count(mystring,  '[\\w\']+')
  
  if (wordCount==4){
    
    npos<-match(mystring,pentagramFile$phrase,nomatch = 0)
    if (npos==0){
      mystring<-word(mystring,-3,-1)
      npos<-match(mystring,quadgramFile$phrase,nomatch = 0)
      if (npos==0){
        mystring<-word(mystring,-2,-1)
        npos<-match(mystring,trigramFile$phrase,nomatch = 0)
        if (npos==0){
          mystring<-word(mystring,-1)
          npos<-match(mystring,bigramFile$phrase,nomatch = 0)
          if(npos==0){
                b<-"NA"
                }
          else {
                b<-bigramFile$nextWord[npos]
              }
          
        }
        else {b<-trigramFile$nextWord[npos]}
      }
      else {b<-quadgramFile$nextWord[npos]}
    }
    else {b<-pentagramFile$nextWord[npos]}
  }
  else if (wordCount==3){
    npos<-match(mystring,quadgramFile$phrase,nomatch = 0)
    if (npos==0){
      mystring<-word(mystring,-2,-1)
      npos<-match(mystring,trigramFile$phrase,nomatch = 0)
      if (npos==0){
        mystring<-word(mystring,-1)
        npos<-match(mystring,bigramFile$phrase,nomatch = 0)
        if(npos==0){b<-"NA"}
        else {b<-bigramFile$nextWord[npos]}
        
      }
      else {b<-trigramFile$nextWord[npos]}
    }
    else {b<-quadgramFile$nextWord[npos]}
    
  }
  else if (wordCount==2){
    npos<-match(mystring,trigramFile$phrase,nomatch = 0)
 
      if (npos==0){
        mystring<-word(mystring,-1)
        npos<-match(mystring,bigramFile$phrase,nomatch = 0)
        if(npos==0){b<-"NA"}
        else {b<-bigramFile$nextWord[npos]}
        
      }
      else {b<-trigramFile$nextWord[npos]}

    
  }
  else if (wordCount==1){
  npos<-match(mystring,bigramFile$phrase,nomatch = 0)
  if(npos==0){b<-"NA"}
  else {b<-bigramFile$nextWord[npos]}
  
  }
  else {
    b<-"NA"
    
  }
  b
}

