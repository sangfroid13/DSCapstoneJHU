library(stringr)

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
