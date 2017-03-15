histograma <-function(dataset,label,names,K){
  
  
  
  #check if label is correct
  
  if(!(label %in% names)){
    
    stop("Label not in names, no results shown\n")
  }
  
  #select the subset of data
  
  datos=dataset[which(dataset[,3]==label),1]
  
  #create the corpus structure
  
  palCorpus=Corpus(VectorSource(datos))
  
  #filter out unwanted terms
  
  palCorpus=tm_map(palCorpus, content_transformer(tolower))
  palCorpus <- tm_map(palCorpus, removePunctuation)
  palCorpus <- tm_map(palCorpus, removeWords, stopwords('english'))
  palCorpus <- tm_map(palCorpus, stemDocument)
  
  #transformation into easy-to-handle structures
  
  dtm <- DocumentTermMatrix(palCorpus)
  m <- as.matrix(dtm)
  v <- sort(colSums(m),decreasing=TRUE)
  d <- data.frame(word = names(v),freq=v)
  
  #plot the histogram
  
  barplot(d[1:10,]$freq, las = 2, names.arg = d[1:10,]$word,
          col ="lightblue", main ="Most frequent words",
          ylab = "Word frequencies")
  
 
}