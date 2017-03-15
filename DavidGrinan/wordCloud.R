wordCloud <-function(dataset,label,names,K){
  
  #check if label is correct
  
  
  if(!(label %in% names)){
    
    stop("Label not in names, no results shown\n")
  }
  
  #select the subset of data
  
  datos=dataset[which(dataset[,3]==label),1]
  
  #create the corpus structure
  
    palCorpus=Corpus(VectorSource(datos))
    
    #filter out unwanted terms
    
    palCorpus <- tm_map(palCorpus, content_transformer(tolower))
    palCorpus <- tm_map(palCorpus, removePunctuation)
    palCorpus <- tm_map(palCorpus, removeWords, stopwords('english'))
    palCorpus <- tm_map(palCorpus, stemDocument)
    
    #plot the wordCloud
    pal=brewer.pal(8,"Dark2")
    wordcloud(palCorpus, max.words = K, random.order = FALSE,colors=pal)
  
}