#rm(list=ls())



library("stringr")
library("tm")
library("SnowballC")
library("wordcloud")

###########################################







fName <- 'news20/20_newsgroup/'
b <-list.files(path='news20/20_newsgroup/')
lendir=length(b)


#cargar datos si no estan ya en el entorno

if (!exists("msgC")){
  msgC=matrix(0,19997,3)
  c=0
  for (j in 1:lendir){
    a<-list.files(path=paste("./news20/20_newsgroup/",b[j],sep=""))
    len=length(a)
    for (i in 1:len){
      
      nombre=paste(fName,b[j],sep="")
      nombre=paste(nombre,'/',sep="")
      nombre=paste(nombre,a[i],sep="")
      # Situarse en "../news20/20_newsgroup/comp.graphics"
      mensaje <- readLines(nombre)
      blankLine <- ""
      indice <- match("", mensaje)
      # ahora tenemos un vector de caracteres (cada componente es una línea) solo 
      # con el cuerpo del mensaje original
      mensaje <- mensaje[indice:length(mensaje)]
      # Nuevo mensaje cuerpo ya sin lineas
      mensaje <- paste(mensaje, collapse= "")
      msgC[i+c,1]=mensaje
      msgC[i+c,3]=b[j]
      msgC[i+c,2]=a[i]
      
    }
    c=c+len
  }
}


#############################################################################################

#cargado de funciones de representacion



source("histograma.R")
source("wordCloud.R")

# plot

#histograma(msgC,b[2],b,6)
wordCloud(msgC,b[2],b,50)


stop("fin")

#crear muestra aleatoria
index=sample(1:dim(msgC)[1],2500)

muestra <-msgC[index,]
fich=muestra[,2]
dir=muestra[,3]

#create the corpus structure

palCorpus=Corpus(VectorSource(muestra[,1]))

#filter out unwanted terms

palCorpus=tm_map(palCorpus, content_transformer(tolower))
palCorpus <- tm_map(palCorpus, removePunctuation)
palCorpus <- tm_map(palCorpus,removeNumbers)
palCorpus <- tm_map(palCorpus, removeWords, stopwords('english'))
palCorpus <- tm_map(palCorpus, stemDocument)

#matriz de terminos-documentos

dtm <- TermDocumentMatrix(palCorpus)
tf<- as.matrix(dtm)
#idf
idf <- log( ncol(tf) / ( 1 + rowSums(tf != 0) ) )
#diag matrix
#( idf <- diag(idf) )
#tf-idf
#tf_idf <- crossprod(tf, idf)
#colnames(tf_idf) <- rownames(tf)
tf=t(tf)*idf
tf=tf / sqrt( rowSums( tf^2 ) )



#comparar (distancia en angulo)

source("distancia.R")

#comparar dos ficheros -> seleccionar dos filas y aplicar "distancia"

#disFich=distancia(tf[row1,],tf[row2,])

#comparar dos terminos -> ¿? distancia euclídea ¿?

#disTerm=distancia(tf[,col1],tf[,col2])

