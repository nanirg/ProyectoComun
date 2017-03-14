rm(list=ls())
library("stringr")
fName <- 'news20/20_newsgroup/'
#file_Contents <- readChar(fName,file.info(fName)$size)
#a<-list.files(path="./news20/20_newsgroup/alt.atheism")
b <-list.files(path='news20/20_newsgroup/')
lendir=length(b)
msg=matrix(0,1000,lendir,2)

for (j in 1:lendir){
  a<-list.files(path=paste("./news20/20_newsgroup/",b[j],sep=""))
  len=length(a)
for (i in 1:len){
  nombre=paste(fName,b[j],sep="")
  nombre=paste(nombre,'/',sep="")
  nombre=paste(nombre,a[i],sep="")
  msg[i,j,1]=readChar(nombre,file.info(nombre)$size)
  msg[i,j,2]=b[j]
  
}

}
