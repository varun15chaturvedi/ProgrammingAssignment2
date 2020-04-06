operation<-function(){
  library(rvest)
  library(selectr)
  library(httr)
  library(XML)
  library(stringi)
  
  hlink<- "http://fast.wistia.net/embed/iframe/"
  links<- read.csv("Time_Series_Analysis.csv",stringsAsFactors = F,header = F)
  link <- vector("character",length = 0)
  vname <- vector("character",length = 0)
  
  for (i in links[,2]){
    
    link_list<- strsplit(i,split = " ")
    link_meta<- link_list[[1]][2]
    link_meta<-substr(link_meta,nchar(link_meta)-14,nchar(link_meta)-5)
    link_meta<- paste(hlink,link_meta,sep = "")
    temp_data<-htmlTreeParse(link_meta,useInternalNodes = T)
    root<- xmlRoot(temp_data)
    a<-xpathSApply(root,"//script",xmlValue)
    b<- substring(a[5],regexpr('http:',a[5]),regexpr('.bin',a[5]) )
    b<- paste(b,'bin',sep = '')
    link<-c(link,b)
  }
  vname<- links[,1]
  write.csv(data.frame(vname,link),"Time_Series_Analysis_link.csv",row.names = F)
}
